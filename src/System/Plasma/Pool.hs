{-|
Module      : System.Plasma.Pool
Description : Functions from pool.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

A process deposits Proteins into, and retrieves Proteins from,
network-soluble ring buffers called “Pools”.  Multiple processes can
be simultaneously connected (via “Hoses”) to a single pool, with each
both depositing and retrieving Proteins.  The ordering of Proteins
stored in a Pool is monotonic and immutable, such that all retrieving
processes observe the same sequence.  Processes most typically read
from Pools in something like real time, with Proteins being retrieved
immediately after being deposited; but Pools are also “rewindable” so
that, for example, a new process joining a distributed system might
attach to a Pool already in use and begin reading Proteins from a time
far enough in the past to be able to reconstruct system context.
-}

module System.Plasma.Pool
  ( -- * Pool names
    PoolName(..)
  , toPoolName
  , fromPoolName
  , (+/)
    -- ** Parsing pool names
  , ParsedPoolUri(..)
  , PoolLocation(..)
  , PoolAuthority(..)
  , parsePoolUri
  , makePoolUri
    -- ** Checking validity of pool names
  , isPoolUriValid
  , isParsedPoolUriValid
  , isPoolPathValid
  , isPoolHostValid
    -- ** Constants for “scheme”
  , kLocal
  , kTcp
  , kTcpo
  , kTcps
    -- * Pool contexts
  , Context            -- opaque
  , ContextOptions(..)
  , newContext
  , getContextOptions
  , loadCredentialsFromFile
    -- * Pool creation options
  , PoolCreateOptions(..)
  , StrOrInt(..)
  , kMmap
    -- ** Standard pool sizes
  , small
  , medium
  , large
  , huge
    -- * Operations on pools
  , participate
  , participateCreatingly
  , create
  , dispose
  , rename
  , listPools
  , doesPoolExist
  , isPoolInUse
  , putPoolToSleep
    -- ** Convenient resource management
  , withHose
  , withHoseCreatingly
  , withTemporaryPool
    -- * Pool hoses
  , Hose               -- opaque
  , PoolIndex
  , RetProtein(..)
  , PoolTimestamp
  , TimeComparison(..)
  , PoolTimeout(..)
  , PoolInfo(..)
    -- ** Hose information
  , hoseName
  , hosePool
  , getHoseContext
  , getInfo
    -- ** Hose operations
  , withdraw
  , cloneHose
  , deposit
  , advanceOldest
  , changeOptions
  , enableWakeup
  , wakeUp
    -- *** Reading proteins
  , nthProtein
  , next
  , awaitNext
  , curr
  , prev
  , probeFrwd
  , awaitProbeFrwd
  , probeBack
    -- **** Fetch
  , FetchOp(..)
  , FetchResult(..)
  , fetch
  , fetch'
    -- *** Getting position
  , newestIndex
  , oldestIndex
  , currIndex
    -- *** Setting position
  , rewind
  , toLast
  , runout
  , frwdBy
  , backBy
  , seekTo
  , seekToTime
  , seekByTime
    -- * Pool gangs
    --
    -- | A t'Gang' is a collection of t'Hose's.  A t'Hose' can only belong
    -- to one t'Gang' at a time.  It is possible to “await” on a t'Gang',
    -- which “awaits” on all t'Hose's in the t'Gang' simultaneously,
    -- similar to the C @select()@ function.
  , Gang               -- opaque
  , newGang
  , gangName
  , newGangWithMembers
    -- ** Gang membership
  , getGangMembers
  , joinGang
  , addGangMembers
  , leaveGang
  , removeGangMembers
  , clearGang
  , withdrawAll
    -- ** Gang operations
  , nextMulti
  , awaitNextMulti
  , awaitMulti
  , wakeGang
  ) where

import Control.Exception
import Data.Default.Class
import Data.Int
import qualified Data.Text                     as T
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

import Data.Slaw
import Data.Slaw.IO.Internal.Options
import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal
import System.Loam.Retorts
import System.Loam.Retorts.Constants
import System.Loam.Util
import System.Plasma.Pool.Internal.FetchOp
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolGang
import System.Plasma.Pool.Internal.PoolHose
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolOpts
import System.Plasma.Pool.Internal.PoolTimeout

type PoolNameFunc = C.ConstCString -> Ptr FgnCtx -> IO Int64

foreign import capi safe "ze-hs-pool.h ze_hs_participate"
    c_participate :: CBool -> Ptr FgnCtx -> C.ConstCString -> C.ConstPtr FgnSlaw -> Ptr Int64 -> IO (Ptr FgnRawHose)

foreign import capi safe "ze-hs-pool.h ze_hs_create"
    c_create :: Ptr FgnCtx -> C.ConstCString -> C.ConstPtr FgnSlaw -> IO Int64

foreign import capi safe "libPlasma/c/pool.h pool_dispose_ctx"
    c_dispose_ctx :: C.ConstCString -> Ptr FgnCtx -> IO Int64

foreign import capi safe "libPlasma/c/pool.h pool_rename_ctx"
    c_rename_ctx :: C.ConstCString -> C.ConstCString -> Ptr FgnCtx -> IO Int64

foreign import capi safe "ze-hs-pool.h ze_hs_list"
    c_list :: Ptr FgnCtx -> C.ConstCString -> Ptr Int64 -> Ptr SlawLen -> IO (Ptr FgnSlaw)

foreign import capi safe "libPlasma/c/pool.h pool_exists_ctx"
    c_exists_ctx :: PoolNameFunc

foreign import capi safe "libPlasma/c/pool.h pool_sleep_ctx"
    c_sleep_ctx :: PoolNameFunc

foreign import capi safe "libPlasma/c/pool.h pool_check_in_use_ctx"
    c_check_in_use_ctx :: PoolNameFunc

-- | Open a new t'Hose' to a given pool.  It is an error
-- ('POOL_NO_SUCH_POOL') if the pool does not already exist.
--
-- In case of error, throws a t'PlasmaException' or an 'IOException'.
--
-- In case of success, returns the new hose.  The current index for
-- the hose will be set to the newest available protein in the pool.
--
-- When you are done with the hose, you should call 'withdraw' to
-- close it.  (Eventually, the hose would be closed during garbage
-- collection, but that is highly nondeterministic, so it is much
-- preferable to call 'withdraw' explicitly.)
participate
  :: HasCallStack
  => Context  -- ^ pool context
  -> T.Text   -- ^ name for new hose
  -> PoolName -- ^ name of pool to participate in
  -> IO Hose
participate ctx name pool = do
  let loc = "participate"
      cs  = callStack
  fst <$> participateInternal loc False cs ctx name pool SlawNil

-- | Open a new t'Hose' to a given pool, as in 'participate'.
-- However, if the pool does not exist, it is created.  The
-- specified options are used when creating the pool.
participateCreatingly
  :: (HasCallStack, ToSlaw a)
  => Context  -- ^ pool context
  -> T.Text   -- ^ name for new hose
  -> PoolName -- ^ name of pool to participate in
  -> a        -- ^ pool create options (usually t'PoolCreateOptions')
  -> IO (Hose, Bool) -- ^ boolean is true iff pool was created
participateCreatingly ctx name pool opts = do
  let loc = "participateCreatingly"
      cs  = callStack
  (h, tort) <- participateInternal loc True cs ctx name pool (š opts)
  return (h, tort == POOL_CREATED)

participateInternal
  :: String   -- ^ name of API function
  -> Bool     -- ^ creatingly?
  -> CallStack
  -> Context  -- ^ pool context
  -> T.Text   -- ^ name for new hose
  -> PoolName -- ^ name of pool to participate in
  -> Slaw     -- ^ pool create options
  -> IO (Hose, Retort)
participateInternal loc crtly cs ctx name pool opts = do
  initialize
  let cbool = fromBool crtly
      erl   = Just $ erlFromPoolName pool
      addn  = Just loc
  (hPtr, ret) <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    C.useAsConstCString (toByteString pool) $ \pnPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        withSlaw opts $ \optPtr -> do
          p <- c_participate cbool cPtr pnPtr optPtr tortPtr
          r <- peek tortPtr
          return (p, Retort r)
  h <- newHose loc cs name pool ctx hPtr
  return (h, ret)

-- | Like 'participate', but guarantees that the hose will be
-- closed (withdrawn) when the given IO action completes, regardless
-- of whether that is normally or by an exception being thrown.
withHose
  :: HasCallStack
  => Context         -- ^ pool context
  -> T.Text          -- ^ name for new hose
  -> PoolName        -- ^ name of pool to participate in
  -> (Hose -> IO a)  -- ^ action to run with hose
  -> IO a
withHose ctx name pool action =
  bracket (participate ctx name pool) withdraw action

-- | Like 'participateCreatingly', but guarantees that the hose will be
-- closed (withdrawn) when the given IO action completes, regardless
-- of whether that is normally or by an exception being thrown.
--
-- The callback is given the hose, and also a boolean, where
-- 'False' indicates the pool already existed, and 'True' means that
-- it was created.
withHoseCreatingly
  :: (HasCallStack, ToSlaw a)
  => Context        -- ^ pool context
  -> T.Text         -- ^ name for new hose
  -> PoolName       -- ^ name of pool to participate in
  -> a              -- ^ pool create options (usually t'PoolCreateOptions')
  -> ((Hose, Bool) -> IO b) -- ^ action to run with hose
  -> IO b
withHoseCreatingly ctx name pool opts action = do
  bracket
    (participateCreatingly ctx name pool opts)
    (withdraw . fst)
    action

-- | Creates the specified pool, using the specified options.
-- It is an error ('POOL_EXISTS') if the pool already exists.
--
-- In case of error, throws a t'PlasmaException' or an 'IOException'.
create
  :: (HasCallStack, ToSlaw a)
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool to create
  -> a        -- ^ pool create options (usually t'PoolCreateOptions')
  -> IO ()
create ctx pool opts = do
  initialize
  let cs    = callStack
      erl   = Just $ erlFromPoolName pool
      addn  = Just "create"
  C.useAsConstCString (toByteString pool) $ \pnPtr -> do
    withForeignPtr (ctxPtr ctx) $ \cPtr -> do
      withSlaw (š opts) $ \optPtr -> do
        tort <- c_create cPtr pnPtr optPtr
        throwRetortCS_ EtPools addn (Retort tort) erl cs

-- | Deletes the specified pool.  Possible errors include
-- 'POOL_NO_SUCH_POOL' if the pool does not exist, and
-- 'POOL_IN_USE' if there is still a hose open to this pool.
--
-- In case of error, throws a t'PlasmaException' or an 'IOException'.
dispose
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool to delete
  -> IO ()
dispose ctx pool = do
  initialize
  let cs    = callStack
      erl   = Just $ erlFromPoolName pool
      addn  = Just "dispose"
  C.useAsConstCString (toByteString pool) $ \pnPtr -> do
    withForeignPtr (ctxPtr ctx) $ \cPtr -> do
      tort <- c_dispose_ctx pnPtr cPtr
      throwRetortCS_ EtPools addn (Retort tort) erl cs

-- | Renames a pool.  It is an error ('POOL_IN_USE') to attempt
-- to rename a pool if there are any open hoses to it.
--
-- In case of error, throws a t'PlasmaException' or an 'IOException'.
rename
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool to rename
  -> PoolName -- ^ new name for pool
  -> IO ()
rename ctx oldName newName = do
  initialize
  let cs     = callStack
      erlOld = Just $ erlFromPoolName oldName
      erlNew = Just $ erlFromPoolName newName
      addn   = Just "rename"
  C.useAsConstCString (toByteString oldName) $ \oldPtr -> do
    C.useAsConstCString (toByteString newName) $ \newPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        tort <- Retort <$> c_rename_ctx oldPtr newPtr cPtr
        let erl = if tort == POOL_EXISTS then erlNew else erlOld
        throwRetortCS_ EtPools addn tort erl cs

-- | List all the pools under a specified URI.  If the URI is
-- 'Nothing', then lists all local pools under @OB_POOLS_DIR@.  A
-- subset of those pools, underneath a specified subdirectory of
-- @OB_POOLS_DIR@, can be requested with a URI of the form
-- @some\/dir@.  Pools underneath an arbitrary local directory can be
-- listed with @local:\/an\/absolute\/dir@.  The URI should be a
-- string like @tcp:\/\/chives.la923.example.net:1234\/@ if you want
-- to list pools on a remote server.
listPools
  :: HasCallStack
  => Context        -- ^ pool context
  -> Maybe PoolName -- ^ uri to list ('Nothing' for all local pools)
  -> IO [PoolName]
listPools ctx Nothing    = listPools0 callStack ctx def C.nullConstPtr
listPools ctx (Just uri) =
  C.useAsConstCString (toByteString uri) $ listPools0 cs ctx erl
  where erl = erlFromPoolName uri
        cs  = callStack

listPools0
  :: CallStack
  -> Context           -- ^ pool context
  -> ErrLocation
  -> C.ConstCString    -- ^ uri to list
  -> IO [PoolName]
listPools0 cs ctx erl uriPtr = do
  initialize
  let addn = Just "listPools"
  mSlaw <- withReturnedSlaw erl $ \lenPtr -> do
    withReturnedRetortCS EtPools addn (Just erl) cs $ \tortPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        c_list cPtr uriPtr tortPtr lenPtr
  case mSlaw >>= ŝm of
    Nothing    -> return []
    Just names -> return names

-- | Creates a uniquely-named, temporary pool which exists for the
-- duration of the specified IO action.  When the IO action returns
-- (whether normally or by raising an exception), the temporary
-- pool is deleted (via 'dispose').
withTemporaryPool
  :: (HasCallStack, ToSlaw a)
  => Context            -- ^ pool context
  -> Maybe PoolName     -- ^ optional directory/server to create pool in
  -> a                  -- ^ pool create options
  -> (PoolName -> IO b) -- ^ action to run with temporary pool
  -> IO b
withTemporaryPool ctx mPool opts action =
  bracket (makeTemporaryPool 10 ctx mPool (š opts)) (dispose ctx) action

makeTemporaryPool
  :: HasCallStack
  => Int
  -> Context
  -> Maybe PoolName
  -> Slaw
  -> IO PoolName
makeTemporaryPool 0 _ mPool _ = do
  let msg = "unable to create temporary pool"
  throwIO $ PlasmaException { peType      = EtPools
                            , peRetort    = Nothing
                            , peMessage   = msg
                            , peCallstack = Just callStack
                            , peLocation  = fmap erlFromPoolName mPool
                            }
makeTemporaryPool !tries ctx mPool opts = do
  uuid <- generateUuid
  let tmpName = toPoolName $ "tmp!" <> uuid
      pool    = case mPool of
                  Nothing  -> tmpName
                  Just pfx -> pfx +/ tmpName
  eth <- tryJust chkExc $ create ctx pool opts
  case eth of
    Left  _ -> makeTemporaryPool (tries - 1) ctx mPool opts
    Right _ -> return pool

chkExc :: PlasmaException -> Maybe ()
chkExc pe =
  case peRetort pe of
    Just POOL_EXISTS -> Just ()
    _                -> Nothing

nameOnlyOp
  :: CallStack
  -> String
  -> PoolNameFunc
  -> [(Retort, a)]
  -> a
  -> Context
  -> PoolName
  -> IO a
nameOnlyOp cs loc func pairs dflt ctx pool = do
  initialize
  let erl   = Just $ erlFromPoolName pool
      addn  = Just loc
  C.useAsConstCString (toByteString pool) $ \pnPtr -> do
    withForeignPtr (ctxPtr ctx) $ \cPtr -> do
      tort <- Retort <$> func pnPtr cPtr
      case tort `lookup` pairs of
        Just x  -> return x
        Nothing -> do
          throwRetortCS_ EtPools addn tort erl cs
          return dflt

-- | Returns 'True' if the specified pool exists, and returns 'False'
-- if the specified pool does not exist.  Throws t'PlasmaException' or
-- 'IOException' in case of error.
--
-- Beware of TOCTOU!  In most cases, it would be more robust to just
-- use 'participate', because then if the pool does exist, you'll have
-- a hose to it.  With 'doesPoolExist', the pool might go away between
-- now and when you participate in it.
doesPoolExist
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool
  -> IO Bool
doesPoolExist =
  nameOnlyOp callStack "doesPoolExist" c_exists_ctx pairs False
  where pairs = [(OB_YES, True)]

-- | If the named pool exists and there are currently no hoses open to it,
-- returns 'False'.  If the named pool currently has one or more hoses
-- open to it, returns 'True'.  Throws t'PlasmaException' or
-- 'IOException' in case of error.
--
-- Beware of TOCTOU issues, though:
-- <http://cwe.mitre.org/data/definitions/367.html>
isPoolInUse
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool
  -> IO Bool
isPoolInUse =
  nameOnlyOp callStack "isPoolInUse" c_check_in_use_ctx pairs False
  where pairs = [(POOL_IN_USE, True)]

-- | Put a pool “to sleep”, which means allowing the pool implementation
-- to free certain resources used by the pool, in the expectation that
-- it won't be used in a while.  A pool can only be put to sleep if
-- there are no open hoses to it; 'POOL_IN_USE' will be thrown if this
-- condition is not met.  The pool will automatically “wake up”
-- (reacquire the resources it needs) the next time it is participated
-- in.
--
-- In practice, in the current implementation, “resources” means
-- “semaphores”.  This function is only useful/necessary if you
-- intend to have a large number (more than 32768) of pools.
putPoolToSleep
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool
  -> IO ()
putPoolToSleep =
  nameOnlyOp callStack "putPoolToSleep" c_sleep_ctx [] ()
