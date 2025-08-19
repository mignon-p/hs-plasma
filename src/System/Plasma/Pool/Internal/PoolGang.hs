{-|
Module      : System.Plasma.Pool.Internal.PoolGang
Description : Gang type, for holding a collection of hoses
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.PoolGang
  ( Gang(..)
  , newGang
  , newGangWithMembers
  , getGangMembers
  , joinGang
  , addGangMembers
  , leaveGang
  , removeGangMembers
  , clearGang
  , withdrawAll
  , wakeGang
  , nextMulti
  , awaitNextMulti
  , awaitMulti
  ) where

import Control.Monad
import Control.DeepSeq
import Control.Exception
-- import Control.Monad
import Data.Bifunctor
-- import qualified Data.ByteString          as B
import Data.Default.Class
import Data.Hashable
import Data.Int
-- import Data.List
-- import Data.Ord
import Data.String
import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
-- import Data.Word
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
-- import Foreign.Marshal.Alloc
-- import Foreign.Marshal.Array
-- import Foreign.Marshal.Error
-- import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
-- import Foreign.Storable
-- import GHC.Generics (Generic)
import GHC.Stack
-- import System.IO.Unsafe

import Data.Slaw
-- import Data.Slaw.Util
import System.Loam.Hash
-- import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
-- import System.Loam.Retorts.Constants
-- import System.Loam.Time
import System.Plasma.Pool.Internal.Hoses
-- import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolHose
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolTimeout

foreign import capi safe "ze-hs-gang.h ze_hs_new_gang"
    c_new_gang :: StablePtr HosesRef -> Ptr Int64 -> IO (Ptr FgnGang)

foreign import capi unsafe "ze-hs-gang.h &ze_hs_finalize_gang"
    c_finalize_gang :: FunPtr (Ptr FgnGang -> IO ())

foreign import capi unsafe "ze-hs-gang.h ze_hs_get_gang_hoses"
    c_get_gang_hoses :: Ptr FgnGang -> IO (StablePtr HosesRef)

foreign import capi safe "ze-hs-gang.h ze_hs_gang_op"
    c_gang_op
      :: CChar              -- op
      -> Ptr FgnGang        -- zGang
      -> Ptr FgnHose        -- zHose
      -> IO Int64           -- retort

foreign import capi safe "ze-hs-gang.h ze_hs_gang_next_op"
    c_gang_next_op
      :: CChar                -- op
      -> Ptr FgnGang          -- zGang
      -> Double               -- timeout
      -> Ptr (Ptr FgnRawHose) -- hose_out
      -> Ptr PoolTimestamp    -- ts_out
      -> Ptr PoolIndex        -- index_out
      -> Ptr Int64            -- tort_out
      -> Ptr SlawLen          -- len_out
      -> IO (Ptr FgnSlaw)

foreign import capi safe "ze-hs-gang.h ze_hs_gang_misc_op"
    c_gang_misc_op
      :: CChar              -- op
      -> Ptr FgnGang        -- zGang
      -> Double             -- timeout
      -> IO Int64           -- retort

kGang :: IsString a => a
kGang = "Gang"

-- | A collection of hoses, for multi-pool await support.
--
-- A hose may only be a member of one gang at any given time.
data Gang = Gang
  { -- | The name of the gang that was given to 'newGang'.
    gangName  :: !T.Text
  , gangPtr   :: !(ForeignPtr FgnGang)
  } deriving (Eq, Ord)

instance NFData Gang where
  rnf x = gangName x `deepseq` (gangPtr x `seq` ())

instance Hashable Gang where
  hash                gang = hashInt $ fPtrToIntegral (gangPtr gang)
  salt `hashWithSalt` gang =
    salt `hash2xInt` fPtrToIntegral (gangPtr gang)

instance Show Gang where
  show gang = fmtForeignObj kGang (gangName gang) [] (gangPtr gang)

instance Nameable Gang where
  typeName _ = "Gang"

-- | Creates a new, empty t'Gang'.
newGang
  :: HasCallStack
  -- | Name of this Gang (only used in 'Show' instance).
  => T.Text
  -> IO Gang
newGang name0 = do
  initialize
  let cs   = callStack
      addn = Just "newGang"
  name     <- nonEmptyName kGang name0 cs
  hosesRef <- newHoses
  stabPtr  <- newStablePtr hosesRef
  zGang <- withReturnedRetortCS EtPools addn Nothing cs $ \tortPtr -> do
    c_new_gang stabPtr tortPtr
  fptr  <- newForeignPtr c_finalize_gang zGang
  return $ Gang { gangName = name
                , gangPtr  = fptr
                }

-- | Creates a new t'Gang' containing the specified hoses.
newGangWithMembers
  :: HasCallStack
  -- | Name of this Gang (only used in 'Show' instance).
  => T.Text
  -> [Hose]
  -> IO Gang
newGangWithMembers name hoses = do
  gang <- newGang name
  addGangMembers gang hoses
  return gang

-- | Returns all members of a t'Gang', in the order they were added.
getGangMembers :: Gang -> IO [Hose]
getGangMembers g = getHoses g >>= listHoses

-- | Add a t'Hose' to a t'Gang'.  Fails if this t'Hose' is already a
-- member of this t'Gang', or of any other t'Gang'.
joinGang
  :: HasCallStack
  => Gang
  -> Hose
  -> IO ()
joinGang gang hose = do
  let ei = mkErrInfo "joinGang" gang callStack
  gangOp ei 'j' gang hose
  hosesRef <- getHoses gang
  addToHoses ei hosesRef hose

-- | Like 'joinGang', but adds multiple hoses at once.
addGangMembers :: HasCallStack => Gang -> [Hose] -> IO ()
addGangMembers gang hoses = do
  let ei = mkErrInfo "addGangMembers" gang callStack
  hosesRef <- getHoses gang
  forM_ hoses $ \hose -> do
    gangOp ei 'j' gang hose
    addToHoses ei hosesRef hose

-- | Remove a t'Hose' from a t'Gang'.
leaveGang
  :: HasCallStack
  => Gang
  -> Hose
  -> IO ()
leaveGang gang hose = do
  let ei = mkErrInfo "leaveGang" gang callStack
  gangOp ei 'l' gang hose
  hosesRef <- getHoses gang
  removeFromHoses ei hosesRef hose

-- | Like 'leaveGang', but removes multiple hoses at once.
removeGangMembers :: HasCallStack => Gang -> [Hose] -> IO ()
removeGangMembers gang hoses = do
  let ei = mkErrInfo "leaveGang" gang callStack
  hosesRef <- getHoses gang
  forM_ hoses $ \hose -> do
    gangOp ei 'l' gang hose
    removeFromHoses ei hosesRef hose

-- | Remove all t'Hose's from a t'Gang'.  Similar to the C function
-- @pool_disband_gang (gang, false)@, except that the t'Gang' still
-- exists (but is empty) upon return.
clearGang
  :: HasCallStack
  => Gang
  -> IO ()
clearGang gang = do
  let ei = mkErrInfo "clearGang" gang callStack
  gangMiscOp ei 'd' gang def
  hosesRef <- getHoses gang
  clearHoses hosesRef

-- | Remove all t'Hose's from a t'Gang', and then performs a
-- 'withdraw' on each t'Hose'.  Similar to the C function
-- @pool_disband_gang (gang, true)@, except that the t'Gang' still
-- exists (but is empty) upon return.
withdrawAll
  :: HasCallStack
  => Gang
  -> IO ()
withdrawAll gang = do
  hs <- getGangMembers gang
  clearGang gang
  mapM_ withdraw hs

-- | A threadsafe way to interrupt any call to 'awaitNextMulti'
-- on this t'Gang'.  For each time that this function is called, one
-- call to 'awaitNextMulti' will throw a t'PlasmaException' with a
-- t'Retort' of 'System.Loam.Retorts.Constants.POOL_AWAIT_WOKEN'.
--
-- Unlike t'Hose's, all gangs have wakeup enabled. Thus there is
-- no need for an equivalent of 'enableWakeup' for gangs.
wakeGang
  :: HasCallStack
  => Gang
  -> IO ()
wakeGang gang = do
  let ei = mkErrInfo "wakeGang" gang callStack
  gangMiscOp ei 'w' gang def

-- | Retrieve the next protein available from one of the pools in
-- the specified gang.  Also returns a t'Hose' indicating which
-- pool the protein came from.
--
-- Besides the errors that can be thrown by
-- 'next', this function can throw a t'PlasmaException' with a
-- t'Retort' of 'System.Loam.Retorts.Constants.POOL_EMPTY_GANG'
-- if the specified gang contains no hoses.
nextMulti
  :: HasCallStack
  => Gang
  -> IO (RetProtein, Hose)
nextMulti gang = do
  let ei = mkErrInfo "nextMulti" gang callStack
  gangNextOp ei 'n' gang def

-- | Retrieve the next protein available from one of the pools in
-- the specified gang.  See 'awaitNext' and 'nextMulti' for more
-- information.
awaitNextMulti
  :: HasCallStack
  => Gang
  -> PoolTimeout
  -> IO (RetProtein, Hose)
awaitNextMulti gang timeout = do
  let ei = mkErrInfo "awaitNextMulti" gang callStack
  gangNextOp ei 'a' gang timeout

-- | Wait until 'awaitNextMulti' would have returned, but without
-- returning any information, and without advancing the index of the
-- pool hose.
awaitMulti
  :: HasCallStack
  => Gang
  -> PoolTimeout
  -> IO ()
awaitMulti gang timeout = do
  let ei = mkErrInfo "awaitMulti" gang callStack
  gangMiscOp ei 'a' gang timeout

--

getHoses :: Gang -> IO HosesRef
getHoses gang = withForeignPtr (gangPtr gang) $ \pGang -> do
  c_get_gang_hoses pGang >>= deRefStablePtr

gangOp
  :: ErrInfo
  -> Char        -- op
  -> Gang
  -> Hose
  -> IO ()
gangOp ei op gang hose = do
  let (addn, erl) = mkAddn   ei (Just hose)
      c           = toCChar  op
      cs          = errStack ei
  withForeignPtr (gangPtr gang) $ \gPtr -> do
    withForeignPtr (hosePtr hose) $ \hPtr -> do
      tort <- c_gang_op c gPtr hPtr
      throwRetortCS_ EtPools addn (Retort tort) erl cs

gangNextOp
  :: ErrInfo
  -> Char        -- op
  -> Gang
  -> PoolTimeout -- timeout
  -> IO (RetProtein, Hose)
gangNextOp ei op gang timeout = do
  let (addn, erl) = mkAddn   ei Nothing
      c           = toCChar  op
      cs          = errStack ei
  tmout    <- encTimeout ei timeout
  hosesRef <- getHoses   gang
  withForeignPtr (gangPtr gang) $ \gPtr -> do
    ((p, idx), ts, hPtr) <- withRet2 (-1, nullPtr) $ \tsPtr hpp -> do
      withReturnedSlawIdx def $ \idxPtr lenPtr -> do
        withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
          c_gang_next_op c gPtr tmout hpp tsPtr idxPtr tortPtr lenPtr
    hose <- findInHoses ei hosesRef hPtr
    return (RetProtein p idx ts, hose)

gangMiscOp
  :: ErrInfo
  -> Char        -- op
  -> Gang
  -> PoolTimeout -- timeout
  -> IO ()
gangMiscOp ei op gang timeout = do
  let (addn, erl) = mkAddn   ei Nothing
      c           = toCChar  op
      cs          = errStack ei
  tmout <- encTimeout ei timeout
  withForeignPtr (gangPtr gang) $ \gPtr -> do
    tort <- c_gang_misc_op c gPtr tmout
    throwRetortCS_ EtPools addn (Retort tort) erl cs

mkAddn
  :: ErrInfo
  -> Maybe Hose
  -> (Maybe String, Maybe ErrLocation)
mkAddn ei mHose = first Just $ mkAddn' ei mHose

mkAddn'
  :: ErrInfo
  -> Maybe Hose
  -> (String, Maybe ErrLocation)
mkAddn' ei mHose = (addn, erl)
  where addn  = concat [ errFunc ei
                       , ": "
                       , show (errGang ei)
                       , pname
                       ]
        pname = case mHose of
                  Just hose -> ", hose " ++ show (hoseName hose)
                  Nothing   -> ""
        erl   = fmap erlFromHose mHose

mkErrInfo :: String -> Gang -> CallStack -> ErrInfo
mkErrInfo loc gang cs =
  ErrInfo { errFunc  = loc
          , errStack = cs
          , errGang  = gangName gang
          }

encTimeout :: ErrInfo -> PoolTimeout -> IO Double
encTimeout ei timeout = do
  let (addn, erl) = mkAddn' ei Nothing
  case encodePoolTimeout timeout of
    Right t  -> return t
    Left msg ->
      throwIO $ def { peType      = EtPools
                    , peMessage   = addn ++ ": " ++ msg
                    , peCallstack = Just $ errStack ei
                    , peLocation  = erl
                    }
