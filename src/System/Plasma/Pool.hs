{-|
Module      : System.Plasma.Pool
Description : Functions from pool.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
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
    -- * Pool hoses
  , Hose
    -- ** Hose information
  , hoseName
  , hosePool
  , getHoseContext
    -- ** Hose operations
  , withdraw
  , cloneHose
  ) where

import Data.Default.Class
import Data.Int
import qualified Data.Text                as T
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Stack

import Data.Slaw
import Data.Slaw.IO.Internal.Options
import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Marshal
import System.Loam.Retorts
import System.Loam.Retorts.Constants
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolHose
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolOpts

foreign import capi safe "ze-hs-pool.h ze_hs_participate"
    c_participate :: CBool -> Ptr () -> C.ConstCString -> C.ConstPtr () -> Ptr Int64 -> IO (Ptr ())

foreign import capi safe "ze-hs-pool.h ze_hs_create"
    c_create :: Ptr () -> C.ConstCString -> C.ConstPtr () -> IO Int64

foreign import capi safe "libPlasma/c/pool.h pool_dispose_ctx"
    c_dispose_ctx :: C.ConstCString -> Ptr () -> IO Int64

foreign import capi safe "libPlasma/c/pool.h pool_rename_ctx"
    c_rename_ctx :: C.ConstCString -> C.ConstCString -> Ptr () -> IO Int64

foreign import capi safe "ze-hs-pool.h ze_hs_list"
    c_list :: Ptr () -> C.ConstCString -> Ptr Int64 -> Ptr Int64 -> IO (Ptr ())

participate
  :: HasCallStack
  => Context  -- ^ pool context
  -> T.Text   -- ^ name for new hose
  -> PoolName -- ^ name of pool to participate in
  -> IO Hose
participate ctx name pool = do
  let loc = "participate"
      cs  = callStack
  participateInternal loc False cs ctx name pool SlawNil

participateCreatingly
  :: (HasCallStack, ToSlaw a)
  => Context  -- ^ pool context
  -> T.Text   -- ^ name for new hose
  -> PoolName -- ^ name of pool to participate in
  -> a        -- ^ pool create options
  -> IO Hose
participateCreatingly ctx name pool opts = do
  let loc = "participateCreatingly"
      cs  = callStack
  participateInternal loc True cs ctx name pool (š opts)

participateInternal
  :: String   -- ^ name of API function
  -> Bool     -- ^ creatingly?
  -> CallStack
  -> Context  -- ^ pool context
  -> T.Text   -- ^ name for new hose
  -> PoolName -- ^ name of pool to participate in
  -> Slaw     -- ^ pool create options
  -> IO Hose
participateInternal loc crtly cs ctx name pool opts = do
  let cbool = fromBool crtly
      erl   = Just $ erlFromPoolName pool
      addn  = Just loc
  hPtr <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    C.useAsConstCString (toByteString pool) $ \pnPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        withSlaw opts $ \optPtr -> do
          c_participate cbool cPtr pnPtr optPtr tortPtr
  newHose loc cs name pool ctx hPtr

create
  :: (HasCallStack, ToSlaw a)
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool to create
  -> a        -- ^ pool create options
  -> IO ()
create ctx pool opts = do
  let cs    = callStack
      erl   = Just $ erlFromPoolName pool
      addn  = Just "create"
  C.useAsConstCString (toByteString pool) $ \pnPtr -> do
    withForeignPtr (ctxPtr ctx) $ \cPtr -> do
      withSlaw (š opts) $ \optPtr -> do
        tort <- c_create cPtr pnPtr optPtr
        throwRetortCS EtPools addn (Retort tort) erl cs

dispose
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool to delete
  -> IO ()
dispose ctx pool = do
  let cs    = callStack
      erl   = Just $ erlFromPoolName pool
      addn  = Just "dispose"
  C.useAsConstCString (toByteString pool) $ \pnPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        tort <- c_dispose_ctx pnPtr cPtr
        throwRetortCS EtPools addn (Retort tort) erl cs

rename
  :: HasCallStack
  => Context  -- ^ pool context
  -> PoolName -- ^ name of pool to rename
  -> PoolName -- ^ new name for pool
  -> IO ()
rename ctx oldName newName = do
  let cs     = callStack
      erlOld = Just $ erlFromPoolName oldName
      erlNew = Just $ erlFromPoolName newName
      addn   = Just "rename"
  C.useAsConstCString (toByteString oldName) $ \oldPtr -> do
    C.useAsConstCString (toByteString newName) $ \newPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        tort <- Retort <$> c_rename_ctx oldPtr newPtr cPtr
        let erl = if tort == POOL_EXISTS then erlNew else erlOld
        throwRetortCS EtPools addn tort erl cs

listPools
  :: HasCallStack
  => Context        -- ^ pool context
  -> Maybe PoolName -- ^ uri to list
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
  let addn = Just "listPools"
  mSlaw <- withReturnedSlaw erl $ \lenPtr -> do
    withReturnedRetortCS EtPools addn (Just erl) cs $ \tortPtr -> do
      withForeignPtr (ctxPtr ctx) $ \cPtr -> do
        c_list cPtr uriPtr tortPtr lenPtr
  case mSlaw >>= ŝm of
    Nothing    -> return []
    Just names -> return names
