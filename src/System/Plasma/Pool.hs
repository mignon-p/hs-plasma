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
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolHose
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolOpts

foreign import capi safe "ze-hs-pool.h ze_hs_participate"
    c_participate :: CBool -> Ptr () -> C.ConstCString -> C.ConstPtr () -> Ptr Int64 -> IO (Ptr ())

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

