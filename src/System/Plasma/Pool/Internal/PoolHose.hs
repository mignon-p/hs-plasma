{-|
Module      : System.Plasma.Pool.Internal.PoolHose
Description : Hose type
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.PoolHose
  ( Hose(..)
  , PoolIndex
  , newHose
  , withdraw
  , getHoseContext
  , cloneHose
  , deposit
  ) where

import Control.DeepSeq
-- import Control.Exception
-- import Control.Monad
-- import qualified Data.ByteString          as B
-- import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
-- import Data.Word
-- import Foreign.C.String
-- import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import GHC.Stack
-- import System.IO.Unsafe

import Data.Slaw
-- import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
-- import System.Loam.Retorts.Constants
import System.Loam.Time
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolName

foreign import capi safe "ze-hs-hose.h ze_hs_make_hose"
    c_make_hose :: Ptr FgnRawHose -> StablePtr Context -> C.ConstCString -> Ptr Int64 -> IO (Ptr FgnHose)

foreign import capi unsafe "ze-hs-hose.h &ze_hs_finalize_hose"
    c_finalize_hose :: FunPtr (Ptr FgnHose -> IO ())

foreign import capi safe "ze-hs-hose.h ze_hs_withdraw"
    c_withdraw :: Ptr FgnHose -> IO Int64

foreign import capi unsafe "ze-hs-hose.h ze_hs_get_context"
    c_get_context :: Ptr FgnHose -> IO (StablePtr Context)

foreign import capi safe "ze-hs-hose.h ze_hs_hose_clone"
    c_hose_clone :: Ptr FgnHose -> Ptr Int64 -> IO (Ptr FgnRawHose)

foreign import capi safe "ze-hs-hose.h ze_hs_deposit"
    c_deposit :: Ptr FgnHose -> C.ConstPtr FgnSlaw -> Ptr Int64 -> Ptr Double -> IO Int64

kHose :: IsString a => a
kHose = "Hose"

type PoolIndex = Int64

data Hose = Hose
  { hoseName :: !T.Text
  , hosePool :: !PoolName
  , hosePtr  :: !(ForeignPtr FgnHose)
  } deriving (Eq, Ord)

instance NFData Hose where
  rnf x = hoseName x `deepseq` hosePool x `deepseq` (hosePtr x `seq` ())

instance Hashable Hose where
  hash                hose = hashInt $ fPtrToIntegral (hosePtr hose)
  salt `hashWithSalt` hose =
    salt `hash2xInt` fPtrToIntegral (hosePtr hose)

instance Show Hose where
  show hose = fmtForeignObj kHose (hoseName hose) [info] (hosePtr hose)
    where
      info = "pool=" ++ show (hosePool hose)

newHose
  :: String         -- ^ name of API function
  -> CallStack
  -> T.Text         -- ^ name of this Hose
  -> PoolName       -- ^ name of pool
  -> Context        -- ^ context used when creating Hose
  -> Ptr FgnRawHose -- ^ actual pointer to the hose
  -> IO Hose
newHose loc cs name0 pool ctx hPtr = do
  let erl  = Just $ erlFromPoolName pool
      addn = Just loc
  name    <- nonEmptyName kHose name0 cs
  stabPtr <- newStablePtr ctx
  zHose   <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    C.useAsConstCString (T.encodeUtf8 name) $ \namePtr -> do
      c_make_hose hPtr stabPtr namePtr tortPtr
  fptr    <- newForeignPtr c_finalize_hose zHose
  return $ Hose { hoseName = name
                , hosePool = pool
                , hosePtr  = fptr
                }

erlFromHose :: Hose -> Maybe ErrLocation
erlFromHose = Just . erlFromPoolName . hosePool

withdraw :: HasCallStack => Hose -> IO ()
withdraw hose = withForeignPtr (hosePtr hose) $ \ptr -> do
  let erl  = erlFromHose hose
      addn = Just "withdraw"
  tort <- c_withdraw ptr
  throwRetortCS EtPools addn (Retort tort) erl callStack

getHoseContext :: Hose -> IO Context
getHoseContext h = withForeignPtr (hosePtr h) $ \hPtr -> do
  c_get_context hPtr >>= deRefStablePtr

cloneHose
  :: HasCallStack
  => T.Text       -- ^ name of new Hose
  -> Hose         -- ^ hose to clone
  -> IO Hose
cloneHose name orig = withForeignPtr (hosePtr orig) $ \origPtr -> do
  let loc  = "cloneHose"
      addn = Just loc
      pool = hosePool orig
      erl  = Just $ erlFromPoolName pool
      cs   = callStack
  spCtx <- c_get_context origPtr
  ctx   <- deRefStablePtr spCtx
  new   <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    c_hose_clone origPtr tortPtr
  newHose loc cs name pool ctx new

deposit
  :: (HasCallStack, ToSlaw a)
  => Hose
  -> a
  -> IO (PoolIndex, WallTime)
deposit h opts = withForeignPtr (hosePtr h) $ \hPtr -> do
  let addn = Just "deposit"
      erl  = erlFromHose h
  withSlaw (š opts) $ \slawPtr -> do
    alloca $ \idxPtr -> do
      alloca $ \timePtr -> do
        poke idxPtr  minBound
        poke timePtr (-1)
        tort <- Retort <$> c_deposit hPtr slawPtr idxPtr timePtr
        throwRetortCS EtPools addn tort erl callStack
        idx  <- peek idxPtr
        ts   <- peek timePtr
        return (idx, ts)
