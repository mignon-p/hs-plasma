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
  , newHose
  , withdraw
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
-- import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
-- import Foreign.Storable
import GHC.Stack
-- import System.IO.Unsafe

import Data.Slaw
-- import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Misc
import System.Loam.Internal.Marshal
import System.Loam.Retorts
-- import System.Loam.Retorts.Constants
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolName

foreign import capi safe "ze-hs-hose.h ze_hs_make_hose"
    c_make_hose :: Ptr () -> StablePtr Context -> C.ConstCString -> Ptr Int64 -> IO (Ptr ())

foreign import capi unsafe "ze-hs-hose.h &ze_hs_finalize_hose"
    c_finalize_hose :: FunPtr (Ptr () -> IO ())

foreign import capi safe "ze-hs-hose.h ze_hs_withdraw"
    c_withdraw :: Ptr () -> IO Int64

kHose :: IsString a => a
kHose = "Hose"

data Hose = Hose
  { hoseName :: !T.Text
  , hosePool :: !PoolName
  , hosePtr  :: !(ForeignPtr ())
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
  :: String       -- ^ name of API function
  -> CallStack
  -> T.Text       -- ^ name of this Hose
  -> PoolName     -- ^ name of pool
  -> Context      -- ^ context used when creating Hose
  -> Ptr ()       -- ^ actual pointer to the hose
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
