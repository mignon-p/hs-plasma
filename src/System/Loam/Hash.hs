{-|
Module      : System.Loam.Hash
Description : Functions from ob-hash.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Hash
  ( jenkinsHash32
  , jenkinsHash64
  , cityHash64
  , cityHash64withSeed
  , cityHash64withSeeds
  , hashWord64
  , hashWord32
  , hash2xWord64
  ) where

-- import Control.Exception
import qualified Data.ByteString          as B
-- import Data.Default.Class
-- import Data.Int
-- import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
import Data.Word
-- import Foreign.C.String
import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Marshal.Utils
-- import Foreign.Ptr
-- import Foreign.Storable
-- import GHC.Stack
import System.IO.Unsafe

import qualified System.Loam.Internal.ConstPtr as C

foreign import capi unsafe "libLoam/c/ob-hash.h ob_jenkins_hash"
    c_jenkins_hash :: C.ConstPtr () -> CSize -> Word32 -> IO Word32

foreign import capi unsafe "ze-hs-plasma.h ze_hs_jenkins_hash64"
    c_jenkins_hash64 :: C.ConstPtr () -> CSize -> Word64 -> IO Word64

foreign import capi unsafe "libLoam/c/ob-hash.h ob_city_hash64"
    c_city_hash64 :: C.ConstPtr () -> CSize -> IO Word64

foreign import capi unsafe "libLoam/c/ob-hash.h ob_city_hash64_with_seeds"
    c_city_hash64_with_seeds
      :: C.ConstPtr () -- key
      -> CSize         -- length
      -> Word64        -- seed0
      -> Word64        -- seed1
      -> IO Word64     -- return value

foreign import capi unsafe "libLoam/c/ob-hash.h ob_hash_unt64"
    hashWord64 :: Word64 -> Word64

foreign import capi unsafe "libLoam/c/ob-hash.h ob_hash_unt32"
    hashWord32 :: Word32 -> Word32

foreign import capi unsafe "libLoam/c/ob-hash.h ob_hash_2xunt64_to_unt64"
    hash2xWord64 :: Word64 -> Word64 -> Word64

callHash :: (C.ConstPtr () -> CSize -> IO a) -> B.ByteString -> IO a
callHash f bs = do
  C.useAsConstCStringLen bs $ \(charPtr, len) -> do
    f (C.castConstPtr charPtr) (fromIntegral len)

jenkinsHash32 :: Word32 -> B.ByteString -> Word32
jenkinsHash32 seed bs = unsafePerformIO $ callHash f bs
  where f ptr size = c_jenkins_hash ptr size seed

jenkinsHash64 :: Word64 -> B.ByteString -> Word64
jenkinsHash64 seed bs = unsafePerformIO $ callHash f bs
  where f ptr size = c_jenkins_hash64 ptr size seed

cityHash64 :: B.ByteString -> Word64
cityHash64 bs = unsafePerformIO $ callHash c_city_hash64 bs

cityHash64withSeed :: Word64 -> B.ByteString -> Word64
cityHash64withSeed = cityHash64withSeeds 0x9ae1_6a3b_2f90_404f

cityHash64withSeeds :: Word64 -> Word64 -> B.ByteString -> Word64
cityHash64withSeeds seed0 seed1 bs = unsafePerformIO $ callHash f bs
  where f ptr size = c_city_hash64_with_seeds ptr size seed0 seed1
