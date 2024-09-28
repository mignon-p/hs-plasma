{-|
Module      : System.Loam.Hash
Description : Functions from ob-hash.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}

module System.Loam.Hash
  ( jenkinsHash32
  , jenkinsHash64
  , jenkinsHash
  , cityHash64
  , cityHash64withSeed
  , cityHash64withSeeds
  , hashWord64
  , hashWord32
  , hashWord
  , hashInt
  , hash2xWord64
  , hash2xWord32
  , hash2xWord
  , hash2xInt
  ) where

-- import Control.Exception
import Data.Bits
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

#include "MachDeps.h"

foreign import capi unsafe "libLoam/c/ob-hash.h ob_jenkins_hash"
    c_jenkins_hash :: C.ConstPtr () -> CSize -> Word32 -> IO Word32

foreign import capi unsafe "ze-hs-misc.h ze_hs_jenkins_hash64"
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

{-# INLINABLE cityHash64withSeed #-}
cityHash64withSeed :: Word64 -> B.ByteString -> Word64
cityHash64withSeed = cityHash64withSeeds 0x9ae1_6a3b_2f90_404f

cityHash64withSeeds :: Word64 -> Word64 -> B.ByteString -> Word64
cityHash64withSeeds seed0 seed1 bs = unsafePerformIO $ callHash f bs
  where f ptr size = c_city_hash64_with_seeds ptr size seed0 seed1

{-# INLINABLE hash2xWord32 #-}
hash2xWord32 :: Word32 -> Word32 -> Word32
hash2xWord32 w1 w2 = fromIntegral $ hashWord64 w
  where w = (fromIntegral w1 `shiftL` 32) `xor` fromIntegral w2

{-# INLINE jenkinsHash #-}
jenkinsHash :: Word -> B.ByteString -> Word

{-# INLINE hashWord #-}
hashWord    :: Word -> Word

{-# INLINE hash2xWord #-}
hash2xWord  :: Word -> Word -> Word

#if WORD_SIZE_IN_BITS > 32
jenkinsHash seed = fromIntegral . jenkinsHash64 (fromIntegral seed)
hashWord         = fromIntegral . hashWord64   . fromIntegral
hash2xWord w1 w2 =
  fromIntegral $ hash2xWord64 (fromIntegral w1) (fromIntegral w2)
#else  /* WORD_SIZE_IN_BITS */
jenkinsHash seed = fromIntegral . jenkinsHash32 (fromIntegral seed)
hashWord         = fromIntegral . hashWord32   . fromIntegral
hash2xWord w1 w2 =
  fromIntegral $ hash2xWord32 (fromIntegral w1) (fromIntegral w2)
#endif /* WORD_SIZE_IN_BITS */

{-# INLINE hashInt #-}
hashInt :: Int -> Int
hashInt = fromIntegral . hashWord . fromIntegral

{-# INLINE hash2xInt #-}
hash2xInt :: Int -> Int -> Int
hash2xInt x1 x2 =
  fromIntegral $ hash2xWord (fromIntegral x1) (fromIntegral x2)
