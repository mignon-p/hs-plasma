{-|
Module      : System.Loam.Hash
Description : Functions from ob-hash.h
Copyright   : © Mignon Pelletier, 2024-2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}

module System.Loam.Hash
  ( -- * Hash functions on bytestrings.
    jenkinsHash32
  , jenkinsHash64
  , jenkinsHash
  , cityHash64
  , cityHash64withSeed
  , cityHash64withSeeds
    -- * Hash functions on a single integer.
  , hashWord64
  , hashWord32
  , hashWord
  , hashInt
    -- * Hash two integers into one integer.
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

foreign import capi safe "libLoam/c/ob-hash.h ob_jenkins_hash"
    c_jenkins_hash_safe :: C.ConstPtr () -> CSize -> Word32 -> IO Word32

foreign import capi safe "ze-hs-misc.h ze_hs_jenkins_hash64"
    c_jenkins_hash64_safe :: C.ConstPtr () -> CSize -> Word64 -> IO Word64

foreign import capi safe "libLoam/c/ob-hash.h ob_city_hash64"
    c_city_hash64_safe :: C.ConstPtr () -> CSize -> IO Word64

foreign import capi safe "libLoam/c/ob-hash.h ob_city_hash64_with_seeds"
    c_city_hash64_with_seeds_safe
      :: C.ConstPtr () -- key
      -> CSize         -- length
      -> Word64        -- seed0
      -> Word64        -- seed1
      -> IO Word64     -- return value

-- | Hashes a 'Word64'.
foreign import capi unsafe "libLoam/c/ob-hash.h ob_hash_unt64"
    hashWord64 :: Word64 -> Word64

-- | Hashes a 'Word32'.
foreign import capi unsafe "libLoam/c/ob-hash.h ob_hash_unt32"
    hashWord32 :: Word32 -> Word32

-- | Hashes two 64-bit words to produce a single 64-bit word.
foreign import capi unsafe "libLoam/c/ob-hash.h ob_hash_2xunt64_to_unt64"
    hash2xWord64 :: Word64 -> Word64 -> Word64

type HashFunc a = C.ConstPtr () -> CSize -> IO a

-- I have no idea if 1024 is a good value.
threshold :: CSize
threshold = 1024

-- Takes two versions of the hash function.
-- "f" is used for short inputs, and "g" is used for long inputs.
-- "f" and "g" should be the same function, except "f" should
-- be "unsafe" and "g" should be "safe".  Therefore, we minimize
-- overhead on short bytestrings, but avoid spending large amounts
-- of time in unsafe foreign calls.
callHash :: HashFunc a -> HashFunc a -> B.ByteString -> IO a
callHash f g bs = do
  C.useAsConstCStringLen bs $ \(charPtr, len) -> do
    let p = C.castConstPtr charPtr
        l = fromIntegral len
    if l < threshold then f p l else g p l

-- | Hashes a 'B.ByteString' using the Bob Jenkins “lookup3”
-- algorithm.
jenkinsHash32
  :: Word32       -- seed
  -> B.ByteString -- bytes to hash
  -> Word32
jenkinsHash32 seed bs = unsafePerformIO $ callHash f g bs
  where f ptr size = c_jenkins_hash      ptr size seed
        g ptr size = c_jenkins_hash_safe ptr size seed

-- | Hashes a 'B.ByteString' using the Bob Jenkins “lookup3”
-- algorithm.  This version takes two 32-bit seeds, and produces
-- two 32-bit outputs, but here we have combined them into
-- 64-bit values for convenience.
jenkinsHash64
  :: Word64       -- seed
  -> B.ByteString -- bytes to hash
  -> Word64
jenkinsHash64 seed bs = unsafePerformIO $ callHash f g bs
  where f ptr size = c_jenkins_hash64      ptr size seed
        g ptr size = c_jenkins_hash64_safe ptr size seed

-- | Hashes a 'B.ByteString' using Google's CityHash64
-- algorithm, version 1.0.2.
cityHash64
  :: B.ByteString -- bytes to hash
  -> Word64
cityHash64 bs =
  unsafePerformIO $ callHash c_city_hash64 c_city_hash64_safe bs

{-# INLINABLE cityHash64withSeed #-}
-- | Hashes a 'B.ByteString' using Google's CityHash64
-- algorithm, version 1.0.2.  Takes one seed.
cityHash64withSeed
  :: Word64       -- seed
  -> B.ByteString -- bytes to hash
  -> Word64
cityHash64withSeed = cityHash64withSeeds 0x9ae1_6a3b_2f90_404f

-- | Hashes a 'B.ByteString' using Google's CityHash64
-- algorithm, version 1.0.2.  Takes two seeds.
cityHash64withSeeds
  :: Word64       -- first seed
  -> Word64       -- second seed
  -> B.ByteString -- bytes to hash
  -> Word64
cityHash64withSeeds seed0 seed1 bs = unsafePerformIO $ callHash f g bs
  where f ptr size = c_city_hash64_with_seeds      ptr size seed0 seed1
        g ptr size = c_city_hash64_with_seeds_safe ptr size seed0 seed1

{-# INLINABLE hash2xWord32 #-}
-- | Hashes two 32-bit words to produce a single 32-bit word.
hash2xWord32 :: Word32 -> Word32 -> Word32
hash2xWord32 w1 w2 = fromIntegral $ hashWord64 w
  where w = (fromIntegral w1 `shiftL` 32) `xor` fromIntegral w2

-- | Hashes a 'B.ByteString' using the Bob Jenkins “lookup3”
-- algorithm.  Takes a 'Word' seed, and produces a 'Word' output.
{-# INLINE jenkinsHash #-}
jenkinsHash :: Word -> B.ByteString -> Word

{-# INLINE hashWord #-}
-- | Hashes a 'Word'.
hashWord    :: Word -> Word

{-# INLINE hash2xWord #-}
-- | Hashes two 'Word's into one 'Word'.
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
-- | Hashes an 'Int'.
hashInt :: Int -> Int
hashInt = fromIntegral . hashWord . fromIntegral

{-# INLINE hash2xInt #-}
-- | Hashes two 'Int's into one 'Int'.
hash2xInt :: Int -> Int -> Int
hash2xInt x1 x2 =
  fromIntegral $ hash2xWord (fromIntegral x1) (fromIntegral x2)
