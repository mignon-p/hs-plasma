{-|
Module      : System.Loam.Rand
Description : Functions from ob-rand.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Rand
  ( -- * Unpredictable numbers
    trulyRandom
    -- * Pseudo-random numbers (Mersenne Twister)
  , RandState     -- opaque
  , newRandState
  , randFloat64
  , randInt32
  , randWord32
  , randWord64
  , randNormal
  , randBytes
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.ByteString          as B
import Data.Default.Class
import Data.Hashable
import Data.Int
import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
import Data.Word
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

import Data.Slaw.Internal
import System.Loam.Hash
-- import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Misc
import System.Loam.Retorts
import System.Loam.Retorts.Constants

foreign import capi "libLoam/c/ob-rand.h ob_truly_random"
    c_truly_random :: Ptr () -> CSize -> IO Int64

foreign import capi unsafe "libLoam/c/ob-rand.h value OB_RAND_COMPLETELY_RANDOM_PLEASE"
    c_completely_random :: Int32

foreign import capi "libLoam/c/ob-rand.h ob_rand_allocate_state"
    c_rand_allocate_state :: Int32 -> IO (Ptr ())

foreign import capi unsafe "ze-hs-misc.h &ze_hs_rand_free_state"
    c_rand_free_state :: FunPtr (Ptr () -> IO ())

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_float64"
    c_rand_state_float64 :: Double -> Double -> Ptr () -> IO Double

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_int32"
    c_rand_state_int32 :: Int32 -> Int32 -> Ptr () -> IO Int32

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_unt32"
    c_rand_state_unt32 :: Ptr () -> IO Word32

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_unt64"
    c_rand_state_unt64 :: Ptr () -> IO Word64

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_normal_state"
    c_rand_normal_state :: Ptr () -> Ptr Double -> IO Double

foreign import capi unsafe "libLoam/c/ob-rand.h ob_random_bytes_state"
    c_random_bytes_state :: Ptr () -> Ptr Word8 -> CSize -> IO ()

-- | Encapsulates the state of a random number generator.
data RandState = RandState
  { rsName :: !T.Text
  , rsSeed :: !(Maybe Int)
  , rsPtr  :: !(ForeignPtr ())
  } deriving (Eq, Ord)

instance NFData RandState where
  rnf x = rsName x `deepseq` rsSeed x `deepseq` (rsPtr x `seq` ())

instance Hashable RandState where
  hash                rs = hashInt        $ fPtrToIntegral (rsPtr rs)
  salt `hashWithSalt` rs = salt `hash2xInt` fPtrToIntegral (rsPtr rs)

instance Show RandState where
  show rs  = concat [ "{RandState: "
                    , name
                    , " seed="
                    , seed
                    , " <"
                    , ptr
                    , ">}"
                    ]
    where
      name = showEscapedStr $ T.unpack $ rsName rs
      ptr  = fmtForeignPtr             $ rsPtr  rs
      seed = case rsSeed rs of
               Nothing -> "random"
               Just x  -> show x

-- | Produces true random numbers from a platform-specific
-- source, such as @\/dev\/urandom@.
trulyRandom
  :: HasCallStack
  => Int             -- ^ number of bytes to generate
  -> IO B.ByteString
trulyRandom nBytes
  | nBytes < 0 = do
      let msg = "trulyRandom: nBytes " ++ show nBytes ++ " < 0"
      throwIO $ def { peType      = EtInvalidArgument
                    , peMessage   = msg
                    , peCallstack = Just callStack
                    }
  | nBytes == 0 = return B.empty
  | otherwise = withFrozenCallStack $ do
      allocaBytes nBytes $ \ptr -> do
        tort <- Retort <$> c_truly_random ptr (fromIntegral nBytes)
        throwRetort EtOther (Just "trulyRandom") tort Nothing
        B.packCStringLen (castPtr ptr, nBytes)

makeSeed :: Maybe Int -> Int32
makeSeed Nothing       = c_completely_random
makeSeed (Just seedIn) =
  -- On a 64-bit system, Int will have a larger range than Int32.
  -- If the seed doesn't fit in an Int32, hash it.
  let min32  = minBound :: Int32
      max32  = maxBound :: Int32
      seed32 =
        if seedIn >= fromIntegral min32 && seedIn <= fromIntegral max32
        then fromIntegral seedIn
        else fromIntegral (hashInt seedIn)
  -- avoid getting OB_RAND_COMPLETELY_RANDOM_PLEASE by accident
  in if seed32 == c_completely_random
     then 0x4ffe874
     else seed32

-- | Allocates a new random number generator.  If a seed is provided,
-- it is seeded deterministically.  If the seed is 'Nothing', a
-- random seed is generated using 'trulyRandom'.
newRandState
  :: HasCallStack
  => T.Text    -- ^ name of this RandState (only used in 'Show' instance)
  -> Maybe Int -- ^ seed
  -> IO RandState
newRandState name seed = do
  checkCleanup
  ptr <- c_rand_allocate_state (makeSeed seed)
  when (ptr == nullPtr) $ do
    let addn = Just "newRandState"
    throwRetortCS EtOther addn OB_NO_MEM Nothing callStack
  fptr <- newForeignPtr c_rand_free_state ptr
  return $ RandState { rsName = name
                     , rsSeed = seed
                     , rsPtr  = fptr
                     }

-- | Returns a uniformly distributed 'Double' @x@,
-- where @low <= x < high@
randFloat64
  :: Double    -- ^ low
  -> Double    -- ^ high
  -> RandState -- ^ random number generator
  -> IO Double
randFloat64 lo hi rs = do
  withForeignPtr (rsPtr rs) $ c_rand_state_float64 lo hi

-- | Returns a uniformly distributed 'Int32' @x@,
-- where @low <= x < high@
randInt32
  :: Int32     -- ^ low
  -> Int32     -- ^ high
  -> RandState -- ^ random number generator
  -> IO Int32
randInt32 lo hi rs = do
  withForeignPtr (rsPtr rs) $ c_rand_state_int32 lo hi

-- | Produces all unsigned 32-bit integers with equal probability.
randWord32 :: RandState -> IO Word32
randWord32 rs = do
  withForeignPtr (rsPtr rs) c_rand_state_unt32

-- | Produces all unsigned 64-bit integers with equal probability.
randWord64 :: RandState -> IO Word64
randWord64 rs = do
  withForeignPtr (rsPtr rs) c_rand_state_unt64

-- | Generate a standard normal variate; the algorithm generates two
-- variates at once.
randNormal :: RandState -> IO (Double, Double)
randNormal rs = do
  withForeignPtr (rsPtr rs) $ \ptr -> do
    alloca $ \sndPtr -> do
      poke sndPtr 0
      ret1 <- c_rand_normal_state ptr sndPtr
      ret2 <- peek sndPtr
      return (ret1, ret2)

-- | Generate random bytes.
randBytes
  :: HasCallStack
  => Int             -- ^ number of bytes to generate
  -> RandState       -- ^ random number generator
  -> IO B.ByteString
randBytes nBytes rs
  | nBytes < 0 = do
      let msg = "randBytes: nBytes " ++ show nBytes ++ " < 0"
      throwIO $ def { peType      = EtInvalidArgument
                    , peMessage   = msg
                    , peCallstack = Just callStack
                    }
  | nBytes == 0 = return B.empty
  | otherwise = withFrozenCallStack $ do
      allocaBytes nBytes $ \bufPtr -> do
        withForeignPtr (rsPtr rs) $ \ptr -> do
          c_random_bytes_state ptr bufPtr (fromIntegral nBytes)
          B.packCStringLen (castPtr bufPtr, nBytes)
