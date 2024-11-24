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

import Data.Slaw
-- import Data.Slaw.Internal
import System.Loam.Hash
-- import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Misc
import System.Loam.Retorts
import System.Loam.Retorts.Constants

foreign import capi safe "ze-hs-rand.h ze_hs_truly_random"
    c_truly_random :: Ptr FgnRand -> CSize -> IO Int64

foreign import capi unsafe "libLoam/c/ob-rand.h value OB_RAND_COMPLETELY_RANDOM_PLEASE"
    c_completely_random :: Int32

foreign import capi safe "ze-hs-rand.h ze_hs_rand_allocate_state"
    c_rand_allocate_state :: Int32 -> IO (Ptr FgnRand)

foreign import capi unsafe "ze-hs-rand.h &ze_hs_rand_free_state"
    c_rand_free_state :: FunPtr (Ptr FgnRand -> IO ())

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_float64"
    c_rand_state_float64 :: Double -> Double -> Ptr FgnRand -> IO Double

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_int32"
    c_rand_state_int32 :: Int32 -> Int32 -> Ptr FgnRand -> IO Int32

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_unt32"
    c_rand_state_unt32 :: Ptr FgnRand -> IO Word32

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_state_unt64"
    c_rand_state_unt64 :: Ptr FgnRand -> IO Word64

foreign import capi unsafe "libLoam/c/ob-rand.h ob_rand_normal_state"
    c_rand_normal_state :: Ptr FgnRand -> Ptr Double -> IO Double

foreign import capi unsafe "libLoam/c/ob-rand.h ob_random_bytes_state"
    c_random_bytes_state :: Ptr FgnRand -> Ptr Word8 -> CSize -> IO ()

-- | Encapsulates the state of a random number generator.
data RandState = RandState
  { rsName :: !T.Text
  , rsSeed :: !(Maybe Int)
  , rsPtr  :: !(ForeignPtr FgnRand)
  } deriving (Eq, Ord)

instance NFData RandState where
  rnf x = rsName x `deepseq` rsSeed x `deepseq` (rsPtr x `seq` ())

instance Hashable RandState where
  hash                rs = hashInt        $ fPtrToIntegral (rsPtr rs)
  salt `hashWithSalt` rs = salt `hash2xInt` fPtrToIntegral (rsPtr rs)

instance Show RandState where
  show rs = fmtForeignObj "RandState" (rsName rs) [info] (rsPtr rs)
    where
      info = "seed=" ++ seed
      seed = case rsSeed rs of
               Nothing -> "random"
               Just x  -> show x

instance Nameable RandState where
  typeName _ = "RandState"

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
  | otherwise = do
      initialize
      allocaBytes nBytes $ \ptr -> do
        tort <- Retort <$> c_truly_random ptr (fromIntegral nBytes)
        throwRetortCS_ EtOther (Just "trulyRandom") tort Nothing callStack
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
newRandState name0 seed = do
  initialize
  name <- nonEmptyName "RandState" name0 callStack
  ptr  <- c_rand_allocate_state (makeSeed seed)
  when (ptr == nullPtr) $ do
    let addn = Just "newRandState"
    throwRetortCS_ EtOther addn OB_NO_MEM Nothing callStack
  fptr <- newForeignPtr c_rand_free_state ptr
  return $ RandState { rsName = name
                     , rsSeed = seed
                     , rsPtr  = fptr
                     }

checkFinite
  :: (RealFloat a, Show a)
  => String
  -> CallStack
  -> String
  -> a
  -> IO()
checkFinite func cs argName x = do
  when (isNaN x || isInfinite x) $ do
    let msg = concat [ func
                     , ": "
                     , show argName
                     , " is "
                     , show x
                     , ", but expected a finite number"
                     ]
    throwIO $ def { peType      = EtInvalidArgument
                  , peRetort    = Just OB_INVALID_ARGUMENT
                  , peMessage   = msg
                  , peCallstack = Just cs
                  }

checkBounds :: (Ord a, Show a) => String -> CallStack -> a -> a -> IO ()
checkBounds func cs lo hi = do
  when (lo >= hi) $ do
    let msg = concat [ func
                     , ": lower bound "
                     , show lo
                     , " was not less than upper bound "
                     , show hi
                     ]
    throwIO $ def { peType      = EtInvalidArgument
                  , peRetort    = Just OB_INVALID_ARGUMENT
                  , peMessage   = msg
                  , peCallstack = Just cs
                  }

-- | Returns a uniformly distributed 'Double' @x@,
-- where @low <= x < high@
randFloat64
  :: HasCallStack
  => Double    -- ^ low  (inclusive)
  -> Double    -- ^ high (exclusive)
  -> RandState -- ^ random number generator
  -> IO Double
randFloat64 lo hi rs = do
  checkFinite "randFloat64" callStack "lo" lo
  checkFinite "randFloat64" callStack "hi" hi
  checkBounds "randFloat64" callStack lo hi
  withForeignPtr (rsPtr rs) $ c_rand_state_float64 lo hi

-- | Returns a uniformly distributed 'Int32' @x@,
-- where @low <= x < high@
randInt32
  :: HasCallStack
  => Int32     -- ^ low  (inclusive)
  -> Int32     -- ^ high (exclusive)
  -> RandState -- ^ random number generator
  -> IO Int32
randInt32 lo hi rs = do
  checkBounds "randInt32" callStack lo hi
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
  | otherwise = do
      allocaBytes nBytes $ \bufPtr -> do
        withForeignPtr (rsPtr rs) $ \ptr -> do
          c_random_bytes_state ptr bufPtr (fromIntegral nBytes)
          B.packCStringLen (castPtr bufPtr, nBytes)
