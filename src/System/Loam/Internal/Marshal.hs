{-|
Module      : System.Loam.Internal.Marshal
Description : Functions for marshaling lazy ByteStrings
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Internal.Marshal
  ( withLazyByteStringAsCString
  , withLazyByteStringAsCStringNL
  , withLazyByteStringAsCStringLen
    --
  , copyLazyByteStringToBuffer
    --
  , withSlaw
  , withBinarySlaw
    --
  , SlawLen
  , withReturnedSlaw
  , withReturnedRetort
  , withReturnedRetortCS
  ) where

import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Unsafe   as B
import Data.Char
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

import Data.Slaw
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Retorts
import System.Loam.Retorts.Constants

type SlawLen = Int64

foreign import capi unsafe "libPlasma/c/slaw.h &slaw_free"
  finalizerSlaw :: FinalizerPtr FgnSlaw

withLazyByteStringAsCString :: L.ByteString
                            -> (CString -> IO a)
                            -> IO a
withLazyByteStringAsCString lbs func =
  withLazyByteStringAsCStringLen' False lbs (func . fst)

withLazyByteStringAsCStringNL :: L.ByteString
                              -> (CString -> IO a)
                              -> IO a
withLazyByteStringAsCStringNL lbs func =
  withLazyByteStringAsCStringLen' True lbs (func . fst)

withLazyByteStringAsCStringLen :: L.ByteString
                               -> (CStringLen -> IO a)
                               -> IO a
withLazyByteStringAsCStringLen = withLazyByteStringAsCStringLen' False

withLazyByteStringAsCStringLen' :: Bool -- add a terminating newline?
                                -> L.ByteString
                                -> (CStringLen -> IO a)
                                -> IO a
withLazyByteStringAsCStringLen' termNL lbs func = do
  let len    = L.length lbs
      extra  = 2 -- one for terminating NUL and one for optional newline
      bufLen = len + extra
  when (bufLen >= fromIntegral (maxBound :: Int)) $
    fail $ "length " ++ show bufLen ++ " is too big"
  allocaBytes (fromIntegral bufLen) $ \buf -> do
    withLbs1 termNL (L.toChunks lbs) func buf 0

withLbs1 :: Bool
         -> [B.ByteString]
         -> (CStringLen -> IO a)
         -> CString
         -> Int
         -> IO a
withLbs1 termNL [] func buf !pos = do
  haveNL <- endsWithNewline buf pos
  let needNL = termNL && not haveNL
      byte1  = if needNL then newline else 0
      byte2  = 0 :: CChar
      len    = if needNL then pos + 1 else pos
  pokeByteOff buf  pos      byte1
  pokeByteOff buf (pos + 1) byte2
  func (buf, len)
withLbs1 termNL (bs:rest) func buf !pos = do
  B.unsafeUseAsCStringLen bs $ \(src, srcLen) -> do
    copyBytes (buf `plusPtr` pos) src srcLen
    withLbs1 termNL rest func buf (pos + srcLen)

newline :: CChar
newline = (fromIntegral . ord) '\n'

endsWithNewline :: CString -> Int -> IO Bool
endsWithNewline _ 0 = return False
endsWithNewline buf pos = do
  lastByte <- peekByteOff buf (pos - 1)
  return (lastByte == newline)

--

copyLazyByteStringToBuffer :: L.ByteString -> Ptr Word8 -> Int -> IO ()
copyLazyByteStringToBuffer lbs bufPtr bufSize =
  copyLbs1 (L.toChunks lbs) (castPtr bufPtr) bufSize

copyLbs1 :: [B.ByteString] -> Ptr CChar -> Int -> IO ()
copyLbs1 (bs:rest) bufPtr bufSize
  | bufSize > 0 = do
      let cpyLen = B.length bs `min` bufSize
      B.unsafeUseAsCString bs $ \srcPtr -> do
        copyBytes bufPtr srcPtr cpyLen
      copyLbs1 rest (bufPtr `plusPtr` cpyLen) (bufSize - cpyLen)
copyLbs1 _ _ _ = return ()

--

withSlaw :: Slaw -> (C.ConstPtr FgnSlaw -> IO a) -> IO a
withSlaw slaw func = do
  let lbs = encodeSlaw nativeByteOrder slaw
  withLazyByteStringAsCString lbs (func . C.ConstPtr . castPtr)

withBinarySlaw :: BinarySlaw -> (C.ConstPtr FgnSlaw -> IO a) -> IO a
withBinarySlaw lbs func =
  withLazyByteStringAsCString lbs (func . C.ConstPtr . castPtr)

--

withReturnedSlaw
  :: ErrLocation
  -> (Ptr SlawLen -> IO (Ptr FgnSlaw))
  -> IO (Maybe Slaw)
withReturnedSlaw erl f = do
  mbs <- withReturnedSlaw0 f
  case mbs of
    Just bs
      | B.null bs -> do
          let msg =
                "Corrupt/unrecognized slaw (could not determine length)"
          return $ Just $ SlawError msg erl
      | otherwise -> do
          let lbs  = L.fromStrict bs
          return $ Just $ decodeSlaw' nativeByteOrder erl lbs
    Nothing -> return Nothing

withReturnedSlaw0
  :: (Ptr SlawLen -> IO (Ptr FgnSlaw))
  -> IO (Maybe B.ByteString)
withReturnedSlaw0 f = alloca $ \lenPtr -> do
  poke lenPtr (-1)
  slawPtr <- f lenPtr
  byteLen <- peek lenPtr
  if slawPtr == nullPtr || byteLen < 0
    then return Nothing
    else Just <$> unsafePackMallocSlaw (slawPtr, byteLen)

unsafePackMallocSlaw :: (Ptr FgnSlaw, Int64) -> IO B.ByteString
unsafePackMallocSlaw (ptr, len) = do
  fp <- newForeignPtr finalizerSlaw ptr
  return $ B.BS (castForeignPtr fp) (fromIntegral len)

withReturnedRetort
  :: HasCallStack
  => PlasmaExceptionType
  -> Maybe String
  -> Maybe ErrLocation
  -> (Ptr Int64 -> IO a)
  -> IO a
withReturnedRetort et addn erl =
  withReturnedRetortCS et addn erl callStack

withReturnedRetortCS
  :: PlasmaExceptionType
  -> Maybe String
  -> Maybe ErrLocation
  -> CallStack
  -> (Ptr Int64 -> IO a)
  -> IO a
withReturnedRetortCS et addn erl cs f = alloca $ \tortPtr -> do
  poke tortPtr $ unRetort OB_UNKNOWN_ERR
  ret  <- f tortPtr
  tort <- Retort <$> peek tortPtr
  throwRetortCS et addn tort erl cs
  return ret
