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
  , withReturnedSlaw
  ) where

import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Unsafe   as B
import Data.Char
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

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

withReturnedSlaw
  :: (Ptr Int64 -> IO (Ptr ()))
  -> IO (Maybe L.ByteString)
withReturnedSlaw f = alloca $ \lenPtr -> do
  poke lenPtr (-1)
  slawPtr <- f lenPtr
  byteLen <- peek lenPtr
  if slawPtr == nullPtr || byteLen < 0
    then return Nothing
    else Just <$> mallocToLbs slawPtr byteLen

mallocToLbs :: Ptr () -> Int64 -> IO L.ByteString
mallocToLbs slawPtr byteLen = do
  bs <- B.unsafePackMallocCStringLen ( castPtr slawPtr
                                     , fromIntegral byteLen
                                     )
  return $ L.fromStrict bs
