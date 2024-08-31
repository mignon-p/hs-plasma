{-|
Module      : System.Loam.Internal.Filename
Description : Overloading for FilePath and OsPath
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module System.Loam.Internal.Filename
  ( Filename(..)
  , fnAsErl
  ) where

import Data.Bits
import qualified Data.ByteString          as B
import qualified Data.ByteString.Builder  as R
import qualified Data.ByteString.Lazy     as L
import Data.Char
import Data.Default.Class
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
-- import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import Data.Word
import qualified System.OsPath            as O

import Data.Slaw
import Data.Slaw.Internal (FileClass(..))

class FileClass a => Filename a where
  to8bitFn   :: a -> B.ByteString
  from8bitFn :: B.ByteString -> a

instance Filename FilePath where
  to8bitFn   = T.encodeUtf8 . T.pack
  from8bitFn = T.unpack . T.decodeUtf8Lenient

instance Filename O.OsPath where
  to8bitFn   = if useUTF16 then utf16To8bitFn   else utf8To8bitFn
  from8bitFn = if useUTF16 then utf16From8bitFn else utf8From8bitFn

useUTF16 :: Bool
useUTF16 = maxOsc > 0xff
  where maxOsc = ord $ O.toChar $ O.unsafeFromChar $ chr 0xffff

ospToInts   :: O.OsPath -> [Int]
ospToInts = map (ord . O.toChar) . O.unpack

ospFromInts :: [Int] -> O.OsPath
ospFromInts = O.pack . map (O.unsafeFromChar . chr)

utf8To8bitFn :: O.OsPath -> B.ByteString
utf8To8bitFn = B.pack . map fromIntegral . ospToInts

utf8From8bitFn :: B.ByteString -> O.OsPath
utf8From8bitFn = ospFromInts . map fromIntegral . B.unpack

utf16To8bitFn :: O.OsPath -> B.ByteString
utf16To8bitFn osp = L.toStrict lbs8
  where
    bldr  = mconcat $ map (R.word16LE . fromIntegral) $ ospToInts osp
    lbs16 = R.toLazyByteString   bldr
    txt   = LT.decodeUtf16LEWith T.lenientDecode lbs16
    lbs8  = LT.encodeUtf8        txt

utf16From8bitFn :: B.ByteString -> O.OsPath
utf16From8bitFn bs8 = ospFromInts $ map fromIntegral int16s
  where
    txt    = T.decodeUtf8Lenient bs8
    bs16   = T.encodeUtf16LE     txt
    int16s = le8to16 $ B.unpack  bs16

le8to16 :: [Word8] -> [Word16]
le8to16 (lo:hi:rest) = w16 : le8to16 rest
  where w16 = fromIntegral lo .|. (fromIntegral hi `shiftL` 8)
le8to16 _ = []

fnAsErl :: FileClass a => a -> ErrLocation
fnAsErl fn = def { elSource = DsFile (fcName fn) }
