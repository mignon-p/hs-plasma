{-|
Module      : System.Plasma.Pool.Internal.PoolName
Description : The naming of pools
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Plasma.Pool.Internal.PoolName
  ( PoolName(..)
  , toPoolName
  , fromPoolName
  , (+/)
  , isPoolNameValid
  ) where

import Control.DeepSeq
import qualified Data.ByteString          as B
import qualified Data.ByteString.Short    as SBS
import Data.Char
import Data.Hashable
import Data.Int
import Data.String
import Data.Word
import System.IO.Unsafe

import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C

infixr 5 +/

foreign import capi safe "libPlasma/c/pool.h pool_validate_name"
    c_pool_validate_name :: C.ConstCString -> IO Int64

newtype PoolName = PoolName { unPoolName :: SBS.ShortByteString }
                 deriving newtype (Eq, Ord, Show, Monoid, Semigroup,
                                   NFData, Hashable)

instance ByteStringClass PoolName where
  toByteString        = toByteString     . unPoolName
  toLazyByteString    = toLazyByteString . unPoolName
  toShortByteString   = unPoolName
  toWord8s            = SBS.unpack . unPoolName

  fromByteString      = fromWord8s . toWord8s
  fromLazyByteString  = fromWord8s . toWord8s
  fromShortByteString = PoolName . SBS.map clampByte
  fromWord8s          = PoolName . SBS.pack . map clampByte

instance IsString PoolName where
  fromString   = PoolName . SBS.pack . clampString

instance TextClass PoolName where
  toString     = map (chr . fromIntegral) . SBS.unpack . unPoolName
  toText       = toText     . toString
  toLazyText   = toLazyText . toString
  toUtf8       = toLazyByteString

  fromText     = fromString . toString
  fromLazyText = fromString . toString
  fromUtf8     = fromLazyByteString

clampString :: String -> [Word8]
clampString = map (clampByte . ord)

{-# SPECIALIZE clampByte :: Word8 -> Word8 #-}
{-# SPECIALIZE clampByte :: Int   -> Word8 #-}
clampByte :: Integral a => a -> Word8
clampByte n
  | n >= 0x20 && n < 0x7f = fromIntegral n
  | otherwise             = 0x3f

toPoolName :: TextClass a => a -> PoolName
toPoolName = fromString . toString

fromPoolName :: TextClass a => PoolName -> a
fromPoolName = fromString . toString

(+/) :: PoolName -> PoolName -> PoolName
x +/ y = mconcat [noTrailSlash x, "/", noLeadSlash y]

isSlash :: Word8 -> Bool
isSlash = (== 0x2f)

noTrailSlash :: PoolName -> PoolName
noTrailSlash name@(PoolName sbs)
  | "/" `SBS.isSuffixOf` sbs =
      PoolName . SBS.toShort $ B.dropWhileEnd isSlash $ SBS.fromShort sbs
  | otherwise = name

noLeadSlash :: PoolName -> PoolName
noLeadSlash name@(PoolName sbs)
  | "/" `SBS.isPrefixOf` sbs =
      PoolName . SBS.toShort $ B.dropWhile isSlash $ SBS.fromShort sbs
  | otherwise = name

isPoolNameValid :: PoolName -> Bool
isPoolNameValid (PoolName sbs) = unsafePerformIO $ do
  C.useSBSAsConstCString sbs $ \namePtr -> do
    tort <- c_pool_validate_name namePtr
    return $ tort >= 0
