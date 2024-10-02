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
    --
  , isPoolPathValid
  , isPoolHostValid
  , isPoolUriValid
    --
  , ParsedPoolUri
  , PoolLocation
  , PoolAuthority
  , parsePoolUri
  , makePoolUri
  , isParsedPoolUriValid
  ) where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Short            as SBS
import Data.Char
import Data.Hashable
import Data.Int
import Data.String
import Data.Word
import GHC.Generics (Generic)
import System.IO.Unsafe
import Text.Printf

import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr    as C

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

instance PrintfArg PoolName where
  formatArg = formatString . toString

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

isPoolPathValid :: PoolName -> Bool
isPoolPathValid (PoolName sbs) = unsafePerformIO $ do
  C.useSBSAsConstCString sbs $ \namePtr -> do
    tort <- c_pool_validate_name namePtr
    return $ tort >= 0

data ParsedPoolUri = ParsedPoolUri
  { poolLocation :: Maybe PoolLocation
  , poolPath     :: !PoolName
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data PoolLocation = PoolLocation
  { poolScheme    :: !PoolName
  , poolAuthority :: Maybe PoolAuthority
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data PoolAuthority = PoolAuthority
  { poolHost :: !PoolName
  , poolPort :: Maybe Int
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

parsePoolUri :: PoolName -> ParsedPoolUri
parsePoolUri (PoolName sbs) =
  case A.parseOnly (poolNameP <* A.endOfInput) (SBS.fromShort sbs) of
    Left  _   -> ParsedPoolUri Nothing (PoolName sbs)
    Right ppn -> ppn

makePoolUri :: ParsedPoolUri -> PoolName
makePoolUri ppn =
  mconcat $ locParts (poolLocation ppn) ++ [poolPath ppn]

locParts :: Maybe PoolLocation -> [PoolName]
locParts Nothing = []
locParts (Just loc) =
  [poolScheme loc, ":"] ++ authParts (poolAuthority loc)

authParts :: Maybe PoolAuthority -> [PoolName]
authParts Nothing = []
authParts (Just auth) =
  ["//", poolHost auth] ++ portParts (poolPort auth) ++ ["/"]

portParts :: Maybe Int -> [PoolName]
portParts Nothing = []
portParts (Just port) = [":", fromString (show port)]

isAsciiAlnum :: Char -> Bool
isAsciiAlnum c = isAsciiLower c || isAsciiUpper c || isDigit c

isSchemeChar :: Char -> Bool
isSchemeChar '+' = True
isSchemeChar '.' = True
isSchemeChar '-' = True
isSchemeChar c   = isAsciiAlnum c

isHostChar :: Char -> Bool
isHostChar '.' = True
isHostChar c   = isHostComponentChar c

isHostComponentChar :: Char -> Bool
isHostComponentChar '-' = True
isHostComponentChar '_' = True
isHostComponentChar c   = isAsciiAlnum c

isV6Char :: Char -> Bool
isV6Char '[' = False
isV6Char ']' = False
isV6Char _   = True

bs2pn :: B.ByteString -> PoolName
bs2pn = PoolName . SBS.toShort

takeWhile2 :: (Char -> Bool) -> A.Parser B.ByteString
takeWhile2 predicate = do
  c    <- A.satisfy    predicate
  rest <- A.takeWhile1 predicate
  return $ c `B8.cons` rest

poolNameP :: A.Parser ParsedPoolUri
poolNameP = do
  -- Scheme must be at least two characters, because a Windows
  -- drive letter could look like a one-character scheme.
  scheme <- takeWhile2 isSchemeChar
  A.char ':'
  -- The "local" scheme never has an authority.
  auth <- if B8.map toLower scheme == "local"
          then return   Nothing
          else optional authorityP
  path <- A.option B.empty (A.char '/' >> A.takeByteString)
  let loc = PoolLocation
            { poolScheme    = bs2pn scheme
            , poolAuthority = auth
            }
      ppn = ParsedPoolUri
            { poolLocation = Just loc
            , poolPath     = bs2pn path
            }
  return ppn

authorityP :: A.Parser PoolAuthority
authorityP = do
  A.string "//"
  host <- hostnameP <|> ipv6P
  port <- optional portP
  let auth = PoolAuthority
             { poolHost = bs2pn host
             , poolPort = port
             }
  return auth

-- matches a DNS name or an IPv4 address
hostnameP :: A.Parser B.ByteString
hostnameP = A.takeWhile1 isHostChar

-- matches an IPv6 address (really, any string in square brackets)
ipv6P :: A.Parser B.ByteString
ipv6P = do
  A.char '['
  v6 <- A.takeWhile isV6Char
  A.char ']'
  return $ mconcat ["[", v6, "]"]

portP :: A.Parser Int
portP = A.char ':' >> A.decimal

isParsedPoolUriValid :: ParsedPoolUri -> Bool
isParsedPoolUriValid ppu = hostOk && pathOk
  where
    (hasAuth, hostOk) =
      case poolLocation ppu of
        Just (PoolLocation _ (Just (PoolAuthority h _))) ->
          (True, isPoolHostValid h)
        _ -> (False, True)
    path      = poolPath ppu
    pathEmpty = SBS.null $ unPoolName path
    -- If the URI has an authority, allow the path to be empty.
    -- For example, "tcp://chives.la923.oblong.net:1234/" or
    -- "tcp://example.com".  This does not name a pool, but it
    -- is a valid URI for use with pool_list_ex(), for example.
    pathOk    = (hasAuth && pathEmpty) || isPoolPathValid path

isPoolHostValid :: PoolName -> Bool
isPoolHostValid (PoolName sbs) =
  case A.parseOnly (validHostP <* A.endOfInput) (SBS.fromShort sbs) of
    Left  _ -> False
    Right _ -> True

isPoolUriValid :: PoolName -> Bool
isPoolUriValid = isParsedPoolUriValid . parsePoolUri

validHostP :: A.Parser ()
validHostP = validHostnameP <|> validIPv6P

validHostnameP :: A.Parser ()
validHostnameP = do
  A.takeWhile1 isHostComponentChar `A.sepBy1` A.char '.'
  return ()

validIPv6P :: A.Parser ()
validIPv6P = do
  A.char '['
  A.takeWhile isHexDigit `A.sepBy1` A.char ':'
  optional $ do
    -- An IPv6 "zone index" for non-global addresses
    A.char '%'
    -- Not exactly clear what characters are legal in a "zone index"?
    A.takeWhile1 isHostChar
  A.char ']'
  return ()
