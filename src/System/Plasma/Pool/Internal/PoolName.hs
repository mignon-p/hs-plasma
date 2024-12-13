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
  , ParsedPoolUri(..)
  , PoolLocation(..)
  , PoolAuthority(..)
  , parsePoolUri
  , makePoolUri
  , isParsedPoolUriValid
    --
  , kLocal
  , kTcp
  , kTcpo
  , kTcps
    --
  , PoolIndex
  , erlFromPoolName
  , erlFromPoolIdx
  ) where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Short            as SBS
import Data.Char
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.String
import Data.Word
import GHC.Generics (Generic)
import System.IO.Unsafe
import Text.Printf

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr    as C

infixr 5 +/

foreign import capi safe "libPlasma/c/pool.h pool_validate_name"
    c_pool_validate_name :: C.ConstCString -> IO Int64

-- | Every protein in a pool has a unique, immutable, and sequential
-- serial number known as an “index”.
type PoolIndex = Int64

-- | A string type which represents the name of a pool.
-- Should only contain printable ASCII characters, 0x20 to 0x7E.
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

instance Nameable PoolName where
  typeName _ = "PoolName"

instance ToSlaw PoolName where
  toSlaw = SlawString . toUtf8

instance FromSlaw PoolName where
  fromSlaw (SlawString utf8) = Right $ fromUtf8 utf8
  fromSlaw s                 = handleOthers s

clampString :: String -> [Word8]
clampString = map (clampByte . ord)

{-# SPECIALIZE clampByte :: Word8 -> Word8 #-}
{-# SPECIALIZE clampByte :: Int   -> Word8 #-}
clampByte :: Integral a => a -> Word8
clampByte n
  | n >= 0x20 && n < 0x7f = fromIntegral n
  | otherwise             = 0x3f

-- | Convert a string from any instance of 'TextClass' into a
-- 'PoolName'.  Characters outside the range 0x20 to 0x7E are
-- changed to 0x3F (@?@).
toPoolName :: TextClass a => a -> PoolName
toPoolName = fromString . toString

-- | Convert a string from 'PoolName' into any instance of
-- 'TextClass'.
fromPoolName :: TextClass a => PoolName -> a
fromPoolName = fromString . toString

-- | Concatenate two path components of a pool URI.
-- Normally, a @/@ is automatically inserted between the two
-- components.  However, if the second component is an
-- absolute pool URI, then the second component is returned
-- unchanged, and the first component is ignored.
(+/) :: PoolName -> PoolName -> PoolName
x +/ y
  | isAbsolutePoolName y =                  y
  | needsSlash         x = mconcat [x, "/", y]
  | otherwise            = mconcat [x,      y]

isAbsolutePoolName :: PoolName -> Bool
isAbsolutePoolName (PoolName sbs)
  | SBS.null sbs            = False
  | SBS.null pfx            = True
  | ":" `SBS.isInfixOf` pfx = True
  | otherwise               = False
  where pfx = SBS.takeWhile (not . isSlash) sbs

isSlash :: Word8 -> Bool
isSlash = (== 0x2f)         -- '/'

needsSlash :: PoolName -> Bool
needsSlash (PoolName sbs) =
  case SBS.unsnoc sbs of
    Nothing        -> False
    Just (_, 0x2f) -> False -- '/'
    Just (_, 0x3a) -> False -- ':'
    _              -> True

-- | Determines whether a 'PoolName' represents a syntactically
-- valid pool name.  This wraps the C function
-- @pool_validate_name()@.
isPoolPathValid :: PoolName -> Bool
isPoolPathValid (PoolName sbs) = unsafePerformIO $ do
  C.useSBSAsConstCString sbs $ \namePtr -> do
    tort <- c_pool_validate_name namePtr
    return $ tort >= 0

-- | Represents a pool URI which has been separated into its
-- components.  The output of 'parsePoolUri', or the input to
-- 'makePoolUri'.
data ParsedPoolUri = ParsedPoolUri
  { -- | “scheme” and “authority” of the URI (optional)
    poolLocation :: Maybe PoolLocation
    -- | The path of the pool, on the specified server.
  , poolPath     :: !PoolName
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable ParsedPoolUri where
  typeName _ = "ParsedPoolUri"

-- | Represents a pool server.  (“scheme” and “authority”
-- in URL terminology)
data PoolLocation = PoolLocation
  { -- | For example: @local@, @tcp@, @tcpo@, or @tcps@
    poolScheme    :: !PoolName
    -- | The name and port of the server.
  , poolAuthority :: Maybe PoolAuthority
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable PoolLocation where
  typeName _ = "PoolLocation"

-- | The name and port of a pool server.
data PoolAuthority = PoolAuthority
  { -- | DNS name, or IPv4 or IPv6 address, of the server.
    -- If it is an IPv6 address, it must be enclosed in
    -- square brackets.
    poolHost :: !PoolName
    -- | Optional port number of the server.
  , poolPort :: Maybe Int
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable PoolAuthority where
  typeName _ = "PoolAuthority"

-- | Parse a 'PoolName' into its URI components, as a
-- 'ParsedPoolUri'.
parsePoolUri :: PoolName -> ParsedPoolUri
parsePoolUri (PoolName sbs) =
  case A.parseOnly (poolNameP <* A.endOfInput) (SBS.fromShort sbs) of
    Left  _   -> ParsedPoolUri Nothing (PoolName sbs)
    Right ppn -> ppn

-- | Convert a 'ParsedPoolUri' into a single string, as a
-- 'PoolName'.
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

{-
ciBsEq :: B.ByteString -> B.ByteString -> Bool
ciBsEq bs1 bs2 =
  if B.length bs1 == B.length bs2
  then B.map lcAscii8 bs1 == B.map lcAscii8 bs2
  else False
-}

isLocal :: PoolName -> Bool
isLocal = (== kLocal)

poolNameP :: A.Parser ParsedPoolUri
poolNameP = do
  -- Scheme must be at least two characters, because a Windows
  -- drive letter could look like a one-character scheme.
  scheme <- bs2pn <$> takeWhile2 isSchemeChar
  A.char ':'
  -- The "local" scheme never has an authority.
  auth <- if isLocal scheme
          then return   Nothing
          else optional authorityP
  path <- case auth of
            Nothing -> A.takeByteString
            Just _  -> A.option B.empty (A.char '/' >> A.takeByteString)
  let loc = PoolLocation
            { poolScheme    = scheme
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

-- | Determines whether a given 'ParsedPoolUri' represents a
-- syntactically valid pool URI.
isParsedPoolUriValid :: ParsedPoolUri -> Bool
isParsedPoolUriValid ppu = hostOk && pathOk
  where
    loc = poolLocation ppu
    (hasAuth, hostOk) =
      case loc of
        Just (PoolLocation _ (Just (PoolAuthority h _))) ->
          (True, isPoolHostValid h)
        _ -> (False, True)
    scheme =
      case loc of
        Just (PoolLocation sch _) -> sch
        _                         -> mempty
    uri       = makePoolUri ppu
    path      = poolPath ppu
    pathEmpty = SBS.null $ unPoolName path
    -- If the URI has an authority, allow the path to be empty.
    -- For example, "tcp://chives.la923.oblong.net:1234/" or
    -- "tcp://example.com".  This does not name a pool, but it
    -- is a valid URI for use with pool_list_ex(), for example.
    pathOk1   = (hasAuth && pathEmpty) || isPoolPathValid path
    -- For "local:" URIs, there must be a path, and there must
    -- not be an authority.  In order to validate the pool path,
    -- we must include the "local:" part when we call the C
    -- function (unlike all other pool URIs).
    pathOk2   = not hasAuth && not pathEmpty && isPoolPathValid uri
    pathOk    = if isLocal scheme then pathOk2 else pathOk1

-- | Determines whether a given string (as a 'PoolName')
-- represents a syntactically valid DNS name, or IPv4 or IPv6
-- address.
--
-- If it is an IPv6 address, it must be enclosed in
-- square brackets.
isPoolHostValid :: PoolName -> Bool
isPoolHostValid (PoolName sbs) =
  case A.parseOnly (validHostP <* A.endOfInput) (SBS.fromShort sbs) of
    Left  _ -> False
    Right _ -> True

-- | Determines whether a given 'PoolName' represents a
-- syntactically valid pool URI.
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

-- | The string @local@.
kLocal :: PoolName
kLocal = "local"

-- | The string @tcp@.
kTcp :: PoolName
kTcp   = "tcp"

-- | The string @tcpo@.
kTcpo :: PoolName
kTcpo  = "tcpo"

-- | The string @tcps@.
kTcps :: PoolName
kTcps  = "tcps"

erlFromPoolName :: PoolName -> ErrLocation
erlFromPoolName pname =
  def { elSource = DsPool (toString pname) Nothing }

erlFromPoolIdx :: PoolName -> PoolIndex -> ErrLocation
erlFromPoolIdx pname idx =
  def { elSource = DsPool (toString pname) (Just idx) }
