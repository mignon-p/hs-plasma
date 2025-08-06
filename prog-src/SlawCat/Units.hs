module SlawCat.Units
  ( Duration
  , formatNumBytes
  , formatDuration
  , parseDuration
  , durUnitsStr
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Numeric.Natural
import Text.Printf
import Text.Read

type Duration = Nano

-- formatNumBytes

data ByteFmt = ByteDecimal Rational Int
             | ByteBinary  Natural  Int
             deriving (Eq, Ord, Show)

decSuffixes, binSuffixes :: B8.ByteString
decSuffixes = " kMGTPEZYRQ"
binSuffixes = B8.map toUpper decSuffixes

maxExponent :: Int
maxExponent = B8.length decSuffixes - 1

formatNumBytes :: Integer -> String
formatNumBytes n
  | n < 0     = show n
  | otherwise =
      case fnb1 (fromInteger n) of
        ByteDecimal rat ex ->
          let sfx = makeSuffix decSuffixes "" ex
              dbl = fromRational rat :: Double
          in if denominator rat == 1
             then printf "%d %s" (numerator rat) sfx
             else printf "%.1f %s" dbl sfx
        ByteBinary nat ex ->
          let sfx = makeSuffix binSuffixes "i" ex
          in printf "%d %s" nat sfx

makeSuffix :: B8.ByteString -> String -> Int -> String
makeSuffix suffixes midChar ex
  | ex < 1    = lastChar
  | otherwise = firstChar : midChar ++ lastChar
    where
      firstChar = suffixes `B8.index` ex
      lastChar  = "B"

fnb1 :: Natural -> ByteFmt
fnb1 n =
  case tryBin n 0 of
    Just bf -> bf
    Nothing -> doDec (fromIntegral n) 0

tryBin :: Natural -> Int -> Maybe ByteFmt
tryBin !n !ex
  | ex >= maxExponent || q == 0 = Just $ ByteBinary n ex
  | r == 0                      = tryBin q (ex + 1)
  | otherwise                   = Nothing
  where (q, r) = n `divMod` 1024

doDec :: Rational -> Int -> ByteFmt
doDec !n !ex
  | ex >= maxExponent || n < 1000 = ByteDecimal n ex
  | otherwise                     = doDec (n / 1000) (ex + 1)

-- durationUnits

durationUnits :: [([String], Duration)]
durationUnits =
  [ (["ns"],    1e-9)
  , (microStrs, 1e-6)
  , (["ms"],    1e-3)
  , (["s"],        1)
  , (["m"],       60)
  , (["h"],     3600)
  , (["d"],    86400)
  ]

microStrs :: [String]
microStrs = map (:"s")
  [ chr 0x00b5  -- U+00B5 MICRO SIGN
  , chr 0x03bc  -- U+03BC GREEK SMALL LETTER MU
  , 'u'
  ]

-- formatDuration

formatDuration :: Duration -> String
formatDuration dur
  | dur < 1   = fmtDurSubSec dur durUnitsSubSec
  | otherwise = intercalate " " $ reverse $ fmtDurHMS dur durUnitsHMS

fmtDurHMS :: Duration -> [(String, Duration)] -> [String]
fmtDurHMS _ [] = []
fmtDurHMS dur ((unit, umag):rest) =
  let (q, r) = divModDur dur umag rest
      more   = fmtDurHMS q rest
  in if | r == 0             -> more
        | r < 1 && umag == 1 -> fmtDurSubSec r durUnitsSubSec : more
        | otherwise          -> (fmtDurNum r ++ unit)         : more

divModDur
  :: Duration
  -> Duration
  -> [(String, Duration)]
  -> (Duration, Duration)
divModDur dur _       []               = (0, dur)
divModDur dur prevMag ((_, umag):_) =
  let divisor = umag / prevMag
      (q, r)  = dur `divMod'` divisor
  in (fromInteger q, r)

fmtDurSubSec :: Duration -> [(String, Duration)] -> String
fmtDurSubSec dur [] = fmtDurNum dur ++ "s" -- shouldn't happen
fmtDurSubSec dur ((unit, umag):rest)
  | isGood dur rest = fmtDurNum (dur / umag) ++ unit
  | otherwise       = fmtDurSubSec dur rest

isGood :: Duration -> [(String, Duration)] -> Bool
isGood _   []            = True
isGood dur ((_, umag):_) = dur < umag

fmtDurNum :: Duration -> String
fmtDurNum dur =
  let rat = toRational dur
      dbl = fromRational rat :: Double
  in if denominator rat == 1
     then show (numerator rat)
     else printf "%f" dbl

durUnitsSubSec, durUnitsHMS :: [(String, Duration)]
durUnitsSubSec = mkFmtUnits (<  1)
durUnitsHMS    = mkFmtUnits (>= 1)

mkFmtUnits :: (Duration -> Bool) -> [(String, Duration)]
mkFmtUnits f = mapMaybe g durationUnits
  where
    g ([], _) = Nothing
    g ((unit:_), umag)
      | f umag    = Just (unit, umag)
      | otherwise = Nothing

-- parseDuration

parseDuration :: String -> Either String Duration
parseDuration = parseDur1 0 . filter (not . isSpace)

parseDur1 :: Duration -> String -> Either String Duration
parseDur1 !dur1 str = do
  let (digs, sfx)  = span  digChar str
      (unit, rest) = break digChar sfx
      digMsg       = concat [ "could not parse "
                            , show digs
                            , " as number"
                            ]
      unitMsg      = concat [ show unit
                            , " was not one of the recognized units: "
                            , durUnitsStr
                            ]
  n    <- m2e digMsg  $ readDuration digs
  mult <- m2e unitMsg $ unit `lookup` durUnitsP
  dur2 <- if (n < 0)
          then Left "duration cannot be negative"
          else Right $ n * mult
  let dur = dur1 + dur2
  if null rest
    then return dur
    else parseDur1 dur rest

readDuration :: String -> Maybe Duration
readDuration str = readMaybe str <|> fmap dbl2dur (readMaybe str)

dbl2dur :: Double -> Duration
dbl2dur = realToFrac

m2e :: String -> Maybe a -> Either String a
m2e msg Nothing  = Left  msg
m2e _   (Just x) = Right x

digChar :: Char -> Bool
digChar '.' = True
digChar 'e' = True
digChar 'E' = True
digChar '+' = True
digChar '-' = True
digChar c   = isDigit c

durUnitsP :: [(String, Duration)]
durUnitsP = ("", 1) : concatMap f durationUnits
  where f (xs, t) = map (,t) xs

durUnitsStr :: String
durUnitsStr =
  intercalate ", " $ concatMap (filter isAscStr . fst) durationUnits

isAscStr :: String -> Bool
isAscStr = all isAscii
