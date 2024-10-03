{-|
Module      : OtherInstances
Description : Arbitrary instance for various types
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module OtherInstances where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as S
import Test.QuickCheck
import Text.Printf

-- import Data.Slaw.Util
import System.Plasma.Pool

forbiddenNames :: String
forbiddenNames = map toLower $
  "CON PRN AUX NUL COM1 COM2 COM3 COM4 COM5 COM6 COM7 COM8 " ++
  "COM9 LPT1 LPT2 LPT3 LPT4 LPT5 LPT6 LPT7 LPT8 LPT9 lost+found"

forbiddenNameSet :: S.Set String
forbiddenNameSet = S.fromList $ words forbiddenNames

nameMidChars :: String
nameMidChars = " !#$%&'()+,-.0123456789;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
               "[]^_`abcdefghijklmnopqrstuvwxyz{}~"

nameFirstChars :: String
nameFirstChars = nameMidChars \\ "."

nameLastChars :: String
nameLastChars = nameMidChars \\ ". $"

alnumChars :: String
alnumChars = concat [['0'..'9'], ['A'..'Z'], ['a'..'z']]

schemeChars :: String
schemeChars = "+.-" ++ alnumChars

hostChars :: String
hostChars = "-_" ++ alnumChars

zoneChars :: String
zoneChars = ".-_" ++ alnumChars

data ValidChars = ValidChars
  { vcFirst :: String
  , vcMid   :: String
  , vcLast  :: String
  }

vNameChars, vHostChars :: ValidChars
vNameChars   = ValidChars nameFirstChars nameMidChars nameLastChars
vHostChars   = ValidChars hostChars      hostChars    hostChars

genComponent :: ValidChars -> Int -> Gen String
genComponent vc n
  | n < 2 = do
      c <- elements $ vcLast vc
      return [c]
  | otherwise = do
      c1 <- elements $ vcFirst vc
      s  <- replicateM (n - 2) $ elements $ vcMid vc
      c2 <- elements $ vcLast vc
      return $ [c1] ++ s ++ [c2]

clamp :: (Int, Int) -> Int -> Int
clamp (lo, hi) x = hi `min` (x `max` lo)

genPathComponent :: Int -> Gen String
genPathComponent n = do
  str <- genComponent vNameChars n
  let pfx = map toLower $ takeWhile (/= '.') str
  case pfx `S.member` forbiddenNameSet of
    False -> return str
    True  -> genPathComponent n

genPath :: Gen PoolName
genPath = genName genPathComponent "/" 100

genName
  :: (Int -> Gen String)
  -> String
  -> Int
  -> Gen PoolName
genName genComp sep totalLen = do
  sz <- getSize
  let maxComps = clamp (1, 10) sz
  nComps <- chooseInt (1, maxComps)
  let budget = (totalLen `div` nComps) - 1
      maxLen = clamp (1, budget) sz
  comps <- replicateM nComps $ do
    len <- chooseInt (1, maxLen)
    genComp len
  return $ toPoolName $ intercalate sep comps

genScheme :: Gen PoolName
genScheme = do
  scheme <- toPoolName <$> genSimpleName (2, 15) schemeChars
  if scheme == kLocal
    then genScheme -- try again, to avoid "local" scheme
    else return scheme

genSimpleName :: (Int, Int) -> String -> Gen String
genSimpleName (lo, hi) chars = do
  sz   <- getSize
  let maxLen = clamp (lo, hi) sz
  len  <- chooseInt (lo, maxLen)
  replicateM len $ elements chars

genDNS :: Gen PoolName
genDNS = genName (genComponent vHostChars) "." 63

genIPv4 :: Gen PoolName
genIPv4 = do
  nums <- replicateM 4 $ chooseInt (0, 255)
  return $ toPoolName $ intercalate "." $ map show nums

fmtHex :: Int -> String
fmtHex = printf "%x"

genZone :: Gen String
genZone = ('%':) <$> genSimpleName (1, 15) zoneChars

genIPv6 :: Gen PoolName
genIPv6 = do
  sz <- getSize
  let maxLen = clamp (1, 8) sz
  len  <- chooseInt (1, maxLen)
  nums <- replicateM len $ chooseInt (0, 0xffff)
  let sep  = if len < 8 then "::" else ":"
      hLen = len `div` 2
      (nums1, nums2) = splitAt hLen $ map fmtHex nums
      str1 = intercalate ":" nums1
      str2 = intercalate ":" nums2
      addr = concat [str1, sep, str2]
  zone <- oneof [return "", genZone]
  return $ toPoolName $ concat ["[", addr, zone, "]"]

genHost :: Gen PoolName
genHost = oneof [genDNS, genIPv4, genIPv6]

genPort :: Gen (Maybe Int)
genPort = oneof
  [ return Nothing
  , Just <$> chooseInt (1, 65535)
  ]

instance Arbitrary PoolAuthority where
  arbitrary = PoolAuthority <$> genHost <*> genPort

instance Arbitrary PoolLocation where
  arbitrary = PoolLocation <$> genScheme <*> arbitrary

instance Arbitrary ParsedPoolUri where
  arbitrary = frequency
    [ (15, ParsedPoolUri <$> arbitrary <*> genPath)
    , (1,  mkLocalUri <$> genPath)
    ]
    where
      mkLocalUri path = ParsedPoolUri (Just localLoc) ("/" <> path)
      localLoc        = PoolLocation kLocal Nothing
