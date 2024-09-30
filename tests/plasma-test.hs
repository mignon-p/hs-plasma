{-|
Module      : Main
Description : test plasma package
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}

import Control.Monad
-- import qualified Data.ByteString.Lazy     as L
-- import Data.Char
-- import Data.Complex
import Data.Default.Class
-- import Data.Either
import Data.Int
-- import qualified Data.IntMap.Strict       as IM
-- import Data.List
-- import qualified Data.Map.Strict          as M
-- import qualified Data.Text                as T
-- import qualified Data.Vector              as V
-- import qualified Data.Vector.Storable     as S
-- import Data.Word
-- import Foreign.Storable
-- import Numeric.Half
-- import System.Directory
import System.Environment
-- import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck    as QC
import qualified Test.QuickCheck.Monadic  as QC

import Data.Slaw
-- import Data.Slaw.IO
import Data.Slaw.IO.Yaml
-- import Data.Slaw.Path
-- import Data.Slaw.Semantic
import Data.Slaw.Util
import System.Loam.Hash
import System.Loam.Rand

import Comprehensive
import PlasmaTestUtil
import SlawInstances ()

main :: IO ()
main = do
  setIfNotSet "TASTY_COLOR" "always"
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

setIfNotSet :: String -> String -> IO ()
setIfNotSet var val = do
  old <- lookupEnv var
  case old of
    Just (_:_) -> return ()
    _          -> setEnv var val

qcProps :: TestTree
qcProps = testGroup "QuickCheck tests"
  [ QC.testProperty "round-trip IO (yaml)"     $ rtIoProp
  , QC.testProperty "round-trip (yaml string)" $ rtStrProp
  , QC.testProperty "randInt32 bounds"         $ randInt32Prop
  , QC.testProperty "randFloat64 bounds"       $ randFloat64Prop
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
  [ testCase "YAML slaw IO"               $ testSlawIO
  , testCase "cityHash64"                 $ testCityHash64
  , testCase "hash functions"             $ testHash
  ]

rtIoProp :: Slaw -> QC.Property
rtIoProp s = QC.monadicIO $ do
  roundTripIOwr fpQC [s] def False

rtStrProp :: Slaw -> QC.Property
rtStrProp s = Right [s] QC.=== roundTripYamlStr [s]

randInt32Prop :: (Int, Int32, Int32) -> QC.Property
randInt32Prop = randProp randInt32

randFloat64Prop :: (Int, Double, Double) -> QC.Property
randFloat64Prop = randProp randFloat64

randProp
  :: (Ord a, Show a)
  => (a -> a -> RandState -> IO a)
  -> (Int, a, a)
  -> QC.Property
randProp randFunc (seed, x, y) = QC.monadicIO $ do
  let (lo, hi) = if (x < y) then (x, y) else (y, x)
  when (lo < hi) $ do
    rs <- QC.run $ newRandState "test" (Just seed)
    forM_ [1 .. (100 :: Int)] $ \_ -> do
      n <- QC.run $ randFunc lo hi rs
      let msg = concat [ "seed="
                       , show seed
                       , ", lo="
                       , show lo
                       , ", hi="
                       , show hi
                       , ", n="
                       , show n
                       ]
      QC.assertWith (lo <= n) msg
      QC.assertWith (n  < hi) msg

testSlawIO :: Assertion
testSlawIO = do
  let pairs = [ ( def { wyoComment    = Just com
                      , wyoDirectives = Just dir
                      }
                , useName
                )
              | com     <- [minBound..maxBound]
              , dir     <- [minBound..maxBound]
              , useName <- [minBound..maxBound]
              ]
  forM_ pairs $ \(wyo, useName) -> do
    let slawx = [š wyo, SlawNil, š (5 :: Int64), comprehensiveProtein]
    roundTripIOwr fpHU slawx wyo useName

  let testFiles = [ "arf-coord"
                  , "example"
                  , "kp_enter"
                  , "protein"
                  ]
      testDir   = "test-files/"

  forM_ testFiles $ \fn -> do
    checkSlawRead2 (testDir ++ fn ++ ".yaml") (testDir ++ fn ++ ".slaw")

testCityHash64 :: Assertion
testCityHash64 = do
  0x095934e7f55b39ba @=? cityHash64 "!"
  0x3f8d4bbcea903711 @=? cityHash64 "descrips"
  0x16a60446ba1f5119 @=? cityHash64 "ingests"
  0xf5cb44dedc2dd3d4 @=? cityHash64 "rude_data"
  0x32b9c7ec210c1a04 @=? cityHash64 "tag:oblong.com,2009:slaw/array"
  0xa71e08969f161fb7 @=? cityHash64 "tag:oblong.com,2009:slaw/badutf8"
  0x2195c54d76476112 @=? cityHash64 "tag:oblong.com,2009:slaw/complex"
  0xf7b4a5d51438b792 @=? cityHash64 "tag:oblong.com,2009:slaw/cons"
  0x479db3b52758c108 @=? cityHash64 "tag:oblong.com,2009:slaw/f32"
  0xcd876fe462578c4f @=? cityHash64 "tag:oblong.com,2009:slaw/f64"
  0xa8024548a1ca2305 @=? cityHash64 "tag:oblong.com,2009:slaw/i16"
  0xa489c4c31b630c5a @=? cityHash64 "tag:oblong.com,2009:slaw/i32"
  0xc940065b925d3a15 @=? cityHash64 "tag:oblong.com,2009:slaw/i64"
  0xa266b0470338e398 @=? cityHash64 "tag:oblong.com,2009:slaw/i8"
  0x5b6eb1410fe7bd04 @=? cityHash64 "tag:oblong.com,2009:slaw/multivector"
  0xd70c69f2b823fb40 @=? cityHash64 "tag:oblong.com,2009:slaw/nonstd"
  0xdf8a6b35e9acd602 @=? cityHash64 "tag:oblong.com,2009:slaw/protein"
  0x2cdc5570e3b4f4c0 @=? cityHash64 "tag:oblong.com,2009:slaw/u16"
  0xa1d53e6f55db8ac8 @=? cityHash64 "tag:oblong.com,2009:slaw/u32"
  0x2c67cbce14289d59 @=? cityHash64 "tag:oblong.com,2009:slaw/u64"
  0x9cdaf818f6756e1a @=? cityHash64 "tag:oblong.com,2009:slaw/u8"
  0x64fdc5a5ca6cd3c6 @=? cityHash64 "tag:oblong.com,2009:slaw/vector"
  0x5cafcc4621b348fa @=? cityHash64 "tag:yaml.org,2002:binary"
  0xb6492c832b82f189 @=? cityHash64 "tag:yaml.org,2002:bool"
  0xb17678ff2f939aa2 @=? cityHash64 "tag:yaml.org,2002:map"
  0x9e34c34e89b60f38 @=? cityHash64 "tag:yaml.org,2002:null"
  0x42443404839a2550 @=? cityHash64 "tag:yaml.org,2002:omap"
  0xd7ec31d8a099ef3c @=? cityHash64 "tag:yaml.org,2002:seq"
  0x6e17023b1ba5ddbf @=? cityHash64 "tag:yaml.org,2002:str"
  0x2e7062203251384f @=? cityHash64 "tag:yaml.org,2002:string"

testHash :: Assertion
testHash = do
  let myBs  = toByteString $ toUtf8 ("ρρρ your boat" :: String)
      s32   = 0xC0DE_BABE
      s64   = 0xDEFACED_BAD_FACADE
      h64   = hashWord64          s64
      h32   = hashWord32          s32
      h64x  = hash2xWord64        s64 (fromIntegral s32)
      h32x  = hash2xWord32        s32 (fromIntegral s64)
      jh32  = jenkinsHash32       s32      myBs
      jh64  = jenkinsHash64       s64      myBs
      c64   = cityHash64                   myBs
      c64s  = cityHash64withSeed  s64      myBs
      c64s2 = cityHash64withSeeds s64 h64x myBs

  0x427ed0732b08bd3a @=? h64
  0xeaf30d40         @=? h32
  0x038b51a0cd958308 @=? h64x
  0x0fbac4b7         @=? h32x
  0x126691af         @=? jh32
  0x947092ff8e397ad3 @=? jh64
  0x833870d4da30a39e @=? c64
  0xcfda188d7ad9ec2c @=? c64s
  0xf2cf4e6d03f7137a @=? c64s2
