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

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy     as L
-- import Data.Char
-- import Data.Complex
import Data.Default.Class
import Data.Either
import Data.Int
-- import qualified Data.IntMap.Strict       as IM
-- import Data.List
-- import qualified Data.Map.Strict          as M
import Data.Maybe
import qualified Data.Set                 as S
-- import qualified Data.Text                as T
-- import qualified Data.Vector              as V
-- import qualified Data.Vector.Storable     as S
import Data.Word
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
import System.Loam.File
import System.Loam.Hash
import System.Loam.Rand
import System.Loam.Retorts.Constants
import System.Plasma.Pool

import Comprehensive
import OtherInstances ()
import PlasmaTestUtil
import SlawInstances ()

main :: IO ()
main = withTempDir "pool-" $ \dir -> do
  setEnv      "OB_POOLS_DIR" dir
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
  , QC.testProperty "randWord32 repeatability" $ randRep32Prop
  , QC.testProperty "randWord64 repeatability" $ randRep64Prop
  , QC.testProperty "ParsedPoolUri round-trip" $ poolUriProp
  , QC.testProperty "ParsedPoolUri validity"   $ uriValidityProp
  , QC.testProperty "ContextOptions"           $ ctxOptsProp
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
  [ testCase "YAML slaw IO"               $ testSlawIO
  , testCase "cityHash64"                 $ testCityHash64
  , testCase "hash functions"             $ testHash
  , testCase "pool name validation"       $ testPoolName
  , testCase "listPools"                  $ testListPools
  , testCase "fetch"                      $ testFetch
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
    forM_ [1 .. (100 :: Int)] $ \i -> do
      n <- QC.run $ randFunc lo hi rs
      let msg = concat [ "seed="
                       , show seed
                       , ", i="
                       , show i
                       , ", lo="
                       , show lo
                       , ", hi="
                       , show hi
                       , ", n="
                       , show n
                       ]
      QC.assertWith (lo <= n) msg
      QC.assertWith (n  < hi) msg

randRep32Prop :: Int -> QC.Property
randRep32Prop = randRepeatableProp randWord32

randRep64Prop :: Int -> QC.Property
randRep64Prop = randRepeatableProp randWord64

randRepeatableProp
  :: (Eq a, Show a)
  => (RandState -> IO a)
  -> Int
  -> QC.Property
randRepeatableProp randFunc seed = QC.monadicIO $ do
  rs1 <- QC.run $ newRandState "test1" (Just seed)
  rs2 <- QC.run $ newRandState "test2" (Just seed)
  forM_ [1 .. (100 :: Int)] $ \i -> do
    n1 <- QC.run $ randFunc rs1
    n2 <- QC.run $ randFunc rs2
    let msg = concat [ "seed="
                     , show seed
                     , ", i="
                     , show i
                     , ", n1="
                     , show n1
                     , ", n2="
                     , show n2
                     ]
    QC.assertWith (n1 == n2) msg

poolUriProp :: ParsedPoolUri -> QC.Property
poolUriProp ppu = ppu QC.=== parsePoolUri (makePoolUri ppu)

uriValidityProp :: ParsedPoolUri -> QC.Property
uriValidityProp ppu = True QC.=== isParsedPoolUriValid ppu

ctxOptsProp :: ContextOptions -> QC.Property
ctxOptsProp opts = opts QC.=== roundTripCtxOpts opts

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

testPoolName :: Assertion
testPoolName = do
  let both x = (isPoolPathValid x, isPoolUriValid x)

  (False, False) @=? both "?bad?"
  (True , True ) @=? both "hello"
  (True , True ) @=? both "Fully-Automated Luxury Gay Space Communism"
  (False, False) @=? both "badness$"
  (False, False) @=? both "LPT7.foo"
  (False, False) @=? both "pigs-in "
  (True , True ) @=? both " the-final-frontier"
  (True , True ) @=? both "my_pool"
  (True , True ) @=? both "pipeline/gripes"
  (False, True ) @=? both "tcp://localhost/my_pool"
  (False, True ) @=? both "tcp://mango:10000/my_pool"
  (False, True ) @=? both "tcpo://foo-bar.example.com/some/pool"
  (False, True ) @=? both "tcps://18.85.8.220:1234/a/pool"
  (False, True ) @=? both "tcp://[::1]/a pool"
  (False, True ) @=? both "tcp://[fe80::dead:beef]/foo"
  (False, True ) @=? both "tcp://[fe80::dead:beef]/"
  (False, True ) @=? both "tcp://[fe80::dead:beef]"
  (False, True ) @=? both "tcp://[fe80::1ff:fe23:4567:890a%eth2]/pool"
  (False, True ) @=? both "tcp://chives.la923.oblong.net:1234/"
  (False, True ) @=? both "tcp://[2001:db8::dead:beef:bad:f00d]:5678/C++"
  (False, False) @=? both "tcp://[192.0.2.123]/foo"
  (False, False) @=? both "tcp://[2001:db8::baad%en0:1234]/bar"
  (False, False) @=? both "tcp://[blech]/stuff"
  (False, False) @=? both "local://example.com/foo"
  (True , True ) @=? both "local:/var/some/where"

mkPoolNameSet :: [PoolName] -> S.Set PoolName
mkPoolNameSet = S.fromList

testListPools :: Assertion
testListPools = do
  let fullNames  = mkPoolNameSet [ "a/man/a/plan/a/canal/panama"
                                 , "a/man/a/plan/a/canal+/company"
                                 ]
      shortNames = mkPoolNameSet [ "canal/panama"
                                 , "canal+/company"
                                 ]

  forM_ (S.toList fullNames) $ \pool -> do
    create def pool small

  pnames0 <- listPools def Nothing
  fullNames @=? mkPoolNameSet pnames0

  pnames1 <- listPools def (Just "a/man/a/plan/a")
  shortNames @=? mkPoolNameSet pnames1

  forM_ (S.toList fullNames) (dispose def)

  pnames2 <- listPools def Nothing
  [] @=? pnames2

  pnames3 <- listPools def (Just "a/man/a/plan/a")
  [] @=? pnames3

mkProt :: L.ByteString -> Word8 -> Slaw
mkProt name num = SlawProtein (Just des) (Just ing) name
  where
    des = SlawList ["test", SlawString name]
    ing = SlawMap  [("n", š num)]

noDes :: Slaw -> Slaw
noDes p = p { slawDescrips = Nothing }

noIng :: Slaw -> Slaw
noIng p = p { slawIngests  = Nothing }

trimRude :: Slaw -> Int64 -> Int64 -> Slaw
trimRude p start len = p { slawRudeData = rude }
  where rude = L.take len $ L.drop start $ slawRudeData p

testFetch :: Assertion
testFetch = do
  let p1    = mkProt "keratin"    1
      p2    = mkProt "hemoglobin" 2
      p3    = mkProt "casein"     3
      p4    = mkProt "insulin"    4
      p5    = mkProt "lactase"    5
      prots = [p1, p2, p3, p4, p5]
      pool  = "testFetch"

  hose  <- participateCreatingly def "testFetch" pool small
  pairs <- forM (zip prots [0..]) $ \(p, expectedIdx) -> do
    (actualIdx, ts) <- deposit hose p
    expectedIdx @=? actualIdx
    return (actualIdx, ts)

  let fops = [ def { foIdx          = 0 }
             , def { foIdx          = 1
                   , foRudeOffset   = Nothing
                   }
             , def { foIdx          = 2
                   , foWantDescrips = False
                   }
             , def { foIdx          = 3
                   , foWantIngests  = False
                   }
             , def { foIdx          = 4
                   , foRudeLength   = Just 4
                   }
             , def { foIdx          = 0
                   , foRudeOffset   = Just 1
                   }
             , def { foIdx          = 1
                   , foRudeOffset   = Just 2
                   , foRudeLength   = Just 5
                   }
             ]

  (frs, idxPair) <- fetch hose False fops

  withdraw hose
  dispose  def  pool

  Just (0, 4) @=? idxPair
  7           @=? length frs

  let frs' = catMaybes frs
  7           @=? length frs'

  forM_ (zip frs' (cycle pairs)) $ \(fr, (idx, ts)) -> do
    idx    @=? frIdx         fr
    ts     @=? frTimestamp   fr
    Just 2 @=? frNumDescrips fr
    Just 1 @=? frNumIngests  fr

  let [rp1, rp2, rp3, rp4, rp5, rp6, rp7] = map frProtein frs'

  p1              @=? rp1
  trimRude p2 0 0 @=? rp2
  noDes    p3     @=? rp3
  noIng    p4     @=? rp4
  trimRude p5 0 4 @=? rp5
  trimRude p1 1 6 @=? rp6
  trimRude p2 2 5 @=? rp7

  badness <- try $ currIndex hose
  [Just ZE_HS_ALREADY_CLOSED] @=? map peRetort (lefts [badness])
