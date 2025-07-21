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
-- import Data.Bifunctor
import qualified Data.ByteString.Builder  as R
-- import qualified Data.ByteString.Lazy     as L
-- import Data.Char
-- import Data.Complex
import Data.Default.Class
-- import Data.Either
import Data.Int
-- import qualified Data.IntMap.Strict       as IM
import Data.List
import qualified Data.Map.Strict          as M
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
import qualified Test.QuickCheck.Monadic  as QC
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck    as QC

import Data.Slaw
-- import Data.Slaw.IO
import Data.Slaw.IO.Yaml
-- import Data.Slaw.Path
-- import Data.Slaw.Semantic
import Data.Slaw.Util
import System.Loam.File
import System.Loam.Hash
import System.Loam.Log
import System.Loam.Rand
-- import System.Loam.Retorts.Constants
import System.Loam.Time
import System.Plasma.Pool

import Comprehensive
import GangTest
import LogTest
import OtherInstances ()
import PlasmaTestUtil
import PoolTestFixture
import SlawInstances ()
import TimeTestData

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
  , QC.testProperty "WriteYamlOptions Merge"   $ wyoMergeProp
  , QC.testProperty "PoolCreateOptions Merge"  $ pcoMergeProp
  , QC.testProperty "WriteYamlOptions Merge id"  $ wyoMergeIdProp
  , QC.testProperty "PoolCreateOptions Merge id" $ pcoMergeIdProp
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
  [ testCase "YAML slaw IO"               $ testSlawIO
  , testCase "cityHash64"                 $ testCityHash64
  , testCase "hash functions"             $ testHash
  , testCase "hash functions, large data" $ testLargeHash
  , testCase "Merge typeclass"            $ testMerge
  , testCase "time functions"             $ testTime
  , testCase "pool name validation"       $ testPoolName
  , testCase "listPools"                  $ testListPools
  , testCase "fetch"                      $ testFetch
  , testCase "advanceOldest"              $ testAdvanceOldest
  , testCase "probe"                      $ testProbe
  , testCase "seekToTime"                 $ testSeekToTime
  , testCase "pool exists/in use"         $ testExists
  , testCase "changeOptions"              $ testOptions
  , testCase "+/ operator"                $ testPlusSlash
  , testCase "gang membership"            $ testGangMembership
  , testCase "nextMulti"                  $ testNextMulti
  , logTests
  ]

logTests :: TestTree
logTests = testGroup "log tests" $ map mkLogCase cases
  where
    cases     = concat [ [ ("no flags",               []      ) ]
                       , map mkLogPair                allFlags
                       , [ ("all of the above flags", allFlags) ]
                       , map mkLogPair                moreFlags
                       ]
    allFlags  = [ FlgShowTime
                , FlgShowWhere
                , FlgShowCode
                , FlgShowPid
                , FlgShowProg
                , FlgShowTid
                ]
    moreFlags = [ FlgShowCodeOrWhere
                , FlgShowWhereFull
                ]

mkLogCase :: (TestName, [LogFlag]) -> TestTree
mkLogCase (name, flags) = testCase name (testLog flags)

mkLogPair :: LogFlag -> (TestName, [LogFlag])
mkLogPair flag = (show flag, [flag])

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

mergeProp
  :: (QC.Arbitrary a, Merge a, Eq a, Show a)
  => (a, a)
  -> QC.Property
mergeProp (x, y) = (x `prefLeft` y) QC.=== (y `prefRight` x)

mergeIdentityProp
  :: (QC.Arbitrary a, Merge a, Eq a, Show a, Default a)
  => (a, Bool, Word)
  -> QC.Property
mergeIdentityProp (x, b, w) =
  let op = if b then prefLeft else prefRight
  in case w `mod` 3 of
       0 -> x QC.=== (x   `op` x)
       1 -> x QC.=== (x   `op` def)
       _ -> x QC.=== (def `op` x)

wyoMergeProp :: (WriteYamlOptions, WriteYamlOptions) -> QC.Property
wyoMergeProp = mergeProp

pcoMergeProp :: (PoolCreateOptions, PoolCreateOptions) -> QC.Property
pcoMergeProp = mergeProp

wyoMergeIdProp :: (WriteYamlOptions, Bool, Word) -> QC.Property
wyoMergeIdProp = mergeIdentityProp

pcoMergeIdProp :: (PoolCreateOptions, Bool, Word) -> QC.Property
pcoMergeIdProp = mergeIdentityProp

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

testLargeHash :: Assertion
testLargeHash = do
  let bldr  = mconcat $ map R.word16BE [0, 17 .. 0xffff]
      myBs  = toByteString $ R.toLazyByteString bldr
      s32   = 0xC0DE_BABE
      s64   = 0xDEFACED_BAD_FACADE
      s64b  = 0x0123456789ABCDEF
      jh32  = jenkinsHash32       s32      myBs
      jh64  = jenkinsHash64       s64      myBs
      c64   = cityHash64                   myBs
      c64s  = cityHash64withSeed  s64      myBs
      c64s2 = cityHash64withSeeds s64 s64b myBs

  2946089748           @=? jh32
  15409861163017311376 @=? jh64
  12838808716745102289 @=? c64
  16082252722761465086 @=? c64s
  17686391379370221157 @=? c64s2

testMerge :: Assertion
testMerge = do
  let (keratin : hemoglobin : _) = tstProteins
      pLeft                      = ŝ (keratin `prefLeft`  hemoglobin)
      pRight                     = ŝ (keratin `prefRight` hemoglobin)

  ["test", "keratin", "hemoglobin"] @=? pDescrips             pLeft
  ["test", "keratin", "hemoglobin"] @=? pDescrips             pRight
  [("n", š (1 :: Word8))]           @=? (M.toList . pIngests) pLeft
  [("n", š (2 :: Word8))]           @=? (M.toList . pIngests) pRight
  "keratin"                         @=? pRudeData             pLeft
  "hemoglobin"                      @=? pRudeData             pRight

testTime :: Assertion
testTime = do
  forM_ timeTestData $ \testData -> do
    let expected = case testData of
                     GoodTime _ x    -> Right x
                     BadTime  _ tort -> Left $ Just tort
        inStr    = ttIn testData
        pfx      = "   input: " ++ show inStr
    eth    <- try $ parseTime inStr
    actual <- case eth of
                Left pe -> return $ Left $ peRetort pe
                Right t -> Right <$> formatTime t
    assertEqual pfx expected actual

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

testFetch :: Assertion
testFetch = do
  (depProts, (frs, idxPair)) <- poolTestFixture $ \hose deps -> do
    forM_ (zip deps [0..]) $ \(rp, expectedIdx) -> do
      expectedIdx @=? rpIndex rp
    fPair <- fetch hose False tstFops
    return (deps, fPair)

  Just (0, 4) @=? idxPair
  7           @=? length frs

  let frs' = catMaybes frs
  7           @=? length frs'

  forM_ (zip frs' (cycle depProts)) $ \(fr, depProt) -> do
    let idx = rpIndex     depProt
        ts  = rpTimestamp depProt
    idx    @=? frIdx         fr
    ts     @=? frTimestamp   fr
    Just 2 @=? frNumDescrips fr
    Just 1 @=? frNumIngests  fr

  forM_ (zip frs' expProteins) $ \(fr, expected) -> do
    expected @=? frProtein fr

testAdvanceOldest :: Assertion
testAdvanceOldest = do
  (depProts, (frs, idxPair)) <- poolTestFixture $ \hose deps -> do
    forM_ (zip deps [0..]) $ \(rp, expectedIdx) -> do
      expectedIdx @=? rpIndex rp

    erased1 <- advanceOldest hose 3
    fPair   <- fetch hose False tstFops
    erased2 <- advanceOldest hose 3

    True  @=? erased1
    False @=? erased2

    return (deps, fPair)

  Just (3, 4) @=? idxPair
  7           @=? length frs

  let frs' = catMaybes frs
  2           @=? length frs'

  let depProts'    = take 2 $ drop 3 depProts
      expProteins' = take 2 $ drop 3 expProteins

  forM_ (zip frs' depProts') $ \(fr, depProt) -> do
    let idx = rpIndex     depProt
        ts  = rpTimestamp depProt
    idx    @=? frIdx         fr
    ts     @=? frTimestamp   fr
    Just 2 @=? frNumDescrips fr
    Just 1 @=? frNumIngests  fr

  forM_ (zip frs' expProteins') $ \(fr, expected) -> do
    expected @=? frProtein fr

testProbe :: Assertion
testProbe = do
  poolTestFixture $ \hose deps -> do
    let [_, dep2, _, dep4, _] = deps
        p2 = tstProteins !! 1
        p4 = tstProteins !! 3

    cIdx0 <- currIndex hose
    0 @=? cIdx0

    rp4 <- probeFrwd hose "insulin"
    3   @=? rpIndex   rp4
    p4  @=? rpProtein rp4
    rp4 @=? dep4

    cIdx4 <- currIndex hose
    4 @=? cIdx4

    rp2 <- probeBack hose $ SlawList ["test", "hemoglobin"]
    1   @=? rpIndex   rp2
    p2  @=? rpProtein rp2
    rp2 @=? dep2

    cIdx1 <- currIndex hose
    1 @=? cIdx1

interpolateTime
  :: PoolTimestamp
  -> PoolTimestamp
  -> Double
  -> PoolTimestamp
interpolateTime x y frac =
  let diff = y - x
  in x + diff * frac

type SeekTimeTriple = (PoolTimestamp, TimeComparison, PoolIndex)

checkSeekTime
  :: HasCallStack
  => Hose
  -> SeekTimeTriple
  -> IO ()
checkSeekTime hose (ts, tc, expectedIdx) = do
  seekToTime hose ts tc
  actualIdx <- currIndex hose
  expectedIdx @=? actualIdx

myShuffle :: Int -> [a] -> IO [a]
myShuffle seed xs = do
  r <- newRandState "" (Just seed)
  pairs <- forM xs $ \x -> do
    n <- randWord64 r
    return (n, x)
  return $ map snd $ sortOn fst pairs

testSeekToTime :: Assertion
testSeekToTime = do
  poolTestFixture $ \hose deps -> do
    let [_, dep2, dep3, dep4, _] = deps
        ts2     = rpTimestamp dep2
        ts2a    = interpolateTime ts2 ts3 0.3
        ts2b    = interpolateTime ts2 ts3 0.7
        ts3     = rpTimestamp dep3
        ts3a    = interpolateTime ts3 ts4 0.3
        ts3b    = interpolateTime ts3 ts4 0.7
        ts4     = rpTimestamp dep4
        idx2    = 1
        idx3    = 2
        idx4    = 3
        triples = [ (ts2a, ClosestLower,  idx2)
                  , (ts2b, ClosestLower,  idx2)
                  , (ts3a, ClosestLower,  idx3)
                  , (ts3b, ClosestLower,  idx3)
                  , (ts2a, ClosestHigher, idx3)
                  , (ts2b, ClosestHigher, idx3)
                  , (ts3a, ClosestHigher, idx4)
                  , (ts3b, ClosestHigher, idx4)
                  , (ts2a, Closest,       idx2)
                  , (ts2b, Closest,       idx3)
                  , (ts3a, Closest,       idx3)
                  , (ts3b, Closest,       idx4)
                  ]

    triples' <- myShuffle 1993 triples
    mapM_ (checkSeekTime hose) triples'

testExists :: Assertion
testExists = do
  let pool = "salt water"

  withHoseCreatingly def "testExists" pool small $ \_ -> do
    ex1 <- doesPoolExist def pool
    iu1 <- isPoolInUse   def pool

    True @=? ex1
    True @=? iu1

  ex2 <- doesPoolExist def pool
  iu2 <- isPoolInUse   def pool

  True  @=? ex2
  False @=? iu2

  dispose def pool

  ex3 <- doesPoolExist def pool
  False @=? ex3

testOptions :: Assertion
testOptions = do
  poolTestFixture $ \hose _ -> do
    pi1 <- getInfo hose Nothing
    Just True  @=? piTerminal pi1
    Just kMmap @=? piType     pi1
    Just False @=? piFrozen   pi1

    let newOpts = def { pcoFrozen = Just True }
    changeOptions hose newOpts

    pi2 <- getInfo hose Nothing
    Just True  @=? piFrozen   pi2

testPlusSlash :: Assertion
testPlusSlash = do
  "foo/bar"      @=? "foo"  +/ "bar"
  "foo/bar"      @=? "foo/" +/ "bar"
  "/bar"         @=? "foo"  +/ "/bar"
  "foo/foo/bar"  @=? "foo"  +/ "foo/bar"
  "foo:/bar"     @=? "foo"  +/ "foo:/bar"
  "foo/foo/:bar" @=? "foo"  +/ "foo/:bar"
  "foo/"         @=? "foo"  +/ ""
  "bar"          @=? ""     +/ "bar"
  "foo:bar"      @=? "foo:" +/ "bar"
  ":bar"         @=? "foo:" +/ ":bar"
