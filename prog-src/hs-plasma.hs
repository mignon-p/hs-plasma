import Control.Exception
import Control.Monad
import qualified Data.ByteString          as B
import Data.Default.Class
import Data.List
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import Data.Word
import Numeric.Half
import System.Environment (setEnv)
import System.IO
import Text.Printf

import Data.Slaw
import Data.Slaw.Extras
import Data.Slaw.IO
import Data.Slaw.IO.Yaml
import Data.Slaw.Util
import System.Loam.Dirs
import System.Loam.File
import System.Loam.Hash
import System.Loam.Log
import System.Loam.Rand
import System.Loam.Time
import System.Loam.Util
import System.Loam.Version
import System.Plasma.Pool

main :: IO ()
main = withTempDir "pool-" $ \dir -> do
  setEnv "OB_POOLS_DIR" dir
  main1

main1 :: IO ()
main1 = do
  w <- x86Features
  let s = printf "%-20s = %016x" ("features" :: String) w
  putStrLn s

  forM_ [minBound .. maxBound] $ \v -> do
    t <- getVersion v
    putStrLn $ printf "%-20s = %s" (show v) t

  forM_ [minBound .. maxBound] $ \si -> do
    i <- getSystemInfo si
    putStrLn $ printf "%-20s = %d" (show si) i

  putStrLn ""

  forM_ [minBound .. maxBound] $ \sd -> do
    fns <- splitStandardPath sd
    forM_ fns $ \fn -> do
      putStrLn $ printf "%-20s = %s" (show sd) (fn :: String)

  putStrLn ""

  forM_ [minBound .. maxBound] $ \sd -> do
    fns <- searchStandardPath sd "example" "c"
    forM_ fns $ \fn -> do
      putStrLn $ printf "%-20s = %s" (show sd) (fn :: String)

  putStrLn ""

  uuid     <- generateUuid
  userName <- getUserName
  progName <- getProgName
  setProgName "banana"
  banana   <- getProgName

  forM_ [ ("uuid",     uuid)
        , ("userName", userName)
        , ("progName", progName)
        , ("banana",   banana)
        ] $ \(name, txt) -> do
    putStrLn $ printf "%-20s = %s" (name :: String) (T.unpack txt)

  putStrLn ""

  someBytes <- trulyRandom 8
  let someBytesStr = show $ B.unpack someBytes
  putStrLn $ printf "%-20s = %s" ("someBytes" :: String) someBytesStr

  putStrLn ""
  putStrLn "spewOverview"
  putStrLn "============"
  putStrLn ""

  let mySlaw = SlawList ["Hello, World!", 37619]
      mySlawTxt = spewOverview mySlaw
  putStrLn $ LT.unpack mySlawTxt

  putStrLn ""

  let piDouble = pi :: Double
      piFloat  = pi :: Float
      piHalf   = toFloat16 piFloat

  let nilTxt  = spewOverview SlawNil
      trueTxt = spewOverview $ SlawBool True
      strTxt  = spewOverview "wee"
      symTxt  = spewOverview $ SlawSymbol 12345
      pi64Txt = spewOverview $ š piDouble
      pi32Txt = spewOverview $ š piFloat
      pi16Txt = spewOverview $ š piHalf

  forM_ [ ("nil",  nilTxt)
        , ("true", trueTxt)
        , ("str",  strTxt)
        , ("sym",  symTxt)
        , ("pi64", pi64Txt)
        , ("pi32", pi32Txt)
        , ("pi16", pi16Txt)
        ] $ \(name, txt) -> do
    putStrLn $ printf "%-20s = %s" (name :: String) (LT.unpack txt)

  slawx <- readYamlSlawFile ("test-files/example.yaml" :: String) ()
  forM_ slawx $ \slaw -> do
    putStrLn ""
    putStrLn $ LT.unpack $ spewOverview slaw

  putStrLn ""

  writeYamlSlawFile (NoClose stdout) () [mySlaw]

  let yStr  = "{\"Jenny\": 8675309}"
  ySlaw <- slawFromYamlStringIO yStr ()
  forM_ ySlaw $ \slaw -> do
    putStrLn ""
    putStrLn $ LT.unpack $ spewOverview slaw

  putStrLn ""

  ltxtYaml <- slawToYamlStringIO (mySlaw : ySlaw) ()
  putStrLn $ LT.unpack ltxtYaml

  -- putStrLn ""

  curTime  <- currentTime
  monoTime <- monotonicTime
  fmtTime  <- formatTime curTime
  parTime  <- parseTime fmtTime

  putStrLn $ "currentTime:   " ++ show curTime
  putStrLn $ "monotonicTime: " ++ show monoTime
  putStrLn $ "formatTime:    " ++ T.unpack fmtTime
  putStrLn $ "parseTime:     " ++ show parTime

  putStrLn ""

  let myBs  = toByteString $ toUtf8 ("ρρρ your boat" :: String)
      s32   = 0xC0DE_BABE
      s64   = 0xDEFACED_BAD_FACADE
      jh32  = jenkinsHash32       s32      myBs
      jh64  = jenkinsHash64       s64      myBs
      c64   = cityHash64                   myBs
      c64s  = cityHash64withSeed  s64      myBs
      c64s2 = cityHash64withSeeds s64 h64x myBs
      h64   = hashWord64          s64
      h32   = hashWord32          s32
      h64x  = hash2xWord64        s64 (fromIntegral s32)
      h32x  = hash2xWord32        s32 (fromIntegral s64)

  forM_ [ ("jh32",  jh32)
        , ("h32",   h32)
        , ("h32x",  h32x)
        ] $ \(name, w32) -> do
    putStrLn $ printf "%-20s = 0x%08x" (name :: String) w32

  forM_ [ ("jh64",  jh64)
        , ("c64",   c64)
        , ("c64s",  c64s)
        , ("c64s2", c64s2)
        , ("h64",   h64)
        , ("h64x",  h64x)
        ] $ \(name, w64) -> do
    putStrLn $ printf "%-20s = 0x%016x" (name :: String) w64

  putStrLn ""

  logMsg lvError "Logging an error!"
  logMsg lvInfo  "A two-line\nmessage."

  myLev <- newLogLevel "Custom"
  levelSetDestFile myLev DestStdout
  levelModifyFlags myLev [FlgShowTime, FlgShowPid, FlgShowProg] []
  logMsg myLev "Logging a message\nto a custom LogLevel\n"
  levelSetDestFile myLev DestNone
  logMsg myLev "This won't get printed!"

  putStrLn ""

  let facNames = map T.unpack facilityNames
  putStrLn $ "facilities are: " ++ intercalate ", " facNames

  putStrLn ""

  putStrLn $ show (def :: LogLevel)
  putStrLn $ show (def :: SyslogFacility)
  putStrLn $ show myLev

  putStrLn ""

  withSlawInput ("test-files/example.slaw" :: String) () $ \sis -> do
    putStrLn $ "binary input stream:"
    putStrLn $ "  " ++ show sis

  withSlawInput ("test-files/protein.slaw" :: String) () $ \sis -> do
    putStrLn $ "binary input stream:"
    putStrLn $ "  " ++ show sis

  withSlawInput ("test-files/example.yaml" :: String) () $ \sis -> do
    putStrLn $ "YAML input stream:"
    putStrLn $ "  " ++ show sis

  putStrLn ""

  let wbo     = (def :: WriteBinaryOptions)
      wyo     = (def :: WriteYamlOptions)
      devNull = "/dev/null" :: String

  withSlawOutput devNull wbo $ \sos -> do
    putStrLn $ "binary output stream:"
    putStrLn $ "  " ++ show sos

  withSlawOutput devNull wyo $ \sos -> do
    putStrLn $ "YAML output stream:"
    putStrLn $ "  " ++ show sos

  putStrLn ""

  rs   <- newRandState "Jenny" $ Just (867-5309)
  rf64 <- randFloat64 0.0 1.0 rs
  ri32 <- randInt32   1   100 rs
  rw32 <- randWord32 rs
  rw64 <- randWord64 rs
  nrml <- randNormal rs

  forM_ [ ("rs",   show rs)
        , ("rf64", show rf64)
        , ("ri32", show ri32)
        , ("rw32", show rw32)
        , ("rw64", show rw64)
        , ("nrml", show nrml)
        ] $ \(name, str) -> do
    putStrLn $ printf "%-20s = %s" (name :: String) str

  rBytes <- randBytes 16 rs
  let rBytesStr = concatMap (printf "%02x") $ B.unpack rBytes
  putStrLn $ printf "%-20s = %s" ("rBytes" :: String) (rBytesStr :: String)

  putStrLn ""

  let poolNames = [ "?bad?"
                  , "hello"
                  , "Fully-Automated Luxury Gay Space Communism"
                  , "badness$"
                  , "LPT7.foo"
                  , "pigs-in "
                  , " the-final-frontier"
                  , "my_pool"
                  , "pipeline/gripes"
                  , "tcp://localhost/my_pool"
                  , "tcp://mango:10000/my_pool"
                  , "tcpo://foo-bar.example.com/some/pool"
                  , "tcps://18.85.8.220:1234/a/pool"
                  , "tcp://[::1]/a pool"
                  , "tcp://[fe80::dead:beef]/foo"
                  , "tcp://[fe80::dead:beef]/"
                  , "tcp://[fe80::dead:beef]"
                  , "tcp://[fe80::1ff:fe23:4567:890a%eth2]/pool"
                  , "tcp://chives.la923.oblong.net:1234/"
                  , "tcp://[2001:db8::dead:beef:bad:f00d]:5678/C++"
                  , "tcp://[192.0.2.123]/foo"
                  , "tcp://[2001:db8::baad%en0:1234]/bar"
                  , "tcp://[blech]/stuff"
                  ]

  forM_ poolNames $ \name -> do
    let pathOk = show $ isPoolPathValid name
        uriOk  = show $ isPoolUriValid  name
    putStrLn $ printf "%-5s | %-5s | %s" pathOk uriOk (show name)

  putStrLn ""

  emptyRand <- newRandState "" Nothing
  emptyLog  <- newLogLevel  ""
  emptyCtx  <- newContext   "" ()

  putStrLn $ show emptyRand
  putStrLn $ show emptyLog
  putStrLn $ show emptyCtx

  putStrLn ""

  let ctxOpts = def { coCertificate = Just "foo"
                    , coPrivateKey  = Just "bar"
                    }

  myCtx    <- newContext "My Context" ctxOpts
  ctxOpts' <- getContextOptions myCtx

  putStrLn $ show myCtx
  putStrLn $ show (ctxOpts' :: ContextOptions)

  putStrLn ""

  hose <- participateCreatingly def "My Hose" "my pool" small
  putStrLn $ show hose
  let myProtein = protein "foo bar" [("key", "value")]
  depPair <- deposit hose myProtein
  putStrLn $ show depPair

  forM_ [newestIndex, oldestIndex, currIndex] $ \func -> do
    n <- func hose
    putStrLn $ show n

  let simple = def { wyoTagNumbers  = Just False
                   , wyoOrderedMaps = Just False
                   , wyoComment     = Just False
                   }

  putStrLn ""
  info    <- getInfo hose Nothing
  infoTxt <- slawToYamlStringIO [info] simple
  putStr $ LT.unpack infoTxt

  enableWakeup hose

  (RetProtein myProt2 _ _) <- nthProtein hose (fst depPair)
  (RetProtein myProt3 _ _) <- next       hose
  withdraw hose

  forM_ [myProt2, myProt3] $ \prot -> do
    putStrLn ""
    putStrLn $ LT.unpack $ spewOverview prot

  putStrLn ""

  create  def "another pool" small

  pnames <- listPools def Nothing
  mapM_ (putStrLn . show) pnames

  rename  def "my pool" "dead pool"
  dispose def "dead pool"
  dispose def "another pool"

  putStrLn ""

  create  def "a/man/a/plan/a/canal/panama"   small
  create  def "a/man/a/plan/a/canal+/company" small

  pnames1 <- listPools def (Just "a/man/a/plan/a")
  mapM_ (putStrLn . show) pnames1

  dispose def "a/man/a/plan/a/canal/panama"
  dispose def "a/man/a/plan/a/canal+/company"

  putStrLn ""

  printf "%s\n" (SlawBool True)
  printf "%u\n" (SlawBool True)
  printf "%v\n" (SlawBool True)

  printf "%v\n" ("hello" :: Slaw)
  printf "%.4s\n" ("hello" :: Slaw)

  let num = 3 :: Word8
      den = 4 :: Word8

  printf "%s\n" SlawNil
  printf "%d\n" (SlawSymbol 8675309)
  printf "%x\n" ("16067382063509719774" :: Slaw)
  printf "%v\n" $ SlawCons (š num) (š den)

  printf "%v\n" $ š (pi :: Double)
  printf "%v\n" $ š (pi :: Float)
  printf "%v\n" $ š (pi :: Half)
  printf "%03u\n" $ š den

  printf "%s\n" $ š num
  printf "%c\n" $ SlawBool True

  putStrLn ""

  withTemporaryPool def Nothing small $ \tmpPool -> do
    putStrLn $ toString tmpPool

  putStrLn ""

  let badTimes = [ "Dec 20, 2024 13:30:53.63"
                 , "Blob 20, 2024 13:30:53.63"
                 , "Dec 32, 2024 13:30:53.63"
                 , "Dec 20, 2024 25:30:53.63"
                 , "Dec 20, 2024 13:30:xx.63"
                 , "Dec 20, 2024 13:30:53.xx"
                 , "Dec 20, 867-5309 13:30:53.63"
                 ]

  forM_ badTimes $ \badTime -> do
    putStrLn $ show badTime
    errPt <- try $ parseTime badTime
    putStr "    "
    case errPt of
      Left exc -> putStrLn $ displayPlasmaException False exc
      Right t  -> show <$> formatTime t >>= putStrLn

  putStrLn ""

  gang <- newGang ""
  putStrLn $ show gang

  withTemporaryPool def Nothing small $ \tmpPool1 -> do
    withTemporaryPool def Nothing small $ \tmpPool2 -> do
      hose1 <- participate def "hose1" tmpPool1
      hose2 <- participate def "hose2" tmpPool2
      joinGang gang hose1
      joinGang gang hose2
      members <- getGangMembers gang
      forM_ members $ \aHose -> do
        putStrLn $ "    " ++ show aHose
      withdrawAll gang
