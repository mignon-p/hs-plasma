import Control.Monad
import qualified Data.ByteString          as B
import Data.Default.Class
import Data.List
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import System.IO
import Text.Printf

import Data.Slaw
import Data.Slaw.Extras
import Data.Slaw.IO
import Data.Slaw.IO.Yaml
import Data.Slaw.Util
import System.Loam.Dirs
import System.Loam.Hash
import System.Loam.Log
import System.Loam.Rand
import System.Loam.Time
import System.Loam.Util
import System.Loam.Version

-- import Comprehensive

main :: IO ()
main = do
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

{-
  putStrLn ""
  putStrLn "comprehensiveProtein"
  putStrLn "===================="
  putStrLn ""

  let comprehensiveTxt = spewOverview comprehensiveProtein
  putStrLn $ LT.unpack comprehensiveTxt
-}

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
  let fmtTime = formatTime curTime

  putStrLn $ "currentTime:   " ++ show curTime
  putStrLn $ "monotonicTime: " ++ show monoTime
  putStrLn $ "formatTime:    " ++ T.unpack fmtTime
  putStrLn $ "parseTime:     " ++ show (parseTime fmtTime)

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

  forM_ [ ("jh32",  jh32)
        , ("h32",   h32)
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

  myLev <- makeLogLevel "Custom"
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
