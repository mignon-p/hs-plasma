import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import Text.Printf

import Data.Slaw
import Data.Slaw.Extras
import System.Loam.Dirs
import System.Loam.Rand
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

  randBytes <- trulyRandom 8
  let randBytesStr = show $ B.unpack randBytes
  putStrLn $ printf "%-20s = %s" ("randBytes" :: String) randBytesStr

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
