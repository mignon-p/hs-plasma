import Control.Monad
import Text.Printf

import System.Loam.Dirs
import System.Loam.Version

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
    mFn <- getStandardPath sd
    let fn = case mFn of
               Nothing -> "<Nothing>" :: String
               Just fn' -> fn'
    putStrLn $ printf "%-20s = %s" (show sd) fn
