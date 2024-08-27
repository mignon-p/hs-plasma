import Control.Monad
import Text.Printf

import System.Loam.Version

main :: IO ()
main = do
  w <- x86Features
  let s = printf "%016x" w
  putStrLn s

  forM_ [minBound .. maxBound] $ \v -> do
    t <- getVersion v
    putStrLn $ printf "%-18s = %s" (show v) t
