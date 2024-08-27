import Text.Printf

import System.Loam.Version

main :: IO ()
main = do
  w <- x86Features
  let s = printf "%016x" w
  putStrLn s
