{-|
Module      : Main
Description : copy slawx between files and/or pools
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

import Control.Exception
-- import Data.List
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.IO        as LT
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Printf

import Data.Slaw
import Data.Slaw.Util

import SlawCat.CmdLine
import SlawCat.ConvertOpts
import SlawCat.PathUtil
import SlawCat.Run
import SlawCat.Types
import SlawCat.Vers
import SlawCat.Wrap

printVersion :: IO ()
printVersion = do
  T.putStrLn "slawcat © 2025 Mignon Pelletier"
  T.putStrLn "https://github.com/mignon-p/hs-plasma"
  T.putStrLn ""
  pairs <- scVers
  mapM_ (T.putStrLn . mv1) pairs

mv1 :: (T.Text, T.Text) -> T.Text
mv1 (pkg, vers) = T.stripEnd $ T.pack $ printf "%-9s %s" pkg vers

ethExc :: IO a -> IO (Either String a)
ethExc action = do
  eth <- tryIO $ tryPE action
  case eth of
    Left ioe        -> return $ formatIOE ioe
    Right (Left pe) -> return $ formatPE  pe
    Right (Right x) -> return $ Right x

formatIOE :: IOError -> Either String a
formatIOE ioe = Left $ displayException $ substIOE ioe

formatPE :: PlasmaException -> Either String a
formatPE pe = Left $ displayPlasmaException False $ substPE pe

substIOE :: IOError -> IOError
substIOE ioe =
  case ioeGetFileName ioe of
    Nothing -> ioe
    Just fp -> ioeSetFileName ioe $ substTilde fp

substPE :: PlasmaException -> PlasmaException
substPE pe@(PlasmaException { peLocation = Just erl }) =
  let erl' = erl { elSource = substDS (elSource erl) }
  in pe { peLocation = Just erl' }
substPE pe = pe

substDS :: DataSource -> DataSource
substDS (DsFile fp) = DsFile $ substTilde fp
substDS ds          = ds

prExc :: GlobalOpts -> IO ExitCode -> IO ExitCode
prExc gopt action = do
  eth <- ethExc action
  case eth of
    Left msg -> do
      hPutStrLn stderr "*** fatal error"
      ww <- getWidth (goptWidth gopt) stderr
      LT.hPutStr stderr $ wrapWithInd ww 4 msg
      return exceptionFailure
    Right x  -> return x

main1 :: ScOpts -> IO ()
main1 opts = do
  ec <- prExc (optGlobal opts) $ invokeWithOpts opts scRun
  exitWith ec

prDups :: IO ()
prDups =
  case scCheckDups of
    Left msg -> do
      hPutStrLn  stderr "Internal error: duplicate options"
      LT.hPutStr stderr msg
      exitWith usageFailure
    _ -> return ()

printUsage :: Visibility -> GlobalOpts -> IO ()
printUsage vis gopts = do
  w <- getWidth (goptWidth gopts) stdout
  LT.hPutStr stdout $ scUsage vis w
  case (scCheckDups, goptVersion gopts) of
    (Right used, True) -> do
      hPutStrLn  stderr ""
      hPutStrLn  stderr "Used single-char options:"
      LT.hPutStr stderr used
    _ -> return ()

wrapErrors :: Maybe Int -> [String] -> LT.Text
wrapErrors ww = wrapMessage ww "error: "

main :: IO ()
main = do
  prDups
  args <- getArgs
  let opts  = scGetOpts args
      gopts = optGlobal opts
  case optErrors opts of
    [] -> case goptHelp gopts of
            HelpUsage vis         -> printUsage vis gopts
            HelpPod               -> LT.putStr scPod
            _ | goptVersion gopts -> printVersion
              | otherwise         -> main1 opts
    xs -> do
      w <- getWidth (goptWidth gopts) stderr
      LT.hPutStr stderr $ wrapErrors w xs
      hPutStrLn  stderr ""
      LT.hPutStr stderr $ scUsage Brief w
      exitWith usageFailure
