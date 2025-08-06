import Control.Exception
import Data.List
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.IO        as LT
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import Text.Printf

import Data.Slaw
import Data.Slaw.Util

import SlawCat.ConvertOpts
import SlawCat.CmdLine
import SlawCat.Run
import SlawCat.Types
import SlawCat.Wrap
import SlawCat.Vers

printVersion :: IO ()
printVersion = do
  T.putStrLn "slawcat © 2025 Mignon Pelletier"
  T.putStrLn ""
  pairs <- scVers
  mapM_ (T.putStrLn . mv1) pairs

mv1 :: (T.Text, T.Text) -> T.Text
mv1 (pkg, vers) = T.stripEnd $ T.pack $ printf "%-9s %s" pkg vers

ethExc :: IO a -> IO (Either String a)
ethExc action = do
  eth <- tryIO $ tryPE action
  case eth of
    Left ioe        -> formatIOE ioe
    Right (Left pe) -> formatPE  pe
    Right (Right x) -> return $ Right x

formatIOE :: IOError -> IO (Either String a)
formatIOE ioe = do
  ioe' <- substIOE ioe
  return $ Left  $ displayException ioe'

formatPE :: PlasmaException -> IO (Either String a)
formatPE pe = do
  pe' <- substPE pe
  return $ Left  $ displayPlasmaException False pe'

substIOE :: IOError -> IO IOError
substIOE ioe = do
  case ioeGetFileName ioe of
    Nothing -> return ioe
    Just fp -> do
      fp' <- substTilde fp
      return $ ioeSetFileName ioe fp'

substPE :: PlasmaException -> IO PlasmaException
substPE pe@(PlasmaException { peLocation = Just erl }) = do
  ds' <- substDS $ elSource erl
  let erl' = erl { elSource = ds' }
  return $ pe { peLocation = Just erl' }
substPE pe = return pe

substDS :: DataSource -> IO DataSource
substDS (DsFile fp) = DsFile <$> substTilde fp
substDS ds          = return ds

substTilde :: FilePath -> IO FilePath
substTilde fp
  | isRelative fp = return fp
  | otherwise     = do
      home <- lookupEnv "HOME"
      return $ st1 home fp

st1 :: Maybe FilePath -> FilePath -> FilePath
st1 Nothing     orig = orig
st1 (Just home) orig
  | null home || isRelative home = orig
  | tilde                        = '~' : rest
  | otherwise                    = orig
  where h    = dropTrailingPathSeparator home
        hLen = length h
        rest = drop hLen orig
        slash = case rest of
                  ('/' :_) -> True
                  ('\\':_) -> True
                  _        -> False
        tilde = h `isPrefixOf` orig && slash

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
            Just vis              -> printUsage vis gopts
            _ | goptVersion gopts -> printVersion
              | otherwise         -> main1 opts
    xs -> do
      w <- getWidth (goptWidth gopts) stderr
      LT.hPutStr stderr $ wrapErrors w xs
      hPutStrLn  stderr ""
      LT.hPutStr stderr $ scUsage Brief w
      exitWith usageFailure
