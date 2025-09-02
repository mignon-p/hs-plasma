{-|
Module      : SlawCat.ConvertOpts
Description : convert ScOpts to MyOpts, and open files and hoses
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module SlawCat.ConvertOpts
  ( MyOpts(..)
  , invokeWithOpts
  ) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Containers.ListUtils
import Data.Default.Class
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.IO        as LT
import qualified Data.Text.Lazy.Builder   as R
import System.Directory
import System.Exit
import System.IO

import Data.Slaw
import Data.Slaw.IO
import Data.Slaw.IO.Yaml
import Data.Slaw.Util
import System.Plasma.Pool

import SlawCat.CmdLine
import SlawCat.Types
import SlawCat.Units
import SlawCat.Wrap

co0
  :: GlobalOpts
  -> [IoEntity]
  -> IO MyOpts
co0 glOpts ents = do
  mo <- co1 glOpts ents [] [] []
  let awt      = goptAwait    glOpts
      val      = goptValidate glOpts
      inps     = moInputs     mo
      outs     = moOutputs    mo
      havePool = any isPool   inps
      warns1   = moWarnings   mo
      warns2   = if null outs && null val
                 then outWarn : warns1
                 else           warns1
      warns3   = if | null inps           -> inpWarn : warns2
                    | awt && not havePool -> awtWarn : warns2
                    | otherwise           ->           warns2
  return $ mo { moWarnings = warns3 }

awtWarn :: R.Builder
awtWarn = "--await has no effect since there are no input pools"

inpWarn :: R.Builder
inpWarn = "no inputs specified"

outWarn :: R.Builder
outWarn = "no outputs specified"

isPool :: AnnEntity a -> Bool
isPool ae =
  case entType (aeEnt ae) of
    Just IoPool -> True
    _           -> False

co1
  :: GlobalOpts
  -> [IoEntity]
  -> [IoEntity]
  -> [IoEntity]
  -> [R.Builder]
  -> IO MyOpts
co1 glOpts [] inps outs warns =
  return $ MyOpts { moGlobal   = glOpts
                  , moInputs   = map wrapEnt $ reverse inps
                  , moOutputs  = map wrapEnt $ reverse outs
                  , moWarnings = reverse warns
                  }
co1 glOpts (ent:rest) inps outs warns = do
  case entDir ent of
    DirInput -> do
      (inp2, warns2) <- coInput ent
      co1 glOpts rest (inp2 : inps) outs (reverse warns2 ++ warns)
    DirOutput -> do
      (out2, warns2) <- coOutput ent
      co1 glOpts rest inps (out2 : outs) (reverse warns2 ++ warns)

collectWarnings :: IoEntity -> [[T.Text]] -> [R.Builder]
collectWarnings ent ws =
  let warns  = map R.fromText $ concat ws
      pfx    = R.fromText (fixName (entName ent) (entDir ent)) <> ": "
  in map (pfx <>) warns

wrapEnt :: IoEntity -> AnnEntity a
wrapEnt ent = AnnEntity { aeEnt  = ent
                        , aeType = typTxt
                        , aeIdx  = Nothing
                        , aeAnn  = Nothing
                        }
  where typTxt = fmap det1 (entType ent) ?> "unknown"

coInput :: IoEntity -> IO (IoEntity, [R.Builder])
coInput ent = do
  (typ, warns1) <- inpType ent (entType ent)
  let warns2 = warnInOpts  (entOpts  ent) typ
      warns3 = warnInMeta  (entMeta  ent)
      warns4 = warnInQuiet (entQuiet ent)
      warns5 = warnInPos   (entPos   ent) typ
      warns  = [warns1, warns2, warns3, warns4, warns5]
      warns' = collectWarnings ent warns
      ent'   = ent { entType = Just typ }
  return (ent', warns')

inpType :: IoEntity -> Maybe IoType -> IO (IoType, [T.Text])
inpType _ (Just t)
  | t `elem` [IoPool, IoFile] = return (t, [])
  | t == IoSpew               = return (IoFile, [msg])
  | otherwise                 = return (IoFile, [])
  where msg = "'spew' format is not supported on inputs"
inpType ent Nothing = do
  let name = entName ent
  if | looksLikePool name -> return (IoPool, [])
     | looksLikeFile name -> return (IoFile, [])
     | otherwise          -> do
         ex <- orFalse $ doesFileExist name
         return (if ex then IoFile else IoPool, [])

orFalse :: IO Bool -> IO Bool
orFalse action = do
  eth <- tryIO action
  case eth of
    Left  _ -> return False
    Right b -> return b

mySpanEnd :: (a -> Bool) -> [a] -> ([a], [a])
mySpanEnd f xs = (reverse xs1, reverse xs2)
  where (xs2, xs1) = span f $ reverse xs

dirAndBase :: FilePath -> (FilePath, FilePath)
dirAndBase = mySpanEnd (not . isDirSep)

myBaseName :: FilePath -> FilePath
myBaseName = snd . dirAndBase

isDirSep :: Char -> Bool
isDirSep '/'  = True
isDirSep '\\' = True
isDirSep _    = False

myExtension :: FilePath -> FilePath
myExtension fp =
  let base     = myBaseName fp
      (b, ext) = mySpanEnd (/= '.') base
  in if "." `isSuffixOf` b then ext else ""

typeFromExtension :: FilePath -> Maybe IoType
typeFromExtension fp =
  let ext = map toLower $ myExtension fp
  in if | ext `elem` ["slaw", "bin"] -> Just IoBinary
        | ext `elem` ["yaml", "pro"] -> Just IoYaml
        | ext `elem` ["spew", "txt"] -> Just IoSpew
        | otherwise                  -> Nothing

isStdInOut :: FilePath -> Bool
isStdInOut ""  = True
isStdInOut "-" = True
isStdInOut _   = False

looksLikeFile :: FilePath -> Bool
looksLikeFile fp
  | isStdInOut fp = True
  | otherwise     = typeFromExtension fp /= Nothing

looksLikePool :: FilePath -> Bool
looksLikePool fp =
  let (scheme, rest) = span (/= ':') fp
      schemePN       = toPoolName $ map toLower scheme
  in if "://" `isPrefixOf` rest
     then schemePN `elem` [kLocal, kTcp, kTcpo, kTcps]
     else False

getKeys :: Slaw -> [T.Text]
getKeys (SlawProtein { slawIngests = Just s }) = getKeys s
getKeys (SlawMap pairs)                        = mapMaybe gk1 pairs
getKeys _                                      = []

gk1 :: (Slaw, Slaw) -> Maybe T.Text
gk1 (SlawString k, _) = Just $ fromUtf8 k
gk1 _                 = Nothing

warnInOpts :: Slaw -> IoType -> [T.Text]
warnInOpts opts _ = mapMaybe wio1 $ dedup $ getKeys opts

wio1 :: T.Text -> Maybe T.Text
wio1 "format" = Nothing
wio1 k        = Just $ mconcat [ "option \""
                               , k
                               , "\" has no effect on inputs"
                               ]

warnInMeta :: Maybe String -> [T.Text]
warnInMeta Nothing = []
warnInMeta (Just _) =
  ["--add-metadata has no effect on inputs"]

warnInQuiet :: Bool -> [T.Text]
warnInQuiet False = []
warnInQuiet True  =
  ["--no-metadata-comment has no effect on inputs"]

warnInPos :: InitialPos -> IoType -> [T.Text]
warnInPos _   IoPool = []
warnInPos pos _      =
  case posOption pos of
    Nothing  -> []
    Just opt -> [opt <> "has no effect on file inputs"]

posOption :: InitialPos -> Maybe T.Text
posOption PosRewind     = Nothing -- (default option)
posOption PosToLast     = Just "--to-last"
posOption PosRunout     = Just "--runout"
posOption (PosSeekTo _) = Just "--seek-to"

coOutput :: IoEntity -> IO (IoEntity, [R.Builder])
coOutput ent = do
  (typ, warns1) <- outType ent (entType ent)
  let warns2 = warnOutOpts  (entOpts  ent) typ
      warns3 = warnOutQuiet (entQuiet ent) typ
      warns4 = warnOutPos   (entPos   ent)
      warns  = collectWarnings ent [warns1, warns2, warns3, warns4]
      ent'   = ent { entType = Just typ }
  return (ent', warns)

outType :: IoEntity -> Maybe IoType -> IO (IoType, [T.Text])
outType ent (Just IoFile) = otFile ent
outType _   (Just t     ) = return (t, [])
outType ent Nothing       = otAny  ent

otFile :: IoEntity -> IO (IoType, [T.Text])
otFile ent =
  case typeFromExtension (entName ent) of
    Just t  -> return (t,      [])
    Nothing -> return (IoYaml, [])

otAny :: IoEntity -> IO (IoType, [T.Text])
otAny ent
  | looksLikePool name = return (IoPool, [])
  | looksLikeFile name = otFile ent
  | not validUri       = otFile ent
  | otherwise          = return (IoPool, [])
  where
    name     = entName        ent
    pname    = toPoolName     name
    validUri = isPoolUriValid pname

{-
fixDir :: FilePath -> FilePath
fixDir ""  = "."
fixDir dir = dir

isWritableDir :: FilePath -> IO Bool
isWritableDir dir = do
  dx <- doesDirectoryExist dir
  case dx of
    False -> return False
    True  -> do
      p <- getPermissions dir
      return $ writable p
-}

omit1, omit2 :: [T.Text]
omit1 = ["format"]
omit2 = "auto-flush" : omit1

warnOutOpts :: Slaw -> IoType -> [T.Text]
warnOutOpts opts IoBinary =
  woo1 (prune opts (def :: WriteBinaryOptions)) omit2 "binary"
warnOutOpts opts IoYaml   =
  woo1 (prune opts (def :: WriteYamlOptions))   omit2 "yaml"
warnOutOpts opts IoSpew   = woo1 (getKeys opts) omit2 "spew"
warnOutOpts opts IoPool   = woo1 (getKeys opts) omit1 "pool"
warnOutOpts _    _        = []

woo1 :: [T.Text] -> [T.Text] -> T.Text -> [T.Text]
woo1 keys omit tName = mapMaybe (woo2 omit tName) $ dedup keys

woo2 :: [T.Text] -> T.Text -> T.Text -> Maybe T.Text
woo2 omit tName k
  | k `elem` omit = Nothing
  | otherwise     = Just $ mconcat [ "option \""
                                   , k
                                   , "\" has no effect on "
                                   , tName
                                   , " output"
                                   ]

prune :: (ToSlaw a, FromSlaw a) => Slaw -> a -> [T.Text]
prune s1 dflt =
  let opts = s1 ?: dflt
      s2   = toSlaw opts
      k1   = S.fromList $ getKeys s1
      k2   = S.fromList $ getKeys s2
      ks   = k1 `S.difference` k2
  in S.toDescList ks

warnOutQuiet :: Bool -> IoType -> [T.Text]
warnOutQuiet False _   = []
warnOutQuiet True  typ =
  case noComment typ of
    Nothing   -> []
    Just name ->
      let msg = mconcat [ "--no-metadata-comment has no effect on "
                        , name
                        , " output"
                        ]
      in [msg]

warnOutPos :: InitialPos -> [T.Text]
warnOutPos pos =
  case posOption pos of
    Nothing  -> []
    Just opt -> [opt <> "has no effect on outputs"]

noComment :: IoType -> Maybe T.Text
noComment IoBinary = Just "binary"
noComment IoPool   = Just "pool"
noComment _        = Nothing

-- "dump" stuff

printWarn :: GlobalOpts -> Handle -> [R.Builder] -> IO ()
printWarn _     _ []    = return ()
printWarn gopt h warns = do
  ww <- getWidth (goptWidth gopt) h
  LT.hPutStrLn h $ wrapMessage ww "warning: " warns

dmp1 :: MyOpts -> LT.Text
dmp1 mo =
  case slawToYamlString [dmp2 mo] wyo of
    Left  _   -> ""
    Right txt -> removeDashDot txt
  where
    wyo = def { wyoTagNumbers  = Just False
              , wyoDirectives  = Just False
              , wyoOrderedMaps = Just False
              , wyoComment     = Just False
              }

dmp2 :: MyOpts -> Slaw
dmp2 mo = SlawMap
  [ ("Global Options", dmpGlobal   (moGlobal  mo))
  , ("Inputs",         dmpEntities (moInputs  mo))
  , ("Outputs",        dmpEntities (moOutputs mo))
  ]

dmpGlobal :: GlobalOpts -> Slaw
dmpGlobal gopt = SlawMap $ catMaybes
  [ Just ("await", SlawBool (goptAwait gopt))
  , fmap   dmpGTimeout  (goptTimeout  gopt)
  , fmap   dmpGCount    (goptCount    gopt)
  , Just $ dmpGValidate (goptValidate gopt)
  , Just ("digits of fractional seconds", toSlaw (goptFracDigs gopt))
  , dmpGDesIng "descrip" (goptDescrips gopt)
  , dmpGDesIng "ingest"  (goptIngests  gopt)
  ]

dmpGTimeout :: Duration -> (Slaw, Slaw)
dmpGTimeout dur = ("time to await", toSlaw (formatDuration dur))

dmpGCount :: Integer -> (Slaw, Slaw)
dmpGCount n = ("count", toSlaw n)

dmpGValidate :: [ValidationFlag] -> (Slaw, Slaw)
dmpGValidate []    = ("validation", "standard validation only")
dmpGValidate flags = ("validation", SlawList (map dgv1 flags))

dgv1 :: ValidationFlag -> Slaw
dgv1 vf = toSlaw $ case vf `lookup` valNames of
                     Nothing -> T.pack $ show vf
                     Just x  -> x

dmpGDesIng :: T.Text -> [Slaw] -> Maybe (Slaw, Slaw)
dmpGDesIng _ []  = Nothing
dmpGDesIng t [x] = Just (š (t <>  " to filter on"),  x)
dmpGDesIng t xs  = Just (š (t <> "s to filter on"), SlawList xs)

dmpEntities :: [AnnEntity a] -> Slaw
dmpEntities []   = "none"
dmpEntities ents = SlawMap $ map dmpEntity ents

dmpEntity :: AnnEntity a -> (Slaw, Slaw)
dmpEntity ae = (key, SlawMap pairs)
  where
    ent   = aeEnt    ae
    tTxt  = aeType   ae
    idx   = aeIdx    ae

    name  = entName  ent
    dir   = entDir   ent
    typ   = entType  ent
    opts  = entOpts  ent
    meta  = entMeta  ent
    quiet = entQuiet ent
    pos   = entPos   ent

    isOut    = dir == DirOutput
    isIn     = dir == DirInput
    isInPool = isIn && typ == Just IoPool

    key   = toSlaw $ fixName name dir
    pairs = catMaybes
      [ Just (dmpEType tTxt)
      , if isInPool then      dmpEPos  pos  idx else Nothing
      , if isOut    then      dmpEOpts opts     else Nothing
      , if isOut    then fmap dmpEMeta meta     else Nothing
      , if isOut && commentOk typ
        then Just ("add metadata comment", SlawBool (not quiet))
        else Nothing
      ]

commentOk :: Maybe IoType -> Bool
commentOk Nothing  = True
commentOk (Just t) = Nothing == noComment t

fixName :: String -> IoDir -> T.Text
fixName name _        | not (isStdInOut name) = T.pack name
fixName _    DirInput                         = "stdin"
fixName _    DirOutput                        = "stdout"

dmpEType :: T.Text -> (Slaw, Slaw)
dmpEType t = ("type", š t)

det1 :: IoType -> T.Text
det1 IoBinary = "binary"
det1 IoYaml   = "yaml"
det1 IoSpew   = "spew"
det1 IoPool   = "pool"
det1 IoFile   = "file"

dmpEPos :: InitialPos -> Maybe PoolIndex -> Maybe (Slaw, Slaw)
dmpEPos PosRewind        idx = posPair "rewind"       idx
dmpEPos PosToLast        idx = posPair "toLast"       idx
dmpEPos PosRunout        idx = posPair "runout"       idx
dmpEPos (PosSeekTo idx1) (Just idx2)
  | idx1 /= idx2             = posPair (fmtSeek idx1) (Just idx2)
dmpEPos (PosSeekTo idx)  _   = posPair (fmtSeek idx)  Nothing

fmtSeek :: PoolIndex -> String
fmtSeek idx = "seekTo " ++ show idx

posPair :: String -> Maybe PoolIndex -> Maybe (Slaw, Slaw)
posPair str idx = Just ("initial position", toSlaw (str ++ sfx))
  where sfx = case idx of
                Nothing -> ""
                Just i  -> " (index=" ++ show i ++ ")"

dmpEOpts :: Slaw -> Maybe (Slaw, Slaw)
dmpEOpts (SlawProtein { slawIngests = Just s }) = dmpEOpts s
dmpEOpts (SlawMap pairs) =
  case filter notFormat pairs of
    [] -> Nothing
    xs -> Just ("options", SlawMap (dedupPairs xs))
dmpEOpts _ = Nothing

notFormat :: (Slaw, Slaw) -> Bool
notFormat (SlawString "format", _) = False
notFormat _                        = True

dmpEMeta :: String -> (Slaw, Slaw)
dmpEMeta ing = ("add metadata ingest", toSlaw ing)

removeDashDot :: LT.Text -> LT.Text
removeDashDot =
  myStrip LT.stripPrefix "---\n" . myStrip LT.stripSuffix "...\n"

myStrip
  :: (LT.Text -> LT.Text -> Maybe LT.Text)
  -> LT.Text
  -> LT.Text
  -> LT.Text
myStrip f psFix txt = f psFix txt ?> txt

dedup :: [T.Text] -> [T.Text]
dedup = nubOrdOn f
  where f = T.map underDash

dedupPairs :: [(Slaw, Slaw)] -> [(Slaw, Slaw)]
dedupPairs = nubOrdOn ddp1

ddp1 :: (Slaw, Slaw) -> (Maybe T.Text, Slaw)
ddp1 (k, v) =
  case ŝm k of
    Just txt -> (Just (T.map underDash txt), v)
    Nothing  -> (Nothing, SlawCons k v)

underDash :: Char -> Char
underDash '_' = '-'
underDash x   = x

-- open files and hoses

type MyContinuation = MyOpts -> IO ExitCode

invokeWithOpts
  :: ScOpts
  -> MyContinuation
  -> IO ExitCode
invokeWithOpts opts action = do
  let glOpts  = optGlobal opts
      verbose = not $ goptQuiet glOpts
  myOpts <- co0 glOpts (optEnts opts)
  when verbose $ printWarn glOpts stderr $ moWarnings myOpts
  dumpedRef <- newIORef False
  iwo1 dumpedRef action myOpts `onException` dumpIfErr myOpts dumpedRef

dumpIfErr :: MyOpts -> IORef Bool -> IO ()
dumpIfErr mo dumpedRef = do
  dumped <- readIORef dumpedRef
  when (not dumped) $ dumpUnlessQuiet mo

iwo1 :: IORef Bool -> MyContinuation -> MyContinuation
iwo1 dumpedRef action =
  openInputs $ openOutputs $ dumpNormally dumpedRef $ action

openInputs :: MyContinuation -> MyContinuation
openInputs cont mo = oi1 (moInputs mo) [] cont mo

oi1
  :: [AnnEntity InputEntity]
  -> [AnnEntity InputEntity]
  -> MyContinuation
  -> MyContinuation
oi1 [] newEnts cont mo = cont $ mo { moInputs = reverse newEnts }
oi1 (ent:oldEnts) newEnts cont mo = do
  bracket (openInput ent) closeInput $ \newEnt -> do
    oi1 oldEnts (newEnt : newEnts) cont mo

openOutputs :: MyContinuation -> MyContinuation
openOutputs cont mo = oo1 (moOutputs mo) [] cont mo

oo1
  :: [AnnEntity OutputEntity]
  -> [AnnEntity OutputEntity]
  -> MyContinuation
  -> MyContinuation
oo1 [] newEnts cont mo = cont $ mo { moOutputs = reverse newEnts }
oo1 (ent:oldEnts) newEnts cont mo = do
  bracket (openOutput ent $ moGlobal mo) closeOutput $ \newEnt -> do
    oo1 oldEnts (newEnt : newEnts) cont mo

dumpNormally :: IORef Bool -> MyContinuation -> MyContinuation
dumpNormally dumpedRef cont mo = do
  writeIORef dumpedRef True
  dumpUnlessQuiet mo
  cont mo

dumpUnlessQuiet :: MyOpts -> IO ()
dumpUnlessQuiet mo = do
  let verbose = not $ goptQuiet $ moGlobal mo
  when verbose $ LT.hPutStrLn stderr $ dmp1 mo

-- open and close inputs

openInput :: AnnEntity InputEntity -> IO (AnnEntity InputEntity)
openInput ae = do
  let ent = aeEnt ae
  case entType ent of
    Just IoPool -> do
      (ann, tTxt, idx) <- openPoolInput ent
      return $ ae { aeType = tTxt, aeAnn = Just ann, aeIdx = Just idx }
    _           -> do
      (ann, tTxt) <- openStreamInput ent
      return $ ae { aeType = tTxt, aeAnn = Just ann }

openStreamInput :: IoEntity -> IO (InputEntity, T.Text)
openStreamInput ent = do
  let name = entName ent
      opts = entOpts ent
  si <- case isStdInOut name of
          False -> openSlawInput name  opts
          True  -> openSlawInput stdin opts
  return (InStream si, T.pack (siType si))

openPoolInput :: IoEntity -> IO (InputEntity, T.Text, PoolIndex)
openPoolInput ent = do
  let name  = entName ent
      pName = toPoolName name
      hName = "input: " <> T.pack name
  h <- participate def hName pName
  tTxt <- prependPoolType h "pool"
  case entPos ent of
    PosRewind   -> rewind h
    PosToLast   -> toLast h
    PosRunout   -> return () -- this is where "participate" leaves it
    PosSeekTo n -> seekTo h n
  idx <- currIndex h
  return (InPool h, tTxt, idx)

closeInput :: AnnEntity InputEntity -> IO ()
closeInput ae = do
  case aeAnn ae of
    Nothing            -> return ()
    Just (InStream si) -> siClose  si
    Just (InPool   h ) -> withdraw h

-- open and close outputs

openOutput
  :: AnnEntity OutputEntity
  -> GlobalOpts
  -> IO (AnnEntity OutputEntity)
openOutput ae glOpts = do
  let ent = aeEnt ae
  case entType ent of
    Just IoPool -> do
      (ann, tTxt) <- openPoolOutput ent
      return $ ae { aeType = tTxt, aeAnn = Just ann }
    Just IoSpew -> do
      ann <- openFileOutput ent
      return $ ae { aeAnn = Just ann }
    _           -> do
      (ann, tTxt) <- openStreamOutput ent glOpts
      return $ ae { aeType = tTxt, aeAnn = Just ann }

openPoolOutput :: IoEntity -> IO (OutputEntity, T.Text)
openPoolOutput ent = do
  let name  = entName ent
      pName = toPoolName name
      hName = "output: " <> T.pack name
  (h, created) <- participateCreatingly def hName pName small
  let cTxt = if created
             then "pool (created)"
             else "pool (already existed)"
  tTxt <- prependPoolType h cTxt
  return (OutPool h, tTxt)

openFileOutput :: IoEntity -> IO OutputEntity
openFileOutput ent = do
  (h, clo) <- openOutHandle (entName ent)
  af       <- getAutoFlush  (entOpts ent) h
  ref      <- newIORef      0
  return $ OutFile h clo ref af

openStreamOutput :: IoEntity -> GlobalOpts -> IO (OutputEntity, T.Text)
openStreamOutput ent glOpts = do
  (h, clo) <- openOutHandle (entName ent)
  opts     <- tweakCommentOption ent glOpts h
  so       <- if clo
              then openSlawOutput          h  opts
              else openSlawOutput (NoClose h) opts
  return (OutStream h so, T.pack (soType so))

getAutoFlush :: Slaw -> Handle -> IO Bool
getAutoFlush opts h = do
  let af = case ŝm opts of
             Just (WriteYamlOptions { wyoAutoFlush = Just x }) -> x
             _ -> AutoFlushIfNotSeekable
  case af of
    AutoFlushNever         -> return False
    AutoFlushAlways        -> return True
    AutoFlushIfNotSeekable -> not <$> hIsSeekable h

-- Default wyoComment to False, if:
--   • This is a YAML output
--   • This output is going to the terminal
--   • stderr is also going to the terminal
--   • -q (--quiet) option is not specified
--
-- This is because the YAML comment header will show up before
-- the summary of options that is printed to stderr.
tweakCommentOption :: IoEntity -> GlobalOpts -> Handle -> IO Slaw
tweakCommentOption ent@(IoEntity { entType = Just IoYaml }) g h
  | not (goptQuiet g) = do
      let opts = entOpts ent
      termH   <- hIsTerminalDevice h
      termErr <- hIsTerminalDevice stderr
      if termH && termErr
        then return $ opts `prefLeft` falseComment
        else return   opts
tweakCommentOption ent _ _ = return $ entOpts ent

falseComment :: Slaw
falseComment = toSlaw $ def { wyoComment = Just False }

openOutHandle :: FilePath -> IO (Handle, Bool)
openOutHandle fp
  | isStdInOut fp = return (stdout, False)
  | otherwise     = do
      h <- openBinaryFile fp WriteMode
      return (h, True)

closeOutput :: AnnEntity OutputEntity -> IO ()
closeOutput ae = do
  case aeAnn ae of
    Nothing                    -> return ()
    Just (OutPool   h        ) -> withdraw h
    Just (OutStream _ so     ) -> soClose  so
    Just (OutFile   h clo _ _) -> when clo $ hClose h

prependPoolType :: Hose -> T.Text -> IO T.Text
prependPoolType h tTxt = do
  pinfo <- getInfo h (Just 0)
  case (piType pinfo, piSize pinfo) of
    (Nothing,  _      ) -> return tTxt
    (Just typ, Nothing) -> return $ typ <> " " <> tTxt
    (Just typ, Just n ) -> do
      let sz = " (" ++ formatNumBytes n ++ ")"
      return $ typ <> " " <> tTxt <> T.pack sz
