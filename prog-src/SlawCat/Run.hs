module SlawCat.Run (scRun) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.Maybe
import qualified Data.Text                  as T
-- import qualified Data.Text.IO               as T
-- import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as R
import qualified Data.Text.Lazy.Builder.Int as R
import qualified Data.Text.Lazy.IO          as LT
import Data.Time.Format.ISO8601
-- import Data.Word
import System.Exit
import System.IO

import Data.Slaw
import Data.Slaw.Extras
import Data.Slaw.IO
-- import Data.Slaw.IO.Yaml
import System.Loam.Retorts.Constants
import System.Loam.Time
import System.Plasma.Pool

import SlawCat.Types
import SlawCat.Units
import SlawCat.Wrap

type CountRef  = IORef (Maybe Integer)
type AnnInput  = AnnEntity InputEntity
type AnnOutput = AnnEntity OutputEntity

scRun :: MyOpts -> IO ExitCode
scRun opts = do
  let gOpts = moGlobal  opts
      inps  = moInputs  opts
      awt   = goptAwait gOpts
  countRef <- newIORef $ goptCount gOpts
  ec1 <- readInputs opts countRef inps 0 ExitSuccess
  ec2 <- if awt
         then awaitInputs opts countRef inps
         else return ExitSuccess
  return $ ec1 `combineExitCode` ec2

isCountZero :: CountRef -> IO Bool
isCountZero countRef = do
  cnt <- readIORef countRef
  case cnt of
    Nothing -> return False
    Just n  -> return $ n <= 0

decrCount :: CountRef -> IO ()
decrCount countRef = do
  cnt <- readIORef countRef
  case cnt of
    Just n | n > 0 -> writeIORef countRef $ Just $ n - 1
    _              -> return ()

combineExitCode :: ExitCode -> ExitCode -> ExitCode
combineExitCode ExitSuccess e2 = e2
combineExitCode e1          _  = e1

-- read inputs

readInputs
  :: MyOpts
  -> CountRef
  -> [AnnInput]
  -> PoolIndex
  -> ExitCode
  -> IO ExitCode
readInputs _  _    []              _       !ec = return ec
readInputs mo cRef inps@(inp:rest) !serial !ec = do
  done <- isCountZero cRef
  case done of
    True  -> return ec
    False -> do
      (success, ec1) <- readInput mo inp serial
      let ec2 = ec `combineExitCode` ec1
      case success of
        False -> readInputs mo cRef rest 0 ec2
        True  -> do
          decrCount cRef
          readInputs mo cRef inps (serial + 1) ec2

readInput :: MyOpts -> AnnInput -> PoolIndex -> IO (Bool, ExitCode)
readInput mo ae@(AnnEntity { aeAnn = Just ie }) serial = do
  let ent = aeEnt ae
  maybeSlaw <- ri1 mo ent serial ie
  case maybeSlaw of
    Nothing -> return (False, ExitSuccess)
    Just s  -> do
      ec <- processSlaw mo s
      return (True, ec)
readInput _ _ _ = return (False, ExitSuccess)

ri1
  :: MyOpts
  -> IoEntity
  -> PoolIndex
  -> InputEntity
  -> IO (Maybe MetaSlaw)
ri1 mo ent _      (InPool   h ) = readPool   mo ent h
ri1 mo ent serial (InStream si) = readStream mo ent serial si

readPool :: MyOpts -> IoEntity -> Hose -> IO (Maybe MetaSlaw)
readPool _ ent h = do
  eth <- tryJust isNoSuchProtein $ next h
  case eth of
    Left  _  -> return Nothing
    Right rp -> return $ Just $ rpToMetaSlaw rp $ entName ent

isNoSuchProtein :: PlasmaException -> Maybe PlasmaException
isNoSuchProtein pe@(PlasmaException { peRetort = Just tort })
  | tort == POOL_NO_SUCH_PROTEIN = Just pe
isNoSuchProtein _ = Nothing

rpToMetaSlaw :: RetProtein -> String -> MetaSlaw
rpToMetaSlaw rp name = MetaSlaw
  { msSlaw      = rpProtein rp
  , msIndex     = rpIndex   rp
  , msTimestamp = Just $ rpTimestamp rp
  , msSource    = ("pool", name)
  }

readStream
  :: MyOpts
  -> IoEntity
  -> PoolIndex
  -> SlawInputStream
  -> IO (Maybe MetaSlaw)
readStream _ ent serial si = do
  let name = entName ent
  maybeSlaw <- siRead si
  case maybeSlaw of
    Nothing -> return Nothing
    Just s  ->
      return $ Just $ MetaSlaw { msSlaw      = s
                               , msIndex     = serial
                               , msTimestamp = Nothing
                               , msSource    = ("file", name)
                               }

-- await inputs

awaitInputs :: MyOpts -> CountRef -> [AnnInput] -> IO ExitCode
awaitInputs mo cRef ais = do
  let hoses = mapMaybe mbyHose ais
  done <- isCountZero cRef
  case (hoses, done) of
    ((_:_), False) -> do
      bracket (newGang "slawcat await gang") clearGang $ \g -> do
        mapM_ (joinGang g) hoses
        ai0 mo cRef g
    _ -> return ExitSuccess

mbyHose :: AnnInput -> Maybe Hose
mbyHose (AnnEntity { aeAnn = Just (InPool h) }) = Just h
mbyHose _ = Nothing

ai0 :: MyOpts -> CountRef -> Gang -> IO ExitCode
ai0 mo cRef g = do
  mv <- newEmptyMVar
  forkFinally (aiThreadMain mo cRef g) (putMVar mv)
  eth <- onException (readMVar mv) $ do
    wakeGang g
    readMVar mv
  case eth of
    Left  e  -> throwIO e
    Right ec -> return  ec

aiThreadMain :: MyOpts -> CountRef -> Gang -> IO ExitCode
aiThreadMain mo cRef g = do
  let gopt = moGlobal mo
      tout = goptTimeout gopt
  endTime <- case tout of
               Nothing  -> return Nothing
               Just dur -> do
                 beginTime <- monotonicTime
                 return $ Just $ beginTime + dur
  ai1 mo cRef g ExitSuccess endTime

ai1
  :: MyOpts
  -> CountRef
  -> Gang
  -> ExitCode
  -> Maybe MonotonicTime
  -> IO ExitCode
ai1 mo cRef g !ec endTime = do
  done     <- isCountZero cRef
  waitTime <- case endTime of
                Nothing -> return Nothing
                Just et -> do
                  now <- monotonicTime
                  return $ Just $ et - now
  case (done, waitTime) of
    (True,  _      )           -> do
      notify mo "stopped awaiting because count was reached"
      return ec
    (_,     Just wt)
      | wt <= 0 -> do
          notify mo "stopped awaiting because timeout was reached"
          return ec
    (False, _      )           -> do
      ec1 <- catchJust isTimeout (ai2 mo g waitTime) return
      let ec2 = ec `combineExitCode` ec1
      decrCount cRef
      ai1 mo cRef g ec2 endTime

ai2 :: MyOpts -> Gang -> Maybe Duration -> IO ExitCode
ai2 mo g wt = do
  (rp, h) <- awaitNextMulti g $ durToTimeout wt
  let name = fromPoolName $ hosePool h
      ms   = rpToMetaSlaw rp name
  processSlaw mo ms

durToTimeout :: Maybe Duration -> PoolTimeout
durToTimeout Nothing  = WaitForever
durToTimeout (Just t) = Timeout $ realToFrac t

isTimeout :: PlasmaException -> Maybe ExitCode
isTimeout (PlasmaException { peRetort = Just POOL_AWAIT_TIMEDOUT }) =
  Just ExitSuccess
isTimeout _ = Nothing

notify :: MyOpts -> T.Text -> IO ()
notify mo msg = do
  let gopt  = moGlobal mo
      quiet = goptQuiet gopt
  when (not quiet) $ do
    ww <- getWidth (goptWidth gopt) stderr
    LT.hPutStr stderr $ wrapMessage ww "note: " [msg]

-- process slaw

processSlaw :: MyOpts -> MetaSlaw -> IO ExitCode
processSlaw mo metaSlaw = do
  ec <- valSlaw mo metaSlaw $ goptValidate $ moGlobal mo
  writeOutputs mo metaSlaw
  return ec

valSlaw
  :: MyOpts
  -> MetaSlaw
  -> [ValidationFlag]
  -> IO ExitCode
valSlaw mo metaSlaw flags = do
  let gopt = moGlobal mo
  case validateSlaw flags (msSlaw metaSlaw) of
    Right _ -> return ExitSuccess
    Left pe -> do
      ww <- getWidth (goptWidth gopt) stderr
      let msg  = adjMsg pe
          ind1 = "   at "
          ind2 = "      "
          loc  = fmtLocation gopt ind1 ind2 metaSlaw
          xtra = byteLoc ind2 $ peLocation pe
          bld  = R.fromString msg <> nl <> loc <> xtra
          txt  = wrapOnSep ww ": " bld
      LT.hPutStr stderr txt
      return validationFailure

adjMsg :: PlasmaException -> String
adjMsg pe@(PlasmaException { peType = EtCorruptSlaw })
  | not (':' `elem` msg) = msg'
  where msg  = peMessage pe
        msg' = "bad slaw: " ++ msg
adjMsg pe = peMessage pe

byteLoc :: R.Builder -> Maybe ErrLocation -> R.Builder
byteLoc ind (Just (ErrLocation { elOffset = Just off })) = mconcat
  [ ind
  , "byte offset: "
  , R.decimal off
  , " (0x"
  , R.hexadecimal off
  , ")"
  ]
byteLoc _ _ = mempty

fmtTimestamp :: GlobalOpts -> PoolTimestamp -> R.Builder
fmtTimestamp gopt ts = "timestamp: " <> R.fromString str
  where
    utc = loamTimeToUtcTime ts
    str = case formatShowM iso8601Format utc of
            Nothing -> "unknown"
            Just s  -> fmtAsMicros gopt s

fmtAsMicros :: GlobalOpts -> String -> String
fmtAsMicros gopt str =
  let fracDigs = goptFracDigs gopt
  in case break (== '.') str of
       (pfx, '.' : rest) -> pfx ++ fam1 fracDigs rest
       _                 -> str

fam1 :: Int -> String -> String
fam1 fracDigs str
  | fracDigs > 0 = '.' : newDigs ++ sfx
  | otherwise    = sfx
  where
    (digs, sfx) = span isDigit str
    newDigs     = take fracDigs $ digs ++ repeat '0'

fmtSource :: (String, String) -> R.Builder
fmtSource (typ, name) = R.fromString typ <> ": " <> R.fromString name

joinLine :: R.Builder -> R.Builder -> R.Builder
joinLine pfx ln = pfx <> ln <> nl

fmtLocation
  :: GlobalOpts
  -> R.Builder
  -> R.Builder
  -> MetaSlaw
  -> R.Builder
fmtLocation gopt pfx1 pfxRest ms =
  let lns1 = [ Just $ "index: " <> R.decimal (msIndex ms)
             , fmap (fmtTimestamp gopt) $ msTimestamp ms
             , Just $ fmtSource $ msSource ms
             ]
      lns2 = catMaybes lns1
      pfxs = pfx1 : repeat pfxRest
      lns3 = zipWith joinLine pfxs lns2
  in mconcat lns3

-- write outputs

writeOutputs :: MyOpts -> MetaSlaw -> IO ()
writeOutputs mo ms = mapM_ (writeOutput mo ms) (moOutputs mo)

writeOutput :: MyOpts -> MetaSlaw -> AnnOutput -> IO ()
writeOutput mo ms (AnnEntity { aeEnt = ent, aeAnn = Just oe }) =
  wo1 mo (addMeta ms $ entMeta ent) ent oe
writeOutput _  _  _ = return ()

addMeta :: MetaSlaw -> Maybe String -> MetaSlaw
addMeta ms Nothing    = ms
addMeta ms (Just key) = ms { msSlaw = am1 ms key (msSlaw ms) }

am1 :: MetaSlaw -> String -> Slaw -> Slaw
am1 ms key (SlawMap     pairs) = addMetaMap     ms key pairs
am1 ms key (SlawProtein d i r) = addMetaProtein ms key d i r
am1 _  _   s                   = s

addMetaProtein
  :: MetaSlaw
  -> String
  -> Maybe Slaw
  -> Maybe Slaw
  -> RudeData
  -> Slaw
addMetaProtein ms key d i r = SlawProtein d (Just $ amp1 ms key i) r

amp1 :: MetaSlaw -> String -> Maybe Slaw -> Slaw
amp1 ms key Nothing                = addMetaMap ms key []
amp1 ms key (Just SlawNil)         = addMetaMap ms key []
amp1 ms key (Just (SlawMap pairs)) = addMetaMap ms key pairs
amp1 _  _   (Just s)               = s

addMetaMap :: MetaSlaw -> String -> [(Slaw, Slaw)] -> Slaw
addMetaMap ms keyStr pairs1 =
  let key    = toSlaw keyStr
      pairs2 = filter (notKey key) pairs1
      meta   = makeMeta ms
  in SlawMap $ pairs2 ++ [(key, meta)]

notKey :: Slaw -> (Slaw, Slaw) -> Bool
notKey key (s, _) = key /= s

makeMeta :: MetaSlaw -> Slaw
makeMeta ms = SlawMap $ catMaybes
  [ Just ("index",  toSlaw (msIndex  ms))
  , fmap makeMetaTS (msTimestamp ms)
  , Just ("source", toSlaw (msSource ms))
  ]

makeMetaTS :: PoolTimestamp -> (Slaw, Slaw)
makeMetaTS ts = ("timestamp", toSlaw ts)

wo1 :: MyOpts -> MetaSlaw -> IoEntity -> OutputEntity -> IO ()
wo1 mo ms ent (OutPool   h       ) = write2Pool   mo ms ent h
wo1 mo ms ent (OutFile   h _ c af) = write2File   mo ms ent h c af
wo1 mo ms ent (OutStream h so    ) = write2Stream mo ms ent h so

write2Pool :: MyOpts -> MetaSlaw -> IoEntity -> Hose -> IO ()
write2Pool _ ms _ h = void $ deposit h $ ensureProtein $ msSlaw ms

ensureProtein :: Slaw -> Slaw
ensureProtein p@(SlawProtein {}) = p
ensureProtein s                  = SlawProtein Nothing (Just s) mempty

write2File
  :: MyOpts
  -> MetaSlaw
  -> IoEntity
  -> Handle
  -> IORef Integer
  -> Bool
  -> IO ()
write2File mo ms ent h ref af = do
  let spew = spewOverview $ msSlaw ms
      gopt = moGlobal mo
  count <- incrRef ref
  when (count > 0)          $ hPutStrLn h ""
  when (not $ entQuiet ent) $ writeFileComment gopt h ms
  LT.hPutStrLn h spew
  when af                   $ hFlush h

incrRef :: IORef Integer -> IO Integer
incrRef ref = do
  count <- readIORef ref
  let newCount = count + 1
  evaluate newCount
  writeIORef ref newCount
  return count

writeFileComment :: GlobalOpts -> Handle -> MetaSlaw -> IO ()
writeFileComment gopt h ms = do
  let pfx  = "*** "
      meta = R.toLazyText $ fmtLocation gopt pfx pfx ms
  LT.hPutStrLn h meta

write2Stream
  :: MyOpts
  -> MetaSlaw
  -> IoEntity
  -> Handle
  -> SlawOutputStream
  -> IO ()
write2Stream mo ms ent h so = do
  let gopt = moGlobal mo
  when (wantYamlComment ent) $ writeStreamComment gopt h ms
  soWrite so (msSlaw ms)

wantYamlComment :: IoEntity -> Bool
wantYamlComment ent@(IoEntity { entType = Just IoYaml }) =
  not $ entQuiet ent
wantYamlComment _ = False

writeStreamComment :: GlobalOpts -> Handle -> MetaSlaw -> IO ()
writeStreamComment gopt h ms = do
  let pfx  = "# "
      meta = R.toLazyText $ fmtLocation gopt pfx pfx ms
  LT.hPutStr h meta
