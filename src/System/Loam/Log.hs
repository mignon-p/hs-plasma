{-|
Module      : System.Loam.Log
Description : Functions from ob-log.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Log
  ( LogLevel           -- opaque type
  , LogCode
  , AppendMode(..)
  , LogDest(..)
  , LogFlag(..)        -- re-export
  , SyslogPriority(..) -- re-export
  , SyslogFacility     -- opaque type
  , logLoc
  , logCode
  , logMsg
  , logExcCodeMsg
  , logExcCode
  , logExcMsg
  , logExc
    --
  , lvBug
  , lvError
  , lvDeprecation
  , lvWarn
  , lvInfo
  , lvDebug
    --
  , makeLogLevel
  , levelSetPrefix
  , levelGetPrefix
  , levelModifyFlags
  , levelGetFlags
  , levelSetDestFile
  , levelSetSyslogPriority
  , levelGetSyslogPriority
  , levelSetSyslogFacility
  , levelGetSyslogFacility
    --
  , facilityNames
  , facilityFromName
  , facilityToName
    --
  , syslogOpen
  , syslogMask
  , syslogClose
  ) where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString          as B
import qualified Data.ByteString.Unsafe   as B
import Data.Char
import Data.Default.Class
import Data.Hashable
import qualified Data.HashMap.Strict      as HM
import Data.Int
import qualified Data.IntMap.Strict       as IM
import Data.List
import Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy           as LT
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
-- import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
import System.IO.Error
import System.IO.Unsafe
import qualified System.OsPath            as O
import Text.Printf

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Enums
import System.Loam.Internal.Filename
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.IoeRetorts

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level"
    c_log_level :: CChar -> IO (Ptr ())

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_alloc"
    c_log_level_alloc :: IO (Ptr ())

foreign import capi "ze-hs-log.h &ze_hs_log_level_free"
    c_log_level_free :: FunPtr (Ptr () -> IO ())

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_flags"
    c_log_level_set_flags :: Ptr () -> Word32 -> Word32 -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_flags"
    c_log_level_get_flags :: Ptr () -> IO Word32

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_prefix"
    c_log_level_set_prefix :: Ptr () -> C.ConstCString -> CSize -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_prefix"
    c_log_level_get_prefix :: Ptr () -> CString -> CSize -> IO ()

foreign import capi safe "ze-hs-log.h ze_hs_log_level_set_dest"
    c_log_level_set_dest :: Ptr () -> CChar -> C.ConstCString -> IO Int64

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_sl_priority"
    c_log_level_set_sl_priority :: Ptr () -> Int32 -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_sl_priority"
    c_log_level_get_sl_priority :: Ptr () -> IO Int32

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_sl_facility"
    c_log_level_set_sl_facility :: Ptr () -> Int32 -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_sl_facility"
    c_log_level_get_sl_facility :: Ptr () -> IO Int32

foreign import capi unsafe "ze-hs-log.h ze_hs_facility_name"
    c_facility_name :: CSize -> Ptr Int32 -> IO C.ConstCString

foreign import capi unsafe "ze-hs-log.h ze_hs_default_facility"
    c_default_facility :: Int32

foreign import capi safe "ze-hs-log.h ze_hs_syslog_open"
    c_syslog_open :: C.ConstCString -> CInt -> Int32 -> IO ()

foreign import capi unsafe "ze-hs-syslog.h LOG_MASK"
    c_log_mask :: Int32 -> CInt

foreign import capi safe "ze-hs-syslog.h setlogmask"
    c_setlogmask :: CInt -> IO CInt

foreign import capi safe "ze-hs-syslog.h closelog"
    syslogClose :: IO ()

foreign import capi safe "ze-hs-log.h ze_hs_log_loc"
    c_log_loc
      :: C.ConstCString -- file name
      -> Int64          -- line number
      -> Ptr ()         -- log level
      -> Word64         -- code
      -> CString        -- message (writable)
      -> C.ConstCString -- thread
      -> C.ConstCString -- backtrace
      -> IO ()

data LogLevel = LogLevel
  { llName :: !T.Text
  , llPtr  :: !(ForeignPtr ())
  } deriving (Eq, Ord)

instance NFData LogLevel where
  rnf (LogLevel x y) = x `seq` y `seq` ()

instance Hashable LogLevel where
  hash                lev = hashInt        $ lev2Int lev
  salt `hashWithSalt` lev = salt `hash2xInt` lev2Int lev

instance Default LogLevel where
  def = lvInfo

instance Show LogLevel where
  show lev = fmtForeignObj "LogLevel" (llName lev) [] (llPtr lev)

lev2Int :: LogLevel -> Int
lev2Int = fPtrToIntegral . llPtr

mkStaticLevel :: Char -> T.Text -> IO LogLevel
mkStaticLevel c name = do
  ptr  <- c_log_level $ fromIntegral $ ord c
  when (ptr == nullPtr) $ do
    fail $ "mkStaticLevel: nullPtr for " ++ show c
  fptr <- newForeignPtr_ ptr
  return $ LogLevel { llName = name
                    , llPtr  = fptr
                    }

lvBug, lvError, lvDeprecation, lvWarn, lvInfo, lvDebug :: LogLevel

lvBug         = unsafePerformIO $ mkStaticLevel 'B' "Bug"
lvError       = unsafePerformIO $ mkStaticLevel 'E' "Error"
lvDeprecation = unsafePerformIO $ mkStaticLevel 'D' "Deprecation"
lvWarn        = unsafePerformIO $ mkStaticLevel 'W' "Warn"
lvInfo        = unsafePerformIO $ mkStaticLevel 'I' "Info"
lvDebug       = unsafePerformIO $ mkStaticLevel 'G' "Debug"

type LogCode = Word64

data AppendMode = Append | Overwrite
                deriving (Eq, Ord, Show, Read, Bounded, Enum,
                          Generic, NFData, Hashable)

data LogDest = DestNone
             | DestStdout
             | DestStderr
             | DestFilePath !AppendMode FilePath
             | DestOsPath   !AppendMode O.OsPath
             deriving (Eq, Ord, Show, Generic, NFData, Hashable)

newtype SyslogFacility = SyslogFacility Int32
                       deriving (Eq, Ord)

instance NFData SyslogFacility where
  rnf (SyslogFacility n) = n `seq` ()

instance Hashable SyslogFacility where
  hash                (SyslogFacility n) = hashInt        $ fromIntegral n
  salt `hashWithSalt` (SyslogFacility n) = salt `hash2xInt` fromIntegral n

instance Default SyslogFacility where
  def = SyslogFacility $ c_default_facility

instance Show SyslogFacility where
  show (SyslogFacility n) = "{SyslogFacility: " ++ info ++ "}"
    where
      hexFac = printf "<0x%02x>" n
      info   = case IM.lookup (fromIntegral n) facNum2Name of
                 Nothing   -> hexFac
                 Just name ->
                   let escName = showEscapedStr $ T.unpack name
                   in escName ++ " " ++ hexFac

excCode, etCode, ioeCode :: Word64 -> LogCode
excCode = (0xa000_0000 .|.)
etCode  = (0xa000_0100 .|.)
ioeCode = (0xa000_0200 .|.)

unkExcCode, errCallCode, unkIoeCode :: LogCode
unkExcCode  = excCode 0
errCallCode = excCode 1
unkIoeCode  = ioeCode 0

btFromExc
  :: SomeException
  -> (String, Maybe CallStack, Maybe String, Word64)
btFromExc exc =
  let io   = fmap btFromIoExc     $ fromException exc
      err  = fmap btFromErrCall   $ fromException exc
      plas = fmap btFromPlasmaExc $ fromException exc
      dflt = (displayException exc, Nothing, Nothing, unkExcCode)
  in (io <|> err <|> plas) ?> dflt

btFromIoExc
  :: IOException
  -> (String, Maybe CallStack, Maybe String, Word64)
btFromIoExc ioe =
  (displayException ioe, Nothing, Nothing, code ?> unkIoeCode)
  where code = codeFromRetort $ ioetToRetort $ ioeGetErrorType ioe

btFromErrCall
  :: ErrorCall
  -> (String, Maybe CallStack, Maybe String, Word64)
btFromErrCall (ErrorCallWithLocation msg bt) =
  let bt' = if null bt then Nothing else Just bt
  in (msg, Nothing, bt', errCallCode)

btFromPlasmaExc
  :: PlasmaException
  -> (String, Maybe CallStack, Maybe String, Word64)
btFromPlasmaExc pe =
  let code2 = codeFromEt (peType pe)
      code1 = peRetort pe >>= codeFromRetort
      code  = code1 ?> code2
      msg   = displayPlasmaException False pe
      cs    = peCallstack pe
  in (msg, cs, fmap prettyCallStack cs, code)

codeFromEt :: PlasmaExceptionType -> Word64
codeFromEt EtCorruptSlaw     = etCode 1
codeFromEt EtTypeMismatch    = etCode 2
codeFromEt EtRangeError      = etCode 3
codeFromEt EtInvalidArgument = etCode 4
codeFromEt EtValidationError = etCode 5
codeFromEt EtUnicodeError    = etCode 6
codeFromEt EtNotFound        = etCode 7
codeFromEt EtSlawIO          = etCode 8
codeFromEt EtPools           = etCode 9
codeFromEt _                 = etCode 0

codeFromRetort :: Retort -> Maybe Word64
codeFromRetort ZE_HS_IOE_UNKNOWN                 = Just $ unkIoeCode
codeFromRetort ZE_HS_IOE_ALREADY_EXISTS          = Just $ ioeCode 0x01
codeFromRetort ZE_HS_IOE_ALREADY_IN_USE          = Just $ ioeCode 0x02
codeFromRetort ZE_HS_IOE_DOES_NOT_EXIST          = Just $ ioeCode 0x03
codeFromRetort ZE_HS_IOE_EOF                     = Just $ ioeCode 0x04
codeFromRetort ZE_HS_IOE_FULL                    = Just $ ioeCode 0x05
codeFromRetort ZE_HS_IOE_HARDWARE_FAULT          = Just $ ioeCode 0x06
codeFromRetort ZE_HS_IOE_ILLEGAL_OPERATION       = Just $ ioeCode 0x07
codeFromRetort ZE_HS_IOE_INAPPROPRIATE_TYPE      = Just $ ioeCode 0x08
codeFromRetort ZE_HS_IOE_INTERRUPTED             = Just $ ioeCode 0x09
codeFromRetort ZE_HS_IOE_INVALID_ARGUMENT        = Just $ ioeCode 0x0a
codeFromRetort ZE_HS_IOE_PERMISSION              = Just $ ioeCode 0x0b
codeFromRetort ZE_HS_IOE_PROTOCOL_ERROR          = Just $ ioeCode 0x0c
codeFromRetort ZE_HS_IOE_RESOURCE_VANISHED       = Just $ ioeCode 0x0d
codeFromRetort ZE_HS_IOE_SYSTEM_ERROR            = Just $ ioeCode 0x0e
codeFromRetort ZE_HS_IOE_TIMEOUT                 = Just $ ioeCode 0x0f
codeFromRetort ZE_HS_IOE_UNSATISFIED_CONSTRAINTS = Just $ ioeCode 0x10
codeFromRetort ZE_HS_IOE_UNSUPPORTED_OPERATION   = Just $ ioeCode 0x11
codeFromRetort ZE_HS_IOE_USER                    = Just $ ioeCode 0x12
codeFromRetort _                                 = Nothing

fileLineFromCs
  :: CallStack
  -> (String, Int64)
fileLineFromCs cs =
  case getCallStack cs of
    ((_, srcLoc):_) -> ( srcLocFile srcLoc
                       , fromIntegral (srcLocStartLine srcLoc)
                       )
    _               -> ( "unknown", 0 )

logInternal0
  :: TextClass a
  => (String, Int64) -- file and line number
  -> String          -- backtrace
  -> LogLevel
  -> LogCode
  -> a
  -> IO ()
logInternal0 (fname, lineNo) bt lev code msg = do
  thr <- fmtThread
  withLazyByteStringAsCString (toUtf8 fname) $ \fnamePtr -> do
    withLazyByteStringAsCString (toUtf8 bt) $ \btPtr -> do
      withLazyByteStringAsCString (toUtf8 thr) $ \thrPtr -> do
        withLazyByteStringAsCStringNL (toUtf8 msg) $ \msgPtr -> do
          withForeignPtr (llPtr lev) $ \pLev -> do
            let cFn = C.ConstPtr fnamePtr
                cBt = C.ConstPtr btPtr
                cTh = C.ConstPtr thrPtr
            c_log_loc cFn lineNo pLev code msgPtr cTh cBt

logInternal
  :: TextClass a
  => CallStack
  -> LogLevel
  -> LogCode
  -> a
  -> IO ()
logInternal cs = logInternal0 (fileLineFromCs cs) (prettyCallStack cs)

logLoc
  :: CallStack
  -> LogLevel
  -> LogCode
  -> T.Text
  -> IO ()
logLoc = logInternal

logCode
  :: HasCallStack
  => LogLevel
  -> LogCode
  -> T.Text
  -> IO ()
logCode = logInternal callStack

logMsg
  :: HasCallStack
  => LogLevel
  -> T.Text
  -> IO ()
logMsg lev = logInternal callStack lev 0

logExcInternal0
  :: TextClass a
  => CallStack
  -> LogLevel
  -> LogCode
  -> a
  -> SomeException
  -> String
  -> IO ()
logExcInternal0 cs lev code msg exc excName = do
  let (excMsg, mExcCs, mExcBt, exCode) = btFromExc exc
      cs'                              = mExcCs ?> cs
      bt                               = mExcBt ?> prettyCallStack cs'
      fileLine                         = fileLineFromCs cs'
      code' = case code of
                0 -> exCode
                _ -> code
      lmsg  = toLazyText msg
      ind   = indentLines (toLazyText stdIndent)
      eMsg0 = toLazyText excMsg
      eMsg1 = case null excName of
                True  -> eMsg0
                False -> toLazyText excName <> ":\n" <> ind eMsg0
      msg'  = case LT.null lmsg of
                True  -> eMsg1
                False -> lmsg <> "\n" <> eMsg1
  logInternal0 fileLine bt lev code' msg'

logExcInternal
  :: (TextClass a, Exception e)
  => CallStack
  -> LogLevel
  -> LogCode
  -> a
  -> e
  -> IO ()
logExcInternal cs lev code msg exc = do
  let se      = toException exc
      excName = tyConName $ typeRepTyCon $ typeOf exc
  logExcInternal0 cs lev code msg se excName

logExcCodeMsg
  :: (HasCallStack, Exception e)
  => LogLevel
  -> LogCode
  -> T.Text
  -> e
  -> IO ()
logExcCodeMsg = logExcInternal callStack

logExcCode
  :: (HasCallStack, Exception e)
  => LogLevel
  -> LogCode
  -> e
  -> IO ()
logExcCode lev code = logExcInternal callStack lev code LT.empty

logExcMsg
  :: (HasCallStack, Exception e)
  => LogLevel
  -> T.Text
  -> e
  -> IO ()
logExcMsg lev = logExcInternal callStack lev 0

logExc
  :: (HasCallStack, Exception e)
  => LogLevel
  -> e
  -> IO ()
logExc lev = logExcInternal callStack lev 0 LT.empty

fmtThread :: IO String
fmtThread = do
  tid           <- myThreadId
  (cap, locked) <- threadCapability tid
  bound         <- isCurrentThreadBound
  let pfxChr    = if bound then 'B' else 'H'
      tidStr    = show tid
      tidPfx    = "ThreadId "
      tidPfxLen = length tidPfx
      tidDrop   = if tidPfx `isPrefixOf` tidStr then tidPfxLen else 0
      tidStr'   = drop tidDrop tidStr
      capSfx    = if locked then ":C" ++ show cap else ""
  return $ pfxChr : (tidStr' ++ capSfx)

makeLogLevel :: HasCallStack => T.Text -> IO LogLevel
makeLogLevel name = do
  ptr  <- c_log_level_alloc
  when (ptr == nullPtr) $ do
    let addn = Just "makeLogLevel"
    throwRetortCS EtOther addn OB_NO_MEM Nothing callStack
  fptr <- newForeignPtr c_log_level_free ptr
  let lvl = LogLevel { llName = name
                     , llPtr  = fptr
                     }
  levelSetPrefix lvl $ name <> ": "
  return lvl

levelSetPrefix :: LogLevel -> T.Text -> IO ()
levelSetPrefix lev pfx = do
  let bs = T.encodeUtf8 pfx
  withForeignPtr (llPtr lev) $ \levPtr -> do
    C.useAsConstCStringLen bs $ \(pfxPtr, pfxLen) -> do
      c_log_level_set_prefix levPtr pfxPtr (fromIntegral pfxLen)

levelGetPrefix :: LogLevel -> IO T.Text
levelGetPrefix lev = do
  let bufLen = 32
  withForeignPtr (llPtr lev) $ \levPtr -> do
    allocaBytes bufLen $ \bufPtr -> do
      c_log_level_get_prefix levPtr bufPtr (fromIntegral bufLen)
      bs <- B.packCString bufPtr
      return $ T.decodeUtf8With T.lenientDecode bs

levelModifyFlags
  :: LogLevel
  -> [LogFlag] -- ^ flags to set
  -> [LogFlag] -- ^ flags to clear
  -> IO ()
levelModifyFlags lev setFlags clearFlags = do
  let setMask   = flagsToMask setFlags
      clearMask = flagsToMask clearFlags
      allMask   = setMask .|. clearMask
  withForeignPtr (llPtr lev) $ \levPtr -> do
    c_log_level_set_flags levPtr setMask allMask

levelGetFlags :: LogLevel -> IO [LogFlag]
levelGetFlags lev = do
  withForeignPtr (llPtr lev) $ \levPtr -> do
    w32 <- c_log_level_get_flags levPtr
    return $ maskToFlags w32

flagsToMask :: [LogFlag] -> Word32
flagsToMask = orList . map logFlag2w32

maskToFlags :: Word32 -> [LogFlag]
maskToFlags w32 = mapMaybe (maybeFlag w32) [minBound..maxBound]

maybeFlag :: Word32 -> LogFlag -> Maybe LogFlag
maybeFlag w32 flag =
  if (logFlag2w32 flag .&. w32) == 0
  then Nothing
  else Just flag

logDestToBs :: LogDest -> (Char, B.ByteString)
logDestToBs DestNone             = ('N', "(none)")
logDestToBs DestStdout           = ('O', "<stdout>")
logDestToBs DestStderr           = ('E', "<stderr>")
logDestToBs (DestFilePath am fp) = (am2c am, to8bitFn fp)
logDestToBs (DestOsPath   am os) = (am2c am, to8bitFn os)

am2c :: AppendMode -> Char
am2c Append    = 'A'
am2c Overwrite = 'F'

levelSetDestFile :: HasCallStack => LogLevel -> LogDest -> IO ()
levelSetDestFile lev dest = do
  let (c, fn) = logDestToBs dest
      c'      = fromIntegral $ ord c
      cs      = callStack
      addn    = Just "levelSetDestFile"
      erl     = def { elSource = (DsFile . fromUtf8 . fromByteString) fn }
  withForeignPtr (llPtr lev) $ \levPtr -> do
    C.useAsConstCString fn $ \fnPtr -> do
      tort <- c_log_level_set_dest levPtr c' fnPtr
      throwRetortCS EtOther addn (Retort tort) (Just erl) cs

levelSetSyslogPriority :: LogLevel -> SyslogPriority -> IO ()
levelSetSyslogPriority lev pri = do
  let pri' = syslogPriority2int pri
  withForeignPtr (llPtr lev) $ \levPtr -> do
    c_log_level_set_sl_priority levPtr pri'

int2pri :: IM.IntMap SyslogPriority
int2pri = IM.fromList $ map f [minBound..maxBound]
  where f pri = (fromIntegral (syslogPriority2int pri), pri)

levelGetSyslogPriority :: LogLevel -> IO SyslogPriority
levelGetSyslogPriority lev = do
  withForeignPtr (llPtr lev) $ \levPtr -> do
    n <- c_log_level_get_sl_priority levPtr
    return $ IM.findWithDefault LogInfo (fromIntegral n) int2pri

levelSetSyslogFacility :: LogLevel -> Maybe SyslogFacility -> IO ()
levelSetSyslogFacility lev fac = do
  withForeignPtr (llPtr lev) $ \levPtr -> do
    c_log_level_set_sl_facility levPtr $ mf2i32 fac

mf2i32 :: Maybe SyslogFacility -> Int32
mf2i32 (Just (SyslogFacility fac)) = fac
mf2i32 _                           = 0

levelGetSyslogFacility :: LogLevel -> IO (Maybe SyslogFacility)
levelGetSyslogFacility lev = do
  withForeignPtr (llPtr lev) $ \levPtr -> do
    fac <- c_log_level_get_sl_facility levPtr
    if fac == 0
      then return Nothing
      else return $ Just $ SyslogFacility fac

type FacPair = (T.Text, SyslogFacility)

{-# NOINLINE facNames0 #-}
facNames0 :: [FacPair]
facNames0 = unsafePerformIO $ getFacNames 0 []

facNames :: [FacPair]
facNames = filter f facNames0
  -- "mark" is noted as "internal" in syslog.h
  where f ("mark", _) = False
        f _           = True

facName2Num :: HM.HashMap T.Text SyslogFacility
facName2Num = HM.fromListWith keepOld $ map f facNames
  where f = first T.toCaseFold

facNum2Name :: IM.IntMap T.Text
facNum2Name = IM.fromListWith keepOld $ map f facNames
  where f (name, SyslogFacility n) = (fromIntegral n, name)

keepOld :: a -> b -> b
keepOld _ = id

getFacNames :: CSize -> [FacPair] -> IO [FacPair]
getFacNames !idx pairs = do
  (facName, facNum) <- getFacName idx
  let pair = (facName, SyslogFacility facNum)
  case T.null facName || facNum < 0 of
    True  -> return $ reverse pairs
    False -> getFacNames (idx + 1) (pair : pairs)

getFacName :: CSize -> IO (T.Text, Int32)
getFacName !idx = alloca $ \i32Ptr -> do
  poke i32Ptr (-1)
  ccs <- c_facility_name idx i32Ptr
  bs  <- if ccs == C.nullConstPtr
         then return B.empty
         else B.unsafePackCString (C.unConstPtr ccs)
  i32 <- peek i32Ptr
  return (T.decodeUtf8With T.lenientDecode bs, i32)

facilityNames :: [T.Text]
facilityNames = map snd $ IM.toAscList facNum2Name

facilityFromName :: T.Text -> Maybe SyslogFacility
facilityFromName key = HM.lookup key' facName2Num
  where key' = T.toCaseFold key

facilityToName :: SyslogFacility -> T.Text
facilityToName (SyslogFacility n) =
  let dflt = T.pack $ printf "facility 0x%02x" n
  in IM.findWithDefault dflt (fromIntegral n) facNum2Name

syslogOpen :: Maybe T.Text -> [SyslogFlag] -> SyslogFacility -> IO ()
syslogOpen ident flags (SyslogFacility fac) = do
  let flagMask = orList $ map syslogFlag2int flags
  syslogOpen0 ident flagMask fac

syslogOpen0 :: Maybe T.Text -> CInt -> Int32 -> IO ()
syslogOpen0 Nothing flags fac = c_syslog_open C.nullConstPtr flags fac
syslogOpen0 (Just ident) flags fac = do
  C.useAsConstCString (T.encodeUtf8 ident) $ \identPtr -> do
    c_syslog_open identPtr flags fac

syslogMask :: [SyslogPriority] -> IO [SyslogPriority]
syslogMask pris = do
  let priMask = orList $ map pri2mask pris
  oldMask <- c_setlogmask priMask
  return $ mapMaybe (maybePri oldMask) [minBound..maxBound]

pri2mask :: SyslogPriority -> CInt
pri2mask = c_log_mask . syslogPriority2int

maybePri :: CInt -> SyslogPriority -> Maybe SyslogPriority
maybePri priMask pri =
  if (pri2mask pri .&. priMask) == 0
  then Nothing
  else Just pri
