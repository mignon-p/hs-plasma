{-|
Module      : System.Loam.Log
Description : Functions from ob-log.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

A flexible, configurable logging facility.
-}

module System.Loam.Log
  ( -- * Types
    LogLevel           -- opaque type
  , LogCode
  , AppendMode(..)
  , LogDest(..)
  , LogFlag(..)        -- re-export
  , SyslogPriority(..) -- re-export
  , SyslogFacility     -- opaque type
  , SyslogFlag(..)     -- re-export
    -- * Logging functions
  , logCode
  , logMsg
  , logLoc
  , logExcCodeMsg
  , logExcCode
  , logExcMsg
  , logExc
    -- * Predefined log levels
  , lvBug
  , lvError
  , lvDeprecation
  , lvWarn
  , lvInfo
  , lvDebug
    -- * Log level functions
  , newLogLevel
  , levelSetPrefix
  , levelGetPrefix
  , levelMaxPrefixBytes
  , levelModifyFlags
  , levelGetFlags
  , levelSetDestFile
  , levelSetSyslogPriority
  , levelGetSyslogPriority
  , levelSetSyslogFacility
  , levelGetSyslogFacility
    -- * Syslog
  , syslogOpen
  , syslogMask
  , syslogClose
    -- ** Sylog facilities
  , facilityNames
  , facilityFromName
  , facilityToName
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
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Filename
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.IoeRetorts

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level"
    c_log_level :: CChar -> IO (Ptr FgnLogLvl)

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_alloc"
    c_log_level_alloc :: IO (Ptr FgnLogLvl)

foreign import capi "ze-hs-log.h &ze_hs_log_level_free"
    c_log_level_free :: FunPtr (Ptr FgnLogLvl -> IO ())

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_flags"
    c_log_level_set_flags :: Ptr FgnLogLvl -> Word32 -> Word32 -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_flags"
    c_log_level_get_flags :: Ptr FgnLogLvl -> IO Word32

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_prefix"
    c_log_level_set_prefix :: Ptr FgnLogLvl -> C.ConstCString -> CSize -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_prefix"
    c_log_level_get_prefix :: Ptr FgnLogLvl -> CString -> CSize -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_prefix_length"
    c_log_level_prefix_length :: CSize

foreign import capi safe "ze-hs-log.h ze_hs_log_level_set_dest"
    c_log_level_set_dest :: Ptr FgnLogLvl -> CChar -> C.ConstCString -> IO Int64

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_sl_priority"
    c_log_level_set_sl_priority :: Ptr FgnLogLvl -> Int32 -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_sl_priority"
    c_log_level_get_sl_priority :: Ptr FgnLogLvl -> IO Int32

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_set_sl_facility"
    c_log_level_set_sl_facility :: Ptr FgnLogLvl -> Int32 -> IO ()

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level_get_sl_facility"
    c_log_level_get_sl_facility :: Ptr FgnLogLvl -> IO Int32

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

-- | Calls the POSIX function @closelog()@.
foreign import capi safe "ze-hs-syslog.h closelog"
    syslogClose :: IO ()

foreign import capi safe "ze-hs-log.h ze_hs_log_loc"
    c_log_loc
      :: C.ConstCString -- file name
      -> Int64          -- line number
      -> Ptr FgnLogLvl  -- log level
      -> Word64         -- code
      -> CString        -- message (writable)
      -> C.ConstCString -- thread
      -> C.ConstCString -- backtrace
      -> IO ()

-- | Represents a log level.  Can be one of the six predefined log
-- levels: 'lvBug', 'lvError', 'lvDeprecation', 'lvWarn', 'lvInfo',
-- or 'lvDebug', or it can be a log level created with 'newLogLevel'.
data LogLevel = LogLevel
  { llName :: !T.Text
  , llPtr  :: !(ForeignPtr FgnLogLvl)
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

instance Nameable LogLevel where
  typeName _ = "LogLevel"

lev2Int :: LogLevel -> Int
lev2Int = fPtrToIntegral . llPtr

mkStaticLevel :: Char -> T.Text -> IO LogLevel
mkStaticLevel c name = do
  initialize
  ptr  <- c_log_level $ fromIntegral $ ord c
  when (ptr == nullPtr) $ do
    fail $ "mkStaticLevel: nullPtr for " ++ show c
  fptr <- newForeignPtr_ ptr
  return $ LogLevel { llName = name
                    , llPtr  = fptr
                    }

-- | A programming error
lvBug         :: LogLevel
lvBug         = unsafePerformIO $ mkStaticLevel 'B' "Bug"

-- | An error caused by the user, or something external (file, network)
lvError       :: LogLevel
lvError       = unsafePerformIO $ mkStaticLevel 'E' "Error"

-- | A warning about use of a deprecated function
lvDeprecation :: LogLevel
lvDeprecation = unsafePerformIO $ mkStaticLevel 'D' "Deprecation"

-- | A warning caused by the user, or something external (file, network)
lvWarn        :: LogLevel
lvWarn        = unsafePerformIO $ mkStaticLevel 'W' "Warn"

-- | Information (you might want to know, but you could safely ignore)
lvInfo        :: LogLevel
lvInfo        = unsafePerformIO $ mkStaticLevel 'I' "Info"

-- | Debugging (very verbose, normally not printed)
lvDebug       :: LogLevel
lvDebug       = unsafePerformIO $ mkStaticLevel 'G' "Debug"

-- | A “code” is a unique 64-bit identifier which indicates which
-- message is being logged.  There are at least three benefits to
-- specifying such an identifier for each of your log messages:
--
--   1. Allows individual messages to be turned on or off from the
--      @OB_LOG@ environment variable.  And since the bits in “code”
--      are allocated hierarchically, by using a mask you can also
--      enable or disable logging for entire modules at once.
--
--   2. Log messages can be suppressed once they have been printed
--      a certain number of times, to avoid spamming the user.  The
--      unique code identifies messages for this purpose.
--
--   3. The code can optionally be printed with the message, which may
--      make customer support easier, since they can quote a specific
--      number, rather than reading back the text of the message and
--      possibly getting it wrong.
type LogCode = Word64

-- | When logging to a file, indicates whether to append to the
-- existing file, or overwrite the existing file.  (If the file does
-- not exist, then these behave the same.)
data AppendMode = Append | Overwrite
                deriving (Eq, Ord, Show, Read, Bounded, Enum,
                          Generic, NFData, Hashable)

-- | Indicates a file to log to.  Can be standard output, standard
-- error, or a named file.  A named file can be specified with
-- either a 'FilePath' or an 'O.OsPath', and it is possible to
-- either 'Append' to the file, or 'Overwrite' the file if it exists.
data LogDest = DestNone
             | DestStdout
             | DestStderr
             | DestFilePath !AppendMode FilePath
             | DestOsPath   !AppendMode O.OsPath
             deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- | Represents a syslog facility.  Can be created with
-- 'facilityFromName'.
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
codeFromRetort IOERR_UNKNOWN                 = Just $ unkIoeCode
codeFromRetort IOERR_ALREADY_EXISTS          = Just $ ioeCode 0x01
codeFromRetort IOERR_ALREADY_IN_USE          = Just $ ioeCode 0x02
codeFromRetort IOERR_DOES_NOT_EXIST          = Just $ ioeCode 0x03
codeFromRetort IOERR_EOF                     = Just $ ioeCode 0x04
codeFromRetort IOERR_FULL                    = Just $ ioeCode 0x05
codeFromRetort IOERR_HARDWARE_FAULT          = Just $ ioeCode 0x06
codeFromRetort IOERR_ILLEGAL_OPERATION       = Just $ ioeCode 0x07
codeFromRetort IOERR_INAPPROPRIATE_TYPE      = Just $ ioeCode 0x08
codeFromRetort IOERR_INTERRUPTED             = Just $ ioeCode 0x09
codeFromRetort IOERR_INVALID_ARGUMENT        = Just $ ioeCode 0x0a
codeFromRetort IOERR_PERMISSION              = Just $ ioeCode 0x0b
codeFromRetort IOERR_PROTOCOL_ERROR          = Just $ ioeCode 0x0c
codeFromRetort IOERR_RESOURCE_VANISHED       = Just $ ioeCode 0x0d
codeFromRetort IOERR_SYSTEM_ERROR            = Just $ ioeCode 0x0e
codeFromRetort IOERR_TIMEOUT                 = Just $ ioeCode 0x0f
codeFromRetort IOERR_UNSATISFIED_CONSTRAINTS = Just $ ioeCode 0x10
codeFromRetort IOERR_UNSUPPORTED_OPERATION   = Just $ ioeCode 0x11
codeFromRetort IOERR_USER                    = Just $ ioeCode 0x12
codeFromRetort _                             = Nothing

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

-- | Like 'logCode', but uses a 'CallStack' passed as an argument,
-- rather than the current call stack, when determining the file
-- and line number for 'FlgShowWhere'.
logLoc
  :: CallStack
  -> LogLevel
  -> LogCode
  -> T.Text
  -> IO ()
logLoc = logInternal

-- | Logs to a given 'LogLevel', using the specified 'LogCode'
-- and message.
logCode
  :: HasCallStack
  => LogLevel
  -> LogCode
  -> T.Text
  -> IO ()
logCode = logInternal callStack

-- | Like logCode, but passes 0 as the 'LogCode'.
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

-- | Logs a given 'Exception' to the specified 'LogLevel',
-- using the specified 'LogCode' and the specified message.
logExcCodeMsg
  :: (HasCallStack, Exception e)
  => LogLevel
  -> LogCode
  -> T.Text
  -> e
  -> IO ()
logExcCodeMsg = logExcInternal callStack

-- | Like 'logExcCodeMsg', but without any message other than the
-- exception itself.
logExcCode
  :: (HasCallStack, Exception e)
  => LogLevel
  -> LogCode
  -> e
  -> IO ()
logExcCode lev code = logExcInternal callStack lev code LT.empty

-- | Like 'logExcCodeMsg', but the 'LogCode' is 0.
logExcMsg
  :: (HasCallStack, Exception e)
  => LogLevel
  -> T.Text
  -> e
  -> IO ()
logExcMsg lev = logExcInternal callStack lev 0

-- | Like 'logExcCodeMsg', but with neither a 'LogCode' nor a message.
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

-- | Creates a new log level, given the specified name.
-- The name is used in the 'Show' instance.
--
-- Also, the prefix is initially set to the name, followed by
-- a colon and space.  However, the prefix can be changed
-- with 'levelSetPrefix'.
newLogLevel :: HasCallStack => T.Text -> IO LogLevel
newLogLevel name0 = do
  initialize
  name <- nonEmptyName "LogLevel" name0 callStack
  ptr  <- c_log_level_alloc
  when (ptr == nullPtr) $ do
    let addn = Just "newLogLevel"
    throwRetortCS_ EtOther addn OB_NO_MEM Nothing callStack
  fptr <- newForeignPtr c_log_level_free ptr
  let lvl = LogLevel { llName = name
                     , llPtr  = fptr
                     }
  when (not $ T.null name0) $ do
    levelSetPrefix lvl $ name0 <> ": "
  return lvl

-- | Sets the prefix for the 'LogLevel', which is printed at the
-- beginning of every line for that log level.  No space is
-- printed after the prefix, so the prefix should probably end
-- with a space.  (Or possibly a colon and a space.)
levelSetPrefix :: LogLevel -> T.Text -> IO ()
levelSetPrefix lev pfx = do
  let bs = T.encodeUtf8 pfx
  withForeignPtr (llPtr lev) $ \levPtr -> do
    C.useAsConstCStringLen bs $ \(pfxPtr, pfxLen) -> do
      c_log_level_set_prefix levPtr pfxPtr (fromIntegral pfxLen)

-- | Gets the current prefix for the given 'LogLevel'.
levelGetPrefix :: LogLevel -> IO T.Text
levelGetPrefix lev = do
  let bufLen = 32
  withForeignPtr (llPtr lev) $ \levPtr -> do
    allocaBytes bufLen $ \bufPtr -> do
      c_log_level_get_prefix levPtr bufPtr (fromIntegral bufLen)
      bs <- B.packCString bufPtr
      return $ T.decodeUtf8With T.lenientDecode bs

-- | The maximum number of UTF-8 bytes allowed in the prefix.
-- Prefixes longer than this will be truncated.
levelMaxPrefixBytes :: Int
levelMaxPrefixBytes =
  fromIntegral c_log_level_prefix_length - 1   -- account for NUL byte

-- | Sets and/or clears flags on the given 'LogLevel'.
levelModifyFlags
  :: LogLevel  -- ^ log level to modify
  -> [LogFlag] -- ^ flags to set
  -> [LogFlag] -- ^ flags to clear
  -> IO ()
levelModifyFlags lev setFlags clearFlags = do
  let setMask   = flagsToMask setFlags
      clearMask = flagsToMask clearFlags
      allMask   = setMask .|. clearMask
  withForeignPtr (llPtr lev) $ \levPtr -> do
    c_log_level_set_flags levPtr setMask allMask

-- | Gets the current flags for the specified 'LogLevel'.
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

-- | Configures the given 'LogLevel' to log to the given
-- 'LogDest' when 'DstFd' is set.
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
      throwRetortCS_ EtOther addn (Retort tort) (Just erl) cs

-- | Configures the given 'LogLevel' to use the given
-- 'SyslogPriority' when 'DstSyslog' is set.
levelSetSyslogPriority :: LogLevel -> SyslogPriority -> IO ()
levelSetSyslogPriority lev pri = do
  let pri' = syslogPriority2int pri
  withForeignPtr (llPtr lev) $ \levPtr -> do
    c_log_level_set_sl_priority levPtr pri'

int2pri :: IM.IntMap SyslogPriority
int2pri = IM.fromList $ map f [minBound..maxBound]
  where f pri = (fromIntegral (syslogPriority2int pri), pri)

-- | Returns the current 'SyslogPriority' for this 'LogLevel'.
levelGetSyslogPriority :: LogLevel -> IO SyslogPriority
levelGetSyslogPriority lev = do
  withForeignPtr (llPtr lev) $ \levPtr -> do
    n <- c_log_level_get_sl_priority levPtr
    return $ IM.findWithDefault LogInfo (fromIntegral n) int2pri

-- | Configures the given 'LogLevel' to use the given
-- 'SyslogFacility' when 'DstSyslog' is set.
-- 'Nothing' means to use the facility specified when
-- 'syslogOpen' was called.
levelSetSyslogFacility :: LogLevel -> Maybe SyslogFacility -> IO ()
levelSetSyslogFacility lev fac = do
  withForeignPtr (llPtr lev) $ \levPtr -> do
    c_log_level_set_sl_facility levPtr $ mf2i32 fac

mf2i32 :: Maybe SyslogFacility -> Int32
mf2i32 (Just (SyslogFacility fac)) = fac
mf2i32 _                           = 0

-- | Returns the current 'SyslogFacility' for this 'LogLevel'.
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

-- | List the names of the available syslog facilities.
facilityNames :: [T.Text]
facilityNames = map snd $ IM.toAscList facNum2Name

-- | Return a syslog facility, given a name.
facilityFromName :: T.Text -> Maybe SyslogFacility
facilityFromName key = HM.lookup key' facName2Num
  where key' = T.toCaseFold key

-- | Return a name, given a syslog facility.
facilityToName :: SyslogFacility -> T.Text
facilityToName (SyslogFacility n) =
  let dflt = T.pack $ printf "facility 0x%02x" n
  in IM.findWithDefault dflt (fromIntegral n) facNum2Name

-- | Calls the POSIX function @openlog()@.
syslogOpen :: Maybe T.Text -> [SyslogFlag] -> SyslogFacility -> IO ()
syslogOpen ident flags (SyslogFacility fac) = do
  initialize
  let flagMask = orList $ map syslogFlag2int flags
  syslogOpen0 ident flagMask fac

syslogOpen0 :: Maybe T.Text -> CInt -> Int32 -> IO ()
syslogOpen0 Nothing flags fac = c_syslog_open C.nullConstPtr flags fac
syslogOpen0 (Just ident) flags fac = do
  C.useAsConstCString (T.encodeUtf8 ident) $ \identPtr -> do
    c_syslog_open identPtr flags fac

-- | Calls the POSIX function @setlogmask()@.
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
