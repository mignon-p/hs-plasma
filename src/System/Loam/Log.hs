{-|
Module      : System.Loam.Log
Description : Functions from ob-log.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Log
  ( LogLevel
  , LogCode
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
  ) where

import Control.Applicative
import Control.Concurrent
-- import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
-- import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
-- import GHC.Generics (Generic)
import GHC.Stack
import System.IO.Error
import System.IO.Unsafe

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Marshal
import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.IoeRetorts

foreign import capi unsafe "ze-hs-log.h ze_hs_log_level"
    c_log_level :: CChar -> IO (Ptr ())

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
  } deriving (Show, Eq, Ord)

instance Hashable LogLevel where
  hash                lev = hashInt        $ lev2Int lev
  salt `hashWithSalt` lev = salt `hash2xInt` lev2Int lev

lev2Int :: LogLevel -> Int
lev2Int = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . llPtr

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

{-
data LogLevel = Bug
              | Error
              | Deprecation
              | Warn
              | Info
              | Debug
              deriving (Eq, Ord, Show, Read, Bounded, Enum,
                         Generic, NFData, Hashable)
-}

type LogCode = Word64

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
