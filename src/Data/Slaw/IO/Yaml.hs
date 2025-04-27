{-|
Module      : Data.Slaw.IO.Yaml
Description : Read and write slawx to/from YAML files
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Adds additional capabilities to "Data.Slaw.IO", by allowing
'SlawInputStream' and 'SlawOutputStream' to be created with YAML
slaw files, in addition to binary slaw files.  The streams can then
be used with existing functions like 'siRead' and 'soWrite'.
-}

module Data.Slaw.IO.Yaml
  ( -- * Reading slawx from a file (YAML or binary)
    openSlawInput
  , withSlawInput
  , readSlawFile
    -- * Writing slawx to a file (YAML or binary)
  , openSlawOutput
  , withSlawOutput
  , writeSlawFile
    -- * Reading slawx from a YAML file
  , openYamlSlawInput
  , withYamlSlawInput
  , readYamlSlawFile
    -- * Writing slawx to a YAML file
  , openYamlSlawOutput
  , withYamlSlawOutput
  , writeYamlSlawFile
    -- * Slawx to/from YAML-encoded strings
  , slawFromYamlString
  , slawToYamlString
  , slawFromYamlStringIO
  , slawToYamlStringIO
    -- * Options
  , WriteYamlOptions(..) -- re-export
  ) where

import Control.Applicative
import Control.Exception
-- import Control.Monad
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as R
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as L8
import Data.Char
import Data.Default.Class
import Data.Int
import Data.IORef
import qualified Data.Text.Lazy                as LT
import Data.Unique
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.IO
import System.IO.Error
import System.IO.Unsafe

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.IO
import Data.Slaw.IO.Internal.Options
import Data.Slaw.Path
import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.IoeRetorts
import System.Loam.Retorts.Internal.RetortUtil

--

type ReadFunc = CChar -> Ptr Word8 -> CSize -> Ptr CSize -> IO Int64
type ReadPtr  = FunPtr ReadFunc

type WriteFunc = CChar -> C.ConstPtr Word8 -> CSize -> IO Int64
type WritePtr  = FunPtr WriteFunc

type InputPtr     = Ptr FgnSlawIn
type OutputPtr    = Ptr FgnSlawOut

type InputFPtr    = ForeignPtr FgnSlawIn
type OutputFPtr   = ForeignPtr FgnSlawOut

type SlawPtr      = Ptr FgnSlaw
type ConstSlawPtr = C.ConstPtr FgnSlaw

foreign import ccall "wrapper" createReadPtr :: ReadFunc -> IO ReadPtr

foreign import ccall "wrapper" createWritePtr :: WriteFunc -> IO WritePtr

--

foreign import capi "ze-hs-slawio.h ze_hs_open_yaml_input"
    c_open_yaml_input :: ReadPtr -> Ptr Int64 -> IO InputPtr

foreign import capi "ze-hs-slawio.h ze_hs_read_input"
    c_read_input :: InputPtr -> Ptr Int64 -> Ptr Int64 -> IO SlawPtr

foreign import capi "ze-hs-slawio.h ze_hs_close_input"
    c_close_input :: InputPtr -> IO Int64

foreign import capi "ze-hs-slawio.h &ze_hs_finalize_input"
    c_finalize_input :: FunPtr (InputPtr -> IO ())

--

foreign import capi "ze-hs-slawio.h ze_hs_open_yaml_output"
    c_open_yaml_output :: WritePtr -> ConstSlawPtr -> Ptr Int64 -> IO OutputPtr

foreign import capi "ze-hs-slawio.h ze_hs_write_output"
    c_write_output :: OutputPtr -> ConstSlawPtr -> IO Int64

foreign import capi "ze-hs-slawio.h ze_hs_close_output"
    c_close_output :: OutputPtr -> IO Int64

foreign import capi "ze-hs-slawio.h &ze_hs_finalize_output"
    c_finalize_output :: FunPtr (OutputPtr -> IO ())

--

-- | Run an action with a 'SlawInputStream'.
withSlawInput
  :: (HasCallStack, FileClass a, ToSlaw b)
  => a                         -- ^ name (or handle) of file to read
  -> b                         -- ^ options map/protein (currently none)
  -> (SlawInputStream -> IO c) -- ^ action to run
  -> IO c
withSlawInput fname opts act = withFrozenCallStack $ do
  bracket (openSlawInput fname opts) siClose act

-- | Convenience function to read all the slawx from a file.
-- Automatically detects whether the file is binary or YAML.
-- It opens a stream, reads all the slawx from the stream,
-- and then closes the stream.  The slawx that were read are returned
-- as a list.
readSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
             => a -- ^ name (or handle) of file to read
             -> b -- ^ options map/protein (currently none)
             -> IO [Slaw]
readSlawFile fname opts = withFrozenCallStack $ do
  withSlawInput fname opts readAllSlawx

-- | Run an action with a 'SlawInputStream'.
withYamlSlawInput
  :: (HasCallStack, FileClass a, ToSlaw b)
  => a                         -- ^ name (or handle) of file to read
  -> b                         -- ^ options map/protein (currently none)
  -> (SlawInputStream -> IO c) -- ^ action to run
  -> IO c
withYamlSlawInput fname opts act = withFrozenCallStack $ do
  bracket (openYamlSlawInput fname opts) siClose act

-- | Convenience function to read all the slawx from a YAML
-- file.  It opens a stream, reads all the slawx from the stream,
-- and then closes the stream.  The slawx that were read are returned
-- as a list.
readYamlSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
                 => a -- ^ name (or handle) of file to read
                 -> b -- ^ options map/protein (currently none)
                 -> IO [Slaw]
readYamlSlawFile fname opts = withFrozenCallStack $ do
  withYamlSlawInput fname opts readAllSlawx

-- | Run an action with a 'SlawOutputStream'.
withYamlSlawOutput
  :: (HasCallStack, FileClass a, ToSlaw b)
  => a                          -- ^ name (or handle) of file to read
  -> b                          -- ^ options map/protein
  -> (SlawOutputStream -> IO c) -- ^ action to run
  -> IO c
withYamlSlawOutput fname opts act = withFrozenCallStack $ do
  bracket (openYamlSlawOutput fname opts) soClose act

-- | Convenience function to write a YAML slaw file all at once.
-- It opens a stream, writes all the slawx to the stream,
-- and then closes the stream.
writeYamlSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
                  => a      -- ^ name (or handle) of file to write
                  -> b      -- ^ options map/protein
                  -> [Slaw] -- ^ slawx to write to file
                  -> IO ()
writeYamlSlawFile fname opts ss = withFrozenCallStack $ do
  withYamlSlawOutput fname opts $ \sos -> mapM_ (soWrite sos) ss

-- | Run an action with a 'SlawOutputStream'.
withSlawOutput
  :: (HasCallStack, FileClass a, ToSlaw b)
  => a                          -- ^ name (or handle) of file to read
  -> b                          -- ^ options map/protein
  -> (SlawOutputStream -> IO c) -- ^ action to run
  -> IO c
withSlawOutput fname opts act = withFrozenCallStack $ do
  bracket (openSlawOutput fname opts) soClose act

-- | Convenience function to write a slaw file all at once.
-- It opens a stream, writes all the slawx to the stream,
-- and then closes the stream.
--
-- See 'openSlawOutput' for how it chooses to write either
-- a binary or YAML slaw file, depending on the options map
-- or the file extension.
writeSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
              => a      -- ^ name (or handle) of file to write
              -> b      -- ^ options map/protein
              -> [Slaw] -- ^ slawx to write to file
              -> IO ()
writeSlawFile fname opts ss = withFrozenCallStack $ do
  withSlawOutput fname opts $ \sos -> mapM_ (soWrite sos) ss

--

fileMagicLBS :: L.ByteString
fileMagicLBS = R.toLazyByteString $ R.word32BE fileMagic

-- | Opens a 'SlawInputStream' for reading slawx from a file.
-- Automatically detects whether the file is binary or YAML.
-- If an error occurs, may throw 'IOException' or 'PlasmaException'.
--
-- Does not currently take any options, so the second argument is
-- placeholder which is just ignored.  The easiest thing to do
-- is just pass in @()@.
openSlawInput :: (HasCallStack, FileClass a, ToSlaw b)
              => a -- ^ name (or handle) of file to open
              -> b -- ^ options map/protein (currently none)
              -> IO SlawInputStream
openSlawInput file opts = do
  let cs = callStack
  eth  <- fcOpenReadOrMap file
  rdr  <- makeFileReader  eth
  hdr4 <- peekBytes rdr 4
  if hdr4 == fileMagicLBS
    then openBinarySlawInput1 (fcName file) rdr opts
    else openYamlSlawInput1 "openSlawInput" (fcName file) rdr opts cs

--

data YInput = YInput
  { yinName       :: String
  , yinReader     :: !FileReader
  , yinLastOffset :: !(IORef (Maybe Integer))
  }

data YInput2 = YInput2
  { yinPtr :: !InputFPtr
  , yinYin :: !YInput
  }

wrapIOE :: IO Retort -> IO Int64
wrapIOE func = do
  eth <- try func
  case eth of
    Left  ioe  -> return $ unRetort $ ioetToRetort $ ioeGetErrorType ioe
    Right tort -> return $ unRetort tort

yInputReadFunc :: YInput -> ReadFunc
yInputReadFunc yin op bytePtr sizeIn sizeOutPtr = do
  let op' = chr $ fromIntegral op
  wrapIOE $ yInputReadFunc' yin op' bytePtr sizeIn sizeOutPtr

yInputReadFunc'
  :: YInput
  -> Char
  -> Ptr Word8
  -> CSize
  -> Ptr CSize
  -> IO Retort
yInputReadFunc' yin 'r' bytePtr sizeIn sizeOutPtr = do
  let rdr = yinReader yin
  lastOff <- getOffset rdr
  writeIORef (yinLastOffset yin) (Just lastOff)
  lbs     <- readBytes rdr (fromIntegral sizeIn)
  let sizeOut = fromIntegral (L.length lbs) `min` sizeIn
  copyLazyByteStringToBuffer lbs bytePtr (fromIntegral sizeOut)
  poke sizeOutPtr sizeOut
  return OB_OK
yInputReadFunc' yin 'c' _ _ _ = do
  closeFileReader (yinReader yin)
  return OB_OK
yInputReadFunc' _ _ _ _ _ = return ZE_HS_INTERNAL_ERROR

makeInputFunc :: YInput -> IO ReadPtr
makeInputFunc yin = createReadPtr (yInputReadFunc yin)

--

data YOutput = YOutput
  { youtName        :: String
  , youtHandle      :: !Handle
  , youtShouldClose :: !Bool
  , youtOffsets     :: !(IORef YOutOffsets)
  }

data YOutOffsets = YOutOffsets
  { yooCurrentOffset :: {-# UNPACK #-} !Word64
  , yooLastOffset    :: {-# UNPACK #-} !Word64
  }

data YOutput2 = YOutput2
  { youtPtr  :: !OutputFPtr
  , youtYout :: !YOutput
  }

yOutputWriteFunc :: YOutput -> WriteFunc
yOutputWriteFunc yout op bytePtr size = do
  let op' = chr $ fromIntegral op
  wrapIOE $ yOutputWriteFunc' yout op' bytePtr size

yOutputWriteFunc'
  :: YOutput
  -> Char
  -> C.ConstPtr Word8
  -> CSize
  -> IO Retort
yOutputWriteFunc' yout 'w' bytePtr size = do
  modifyIORef' (youtOffsets yout) (advanceOffset $ fromIntegral size)
  hPutBuf (youtHandle yout) (C.unConstPtr bytePtr) (fromIntegral size)
  return OB_OK
yOutputWriteFunc' yout 'f' _ _ = do
  hFlush (youtHandle yout)
  return OB_OK
yOutputWriteFunc' yout 'c' _ _ = do
  let h = youtHandle yout
  if youtShouldClose yout
    then hClose h
    else hFlush h
  return OB_OK
yOutputWriteFunc' _ _ _ _ = return ZE_HS_INTERNAL_ERROR

advanceOffset :: Word64 -> YOutOffsets -> YOutOffsets
advanceOffset !size yoo =
  let o = yooCurrentOffset yoo
  in YOutOffsets { yooCurrentOffset = (o + size)
                 , yooLastOffset    = o
                 }

makeOutputFunc :: YOutput -> IO WritePtr
makeOutputFunc yout = createWritePtr (yOutputWriteFunc yout)

--

-- | Opens a 'SlawInputStream' for reading slawx from a YAML file.
-- If an error occurs, may throw 'IOException' or 'PlasmaException'.
--
-- Does not currently take any options, so the second argument is
-- placeholder which is just ignored.  The easiest thing to do
-- is just pass in @()@.
openYamlSlawInput :: (HasCallStack, FileClass a, ToSlaw b)
                  => a -- ^ name (or handle) of file to open
                  -> b -- ^ options map/protein (currently none)
                  -> IO SlawInputStream
openYamlSlawInput file opts = do
  let nam = fcName file
  eth <- fcOpenReadOrMap file
  rdr <- makeFileReader eth
  openYamlSlawInput1 "openYamlSlawInput" nam rdr opts callStack

openYamlSlawInput1
  :: ToSlaw b
  => String -- location name
  -> String -- file name
  -> FileReader
  -> b      -- options (ignored)
  -> CallStack
  -> IO SlawInputStream
openYamlSlawInput1 loc nam rdr _ cs = do
  initialize
  offRef <- newIORef Nothing
  let yin  = YInput { yinName       = nam
                    , yinReader     = rdr
                    , yinLastOffset = offRef
                    }
      erl  = Just $ def { elSource = DsFile nam }
      addn = Just loc
  iPtr <- withReturnedRetortCS EtSlawIO addn erl cs $ \tortPtr -> do
    readPtr <- makeInputFunc yin
    c_open_yaml_input readPtr tortPtr
  iFPtr <- newForeignPtr c_finalize_input iPtr
  uniq  <- newUnique
  let yin2 = YInput2 { yinPtr = iFPtr
                     , yinYin = yin
                     }
  return $ SlawInputStream { siName   = nam
                           , siShow   = yamPtr iPtr
                           , siUniq   = uniq
                           , siRead'  = yiRead  yin2
                           , siClose' = yiClose yin2
                           }

yiRead :: YInput2 -> CallStack -> IO (Maybe Slaw)
yiRead y2 cs = do
  let addn = Just    "siRead"
      yin  = yinYin  y2
      nam  = yinName yin
  moff <- readIORef (yinLastOffset yin)
  let erl = ErrLocation { elSource = DsFile nam
                        , elOffset = fmap fromInteger moff
                        }
  withForeignPtr (yinPtr y2) $ \iPtr -> do
    withReturnedSlaw erl $ \lenPtr -> do
      withReturnedRetortCS EtSlawIO addn (Just erl) cs $ \tortPtr -> do
        slawPtr <- c_read_input iPtr tortPtr lenPtr
        tort    <- peek tortPtr
        case Retort tort of
          SLAW_END_OF_FILE -> do
            poke tortPtr (unRetort OB_OK)
            return nullPtr
          _                -> return slawPtr

yiClose :: YInput2 -> CallStack -> IO ()
yiClose y2 cs = do
  let addn = Just    "siClose"
      yin  = yinYin  y2
      nam  = yinName yin
  moff <- readIORef (yinLastOffset yin)
  let erl = ErrLocation { elSource = DsFile nam
                        , elOffset = fmap fromInteger moff
                        }
  withForeignPtr (yinPtr y2) $ \iPtr -> do
    tort <- c_close_input iPtr
    throwRetortCS_ EtSlawIO addn (Retort tort) (Just erl) cs

--

-- | Opens a 'SlawOutputStream' for writing slawx to a YAML file.
-- If an error occurs, may throw 'IOException' or 'PlasmaException'.
--
-- The second argument is a map or protein which specifies options.
-- The easiest thing is to pass in 'WriteYamlOptions' if you want
-- to specify any non-default options, or just pass @()@ to use
-- the defaults.
openYamlSlawOutput :: (HasCallStack, FileClass a, ToSlaw b)
                   => a -- ^ name (or handle) of file to open
                   -> b -- ^ options map/protein
                   -> IO SlawOutputStream
openYamlSlawOutput file opts =
  openYamlSlawOutput1 "openYamlSlawOutput" file (š opts) callStack

openYamlSlawOutput1 :: FileClass a
                    => String -- ^ location name
                    -> a      -- ^ name (or handle) of file to open
                    -> Slaw   -- ^ options map/protein
                    -> CallStack
                    -> IO SlawOutputStream
openYamlSlawOutput1 addn file opts cs = do
  initialize
  let nam   = fcName file
      addn' = Just addn
  (h, shouldClose) <- fcOpenWrite file
  offRef           <- newIORef $ YOutOffsets 0 0
  let yout = YOutput { youtName        = nam
                     , youtHandle      = h
                     , youtShouldClose = shouldClose
                     , youtOffsets     = offRef
                     }
      erl  = Just $ def { elSource = DsFile nam }
  oPtr <- withReturnedRetortCS EtSlawIO addn' erl cs $ \tortPtr -> do
    withSlaw opts $ \slawPtr -> do
      writePtr <- makeOutputFunc yout
      c_open_yaml_output writePtr slawPtr tortPtr
  oFPtr <- newForeignPtr c_finalize_output oPtr
  uniq  <- newUnique
  let yout2 = YOutput2 { youtPtr  = oFPtr
                       , youtYout = yout
                       }
  return $ SlawOutputStream { soName   = nam
                            , soShow   = yamPtr oPtr
                            , soUniq   = uniq
                            , soWrite' = yoWrite yout2
                            , soFlush' = yoFlush yout2
                            , soClose' = yoClose yout2
                            }

yoErl :: YOutput -> IO ErrLocation
yoErl yo = do
  yoffs <- readIORef (youtOffsets yo)
  let nam     = youtName yo
  return $ mkErl (DsFile nam) yoffs

mkErl :: DataSource -> YOutOffsets -> ErrLocation
mkErl ds yoffs =
  let curOff  = yooCurrentOffset yoffs
      lastOff = yooLastOffset    yoffs
      o       = if curOff == 0
                then Nothing
                else Just lastOff
  in ErrLocation { elSource = ds
                 , elOffset = o
                 }

yoWrite :: YOutput2 -> CallStack -> Slaw -> IO ()
yoWrite y2 cs slaw = do
  let addn = Just     "soWrite"
      yout = youtYout y2
  erl <- yoErl yout
  withForeignPtr (youtPtr y2) $ \oPtr -> do
    withSlaw slaw $ \slawPtr -> do
      tort <- c_write_output oPtr slawPtr
      throwRetortCS_ EtSlawIO addn (Retort tort) (Just erl) cs

-- Currently, YAML output auto-flushes after each slaw.
-- There's no way to flush manually, so ignore.
yoFlush :: YOutput2 -> CallStack -> IO ()
yoFlush _ _ = return ()

yoClose :: YOutput2 -> CallStack -> IO ()
yoClose y2 cs = do
  let addn = Just     "soClose"
      yout = youtYout y2
  erl <- yoErl yout
  withForeignPtr (youtPtr y2) $ \oPtr -> do
    tort <- c_close_output oPtr
    throwRetortCS_ EtSlawIO addn (Retort tort) (Just erl) cs

--

formatFromName :: String -> Maybe FileFormat
formatFromName nam =
  let ext = L8.takeWhileEnd (/= '.') $ toUtf8 nam
  in stringToEnum extStrings ext

extStrings :: EnumStrings FileFormat
extStrings = makeEnumStrings
  [ ("slaw bin",     BinaryFile)
  , ("yaml txt pro", YamlFile)
  ]

-- | Opens a 'SlawOutputStream' for writing slawx to a file.
-- Can write either a binary or YAML file, depending on the options
-- given, or the file extension.
-- If an error occurs, may throw 'IOException' or 'PlasmaException'.
--
-- The second argument is a map or protein which specifies options.
-- If the map has a key named @format@, with a value of @binary@
-- or @yaml@, then that determines the format used to write the file.
--
-- If the format is not specified in the options, then the file
-- extension is checked.  An extension of @.slaw@ or @.bin@
-- indicates a binary file, and @.yaml@, @.txt@, or @.pro@
-- indicates a YAML file.  If all else fails, a default is
-- used.  (Currently YAML, but subject to change.)
--
-- The easiest thing is to pass in 'WriteBinaryOptions' for the
-- second argument if you want a binary file, or 'WriteYamlOptions'
-- if you want a YAML file.  Or just pass @()@ to use the
-- file extension.
openSlawOutput :: (HasCallStack, FileClass a, ToSlaw b)
               => a -- ^ name (or handle) of file to open
               -> b -- ^ options map/protein
               -> IO SlawOutputStream
openSlawOutput file opts = do
  let opts' = coerceToMap    (š opts)
      nam   = fcName         file
      ff1   = opts'      !:? kFormat
      ff2   = formatFromName nam
      ff    = (ff1 <|> ff2) ?> YamlFile
  case ff of
    YamlFile ->
      openYamlSlawOutput1 "openSlawOutput" file opts' callStack
    BinaryFile ->
      openBinarySlawOutput file opts'

--

-- | Reads zero or more slawx from a string containing YAML.
-- If an error occurs, returns a 'PlasmaException'.
--
-- Does not currently take any options, so the second argument is
-- placeholder which is just ignored.  The easiest thing to do
-- is just pass in @()@.
slawFromYamlString
  :: (HasCallStack, ToSlaw b)
  => LT.Text -- ^ string to read YAML from
  -> b       -- ^ options map/protein (currently none)
  -> Either PlasmaException [Slaw]
slawFromYamlString txt opts = unsafePerformIO $ do
  tryAndConvertExc $ do
    slawFromYamlStringIO' "slawFromYamlString" txt opts callStack

-- | Like 'slawFromYamlString', but runs in the IO monad,
-- and throws a 'PlasmaException' on error.
slawFromYamlStringIO
  :: (HasCallStack, ToSlaw b)
  => LT.Text -- ^ string to read YAML from
  -> b       -- ^ options map/protein (currently none)
  -> IO [Slaw]
slawFromYamlStringIO txt opts =
  slawFromYamlStringIO' "slawFromYamlStringIO" txt opts callStack

slawFromYamlStringIO'
  :: ToSlaw b
  => String  -- ^ function name
  -> LT.Text -- ^ string to read YAML from
  -> b       -- ^ options map/protein (currently none)
  -> CallStack
  -> IO [Slaw]
slawFromYamlStringIO' nam txt opts cs = do
  rdr <- makeFileReaderLazyBS $ toUtf8 txt
  sis <- openYamlSlawInput1 nam "<string>" rdr opts cs
  ss  <- readAllSlawx sis
  siClose sis
  return ss

--

data YStrOut0 = YStrOut0
  { yStr0Str :: ![B.ByteString]
  , yStr0Off :: !YOutOffsets
  }

data YStrOut1 = YStrOut1
  { yStr1Ref  :: !(IORef YStrOut0)
  , yStr1Name :: !DataSource
  , yStr1Addn :: String
  }

data YStrOut2 = YStrOut2
  { yStr2Ptr :: !OutputFPtr
  , yStr2Out :: !YStrOut1
  }

yStrWriteFunc :: YStrOut1 -> WriteFunc
yStrWriteFunc y1 op bytePtr size = do
  let op' = chr $ fromIntegral op
  wrapIOE $ yStrWriteFunc' y1 op' bytePtr size

yStrWriteFunc'
  :: YStrOut1
  -> Char
  -> C.ConstPtr Word8
  -> CSize
  -> IO Retort
yStrWriteFunc' y1 'w' bytePtr size = do
  let ref  = yStr1Ref y1
  y0 <- readIORef ref
  let off' = advanceOffset (fromIntegral size) $ yStr0Off y0
      csl  = (castPtr (C.unConstPtr bytePtr), fromIntegral size)
  buf <- B.packCStringLen csl
  let y0' = YStrOut0 { yStr0Str = buf : yStr0Str y0
                     , yStr0Off = off'
                     }
  writeIORef ref y0'
  return OB_OK
yStrWriteFunc' _ _ _ _ = return OB_OK

makeStrFunc :: YStrOut1 -> IO WritePtr
makeStrFunc y1 = createWritePtr (yStrWriteFunc y1)

-- | Writes zero or more slawx as YAML into a string, and returns
-- that string.
-- If an error occurs, returns a 'PlasmaException'.
--
-- The second argument is a map or protein which specifies options.
-- The easiest thing is to pass in 'WriteYamlOptions' if you want
-- to specify any non-default options, or just pass @()@ to use
-- the defaults.
slawToYamlString
  :: (HasCallStack, ToSlaw b)
  => [Slaw] -- ^ slawx to write to string
  -> b      -- ^ options map/protein
  -> Either PlasmaException LT.Text
slawToYamlString ss opts = withFrozenCallStack $ unsafePerformIO $ do
  tryAndConvertExc $ do
    slawToYamlStringIO' "slawToYamlString" ss opts

-- | Like 'slawToYamlString', but runs in the IO monad,
-- and throws a 'PlasmaException' on error.
slawToYamlStringIO
  :: (HasCallStack, ToSlaw b)
  => [Slaw] -- ^ slawx to write to string
  -> b      -- ^ options map/protein
  -> IO LT.Text
slawToYamlStringIO ss opts = withFrozenCallStack $ do
  slawToYamlStringIO' "slawToYamlStringIO" ss opts

slawToYamlStringIO'
  :: (HasCallStack, ToSlaw b)
  => String -- ^ function name
  -> [Slaw] -- ^ slawx to write to string
  -> b      -- ^ options map/protein
  -> IO LT.Text
slawToYamlStringIO' addn ss opts = do
  let nam   = "<string>"
      opts' = š opts `prefLeft` commentsOff
      opn   = slawOpenYamlString addn nam opts' callStack
  y1 <- bracket opn (soClose . fst) (stys1 ss)
  y0 <- readIORef $ yStr1Ref y1
  let lbs = L.fromChunks $ reverse $ yStr0Str y0
  return $ fromUtf8 lbs

commentsOff :: Slaw
commentsOff = SlawMap [(š kComment, š False)]

stys1 :: [Slaw] -> (SlawOutputStream, YStrOut1) -> IO YStrOut1
stys1 ss (sos, y1) = do
  mapM_ (soWrite sos) ss
  return y1

slawOpenYamlString
  :: HasCallStack
  => String -- ^ original function name
  -> String -- ^ (fake) filename
  -> Slaw   -- ^ options map/protein
  -> CallStack
  -> IO (SlawOutputStream, YStrOut1)
slawOpenYamlString addn nam opts cs = do
  initialize
  ref <- newIORef $ YStrOut0 { yStr0Str = []
                             , yStr0Off = YOutOffsets 0 0
                             }
  let y1    = YStrOut1 { yStr1Ref  = ref
                       , yStr1Name = ds
                       , yStr1Addn = addn
                       }
      ds    = DsOther nam
      erl   = Just $ def { elSource = ds }
      addn' = Just addn
  oPtr <- withReturnedRetortCS EtSlawIO addn' erl cs $ \tortPtr -> do
    withSlaw opts $ \slawPtr -> do
      writePtr <- makeStrFunc y1
      c_open_yaml_output writePtr slawPtr tortPtr
  oFPtr <- newForeignPtr c_finalize_output oPtr
  uniq  <- newUnique
  let y2 = YStrOut2 { yStr2Ptr = oFPtr
                    , yStr2Out = y1
                    }
  return ( SlawOutputStream { soName   = nam
                            , soShow   = yamPtr oPtr
                            , soUniq   = uniq
                            , soWrite' = ysWrite y2
                            , soFlush' = ysFlush y2
                            , soClose' = ysClose y2
                            }
         , y1
         )

ysErl :: YStrOut1 -> IO ErrLocation
ysErl y1 = do
  y0 <- readIORef (yStr1Ref y1)
  let ds    = yStr1Name y1
      yoffs = yStr0Off  y0
  return $ mkErl ds yoffs

ysWrite :: YStrOut2 -> CallStack -> Slaw -> IO ()
ysWrite y2 cs slaw = do
  let y1   = yStr2Out y2
      addn = Just $ yStr1Addn y1
  erl <- ysErl y1
  withForeignPtr (yStr2Ptr y2) $ \oPtr -> do
    withSlaw slaw $ \slawPtr -> do
      tort <- c_write_output oPtr slawPtr
      throwRetortCS_ EtSlawIO addn (Retort tort) (Just erl) cs

ysFlush :: YStrOut2 -> CallStack -> IO ()
ysFlush _ _ = return ()

ysClose :: YStrOut2 -> CallStack -> IO ()
ysClose y2 cs = do
  let y1   = yStr2Out y2
      addn = Just $ yStr1Addn y1
  erl <- ysErl y1
  withForeignPtr (yStr2Ptr y2) $ \oPtr -> do
    tort <- c_close_output oPtr
    throwRetortCS_ EtSlawIO addn (Retort tort) (Just erl) cs

tryAndConvertExc :: HasCallStack => IO a -> IO (Either PlasmaException a)
tryAndConvertExc f = do
  eth <- try $ try f
  case eth of
    Left  ioe -> return $ Left $ ioeToPe EtSlawIO (Just callStack) ioe
    Right x   -> return x

yamPtr :: Ptr a -> String
yamPtr p = "YAML <" ++ fmtPtr p ++ ">"
