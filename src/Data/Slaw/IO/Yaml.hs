{-|
Module      : Data.Slaw.IO.Yaml
Description : Read and write slawx to/from YAML files
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.IO.Yaml
  ( -- * Reading slawx from a file (YAML or binary)
    openSlawInput
  , readSlawFile
    -- * Writing slawx to a file (YAML or binary)
  , openSlawOutput
  , writeSlawFile
    -- * Reading slawx from a YAML file
  , openYamlSlawInput
  , readYamlSlawFile
    -- * Writing slawx to a YAML file
  , openYamlSlawOutput
  , writeYamlSlawFile
    -- * Options
  , WriteYamlOptions(..) -- re-export
  ) where

import Control.Applicative
import Control.Exception
-- import Control.Monad
-- import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as R
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as L8
import Data.Char
import Data.Default.Class
import Data.Int
import Data.IORef
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.IO
import System.IO.Error

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.IO
import Data.Slaw.IO.Internal.Options
import Data.Slaw.Path
import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Retorts
import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.IoeRetorts
import System.Loam.Internal.Marshal

--

type ReadFunc = CChar -> Ptr Word8 -> CSize -> Ptr CSize -> IO Int64
type ReadPtr  = FunPtr ReadFunc

type WriteFunc = CChar -> C.ConstPtr Word8 -> CSize -> IO Int64
type WritePtr  = FunPtr WriteFunc

type InputPtr     = Ptr ()
type OutputPtr    = Ptr ()

type InputFPtr    = ForeignPtr ()
type OutputFPtr   = ForeignPtr ()

type SlawPtr      = Ptr ()
type ConstSlawPtr = C.ConstPtr ()

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
  sis <- openSlawInput fname opts
  ss  <- readAllSlawx sis
  siClose sis
  return ss

-- | Convenience function to read all the slawx from a YAML
-- file.  It opens a stream, reads all the slawx from the stream,
-- and then closes the stream.  The slawx that were read are returned
-- as a list.
readYamlSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
                 => a -- ^ name (or handle) of file to read
                 -> b -- ^ options map/protein (currently none)
                 -> IO [Slaw]
readYamlSlawFile fname opts = withFrozenCallStack $ do
  sis <- openYamlSlawInput fname opts
  ss  <- readAllSlawx sis
  siClose sis
  return ss

-- | Convenience function to write a YAML slaw file all at once.
-- It opens a stream, writes all the slawx to the stream,
-- and then closes the stream.
writeYamlSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
                  => a      -- ^ name (or handle) of file to write
                  -> b      -- ^ options map/protein
                  -> [Slaw] -- ^ slawx to write to file
                  -> IO ()
writeYamlSlawFile fname opts ss = withFrozenCallStack $ do
  sos <- openYamlSlawOutput fname opts
  mapM_ (soWrite sos) ss
  soClose sos

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
  sos <- openSlawOutput fname opts
  mapM_ (soWrite sos) ss
  soClose sos

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
openSlawInput file opts = withFrozenCallStack $ do
  eth  <- fcOpenReadOrMap file
  rdr  <- makeFileReader  eth
  hdr4 <- peekBytes rdr 4
  if hdr4 == fileMagicLBS
    then openBinarySlawInput1 (fcName file) rdr opts
    else openYamlSlawInput1   "openSlawInput" (fcName file) rdr opts

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
openYamlSlawInput file opts = withFrozenCallStack $ do
  let nam = fcName file
  eth <- fcOpenReadOrMap file
  rdr <- makeFileReader eth
  openYamlSlawInput1 "openYamlSlawInput" nam rdr opts

openYamlSlawInput1
  :: (HasCallStack, ToSlaw b)
  => String -- location name
  -> String -- file name
  -> FileReader
  -> b      -- options (ignored)
  -> IO SlawInputStream
openYamlSlawInput1 addn nam rdr _ = do
  offRef <- newIORef Nothing
  let yin = YInput { yinName       = nam
                   , yinReader     = rdr
                   , yinLastOffset = offRef
                   }
      erl = Just $ def { elSource = DsFile nam }
  iPtr <- withReturnedRetort EtSlawIO (Just addn) erl $ \tortPtr -> do
    readPtr <- makeInputFunc yin
    c_open_yaml_input readPtr tortPtr
  iFPtr <- newForeignPtr c_finalize_input iPtr
  let yin2 = YInput2 { yinPtr = iFPtr
                     , yinYin = yin
                     }
  return $ SlawInputStream { siName   = nam
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
    throwRetortCS EtSlawIO addn (Retort tort) (Just erl) cs

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
  let yout2 = YOutput2 { youtPtr  = oFPtr
                       , youtYout = yout
                       }
  return $ SlawOutputStream { soName = nam
                            , soWrite' = yoWrite yout2
                            , soFlush' = yoFlush yout2
                            , soClose' = yoClose yout2
                            }

yoErl :: YOutput -> IO ErrLocation
yoErl yo = do
  yoffs <- readIORef (youtOffsets yo)
  let nam     = youtName yo
      curOff  = yooCurrentOffset yoffs
      lastOff = yooLastOffset yoffs
      o       = if curOff == 0
                then Nothing
                else Just lastOff
  return $ ErrLocation { elSource = DsFile nam
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
      throwRetortCS EtSlawIO addn (Retort tort) (Just erl) cs

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
    throwRetortCS EtSlawIO addn (Retort tort) (Just erl) cs

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
      ff1   = opts'       !? kFormat
      ff2   = formatFromName nam
      ff    = (ff1 <|> ff2) ?> YamlFile
  case ff of
    YamlFile ->
      openYamlSlawOutput1 "openSlawOutput" file opts' callStack
    BinaryFile ->
      openBinarySlawOutput file opts'
