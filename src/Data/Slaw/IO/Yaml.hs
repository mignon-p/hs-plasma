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
    -- * Reading slawx from a YAML file
  , openYamlSlawInput
  , readYamlSlawFile
    -- * Writing slawx to a YAML file
  , openYamlSlawOutput
  , writeYamlSlawFile
  ) where

import Control.Exception
-- import Control.Monad
-- import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Char
-- import Data.Default.Class
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.IO
import System.IO.Error

import Data.Slaw
import Data.Slaw.Internal
-- import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C
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
type SlawPtr      = Ptr ()
type ConstSlawPtr = C.ConstPtr ()

foreign import ccall "wrapper" createReadPtr :: ReadFunc -> IO ReadPtr

foreign import ccall "wrapper" createWritePtr :: WriteFunc -> IO WritePtr

--

foreign import capi "ze-hs-slawio.h ze_hs_open_yaml_input"
    c_open_yaml_input :: ReadPtr -> Ptr Int64 -> InputPtr

foreign import capi "ze-hs-slawio.h ze_hs_read_input"
    c_read_input :: InputPtr -> Ptr Int64 -> Ptr Int64 -> IO SlawPtr

foreign import capi "ze-hs-slawio.h ze_hs_close_input"
    c_close_input :: InputPtr -> IO Int64

foreign import capi "ze-hs-slawio.h &ze_hs_finalize_input"
    c_finalize_input :: FunPtr (InputPtr -> IO ())

--

foreign import capi "ze-hs-slawio.h ze_hs_open_yaml_output"
    c_open_yaml_output :: WritePtr -> ConstSlawPtr -> Ptr Int64 -> OutputPtr

foreign import capi "ze-hs-slawio.h ze_hs_write_output"
    c_write_output :: OutputPtr -> ConstSlawPtr -> IO Int64

foreign import capi "ze-hs-slawio.h ze_hs_close_output"
    c_close_output :: OutputPtr -> IO Int64

foreign import capi "ze-hs-slawio.h &ze_hs_finalize_output"
    c_finalize_output :: FunPtr (OutputPtr -> IO ())

--

readSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
             => a -- ^ name (or handle) of file to read
             -> b -- ^ options map/protein (currently none)
             -> IO [Slaw]
readSlawFile fname opts = withFrozenCallStack $ do
  sis <- openSlawInput fname opts
  ss  <- readAllSlawx sis
  siClose sis
  return ss

readYamlSlawFile :: (HasCallStack, FileClass a, ToSlaw b)
                 => a -- ^ name (or handle) of file to read
                 -> b -- ^ options map/protein (currently none)
                 -> IO [Slaw]
readYamlSlawFile fname opts = withFrozenCallStack $ do
  sis <- openYamlSlawInput fname opts
  ss  <- readAllSlawx sis
  siClose sis
  return ss

writeYamlSlawFile :: (FileClass a, ToSlaw b)
                  => a -- ^ name (or handle) of file to write
                  -> b -- ^ options map/protein
                  -> [Slaw] -- ^ slawx to write to file
                  -> IO ()
writeYamlSlawFile fname opts ss = do
  sos <- openYamlSlawOutput fname opts
  mapM_ (soWrite sos) ss
  soClose sos

--

fileMagicLBS :: L.ByteString
fileMagicLBS = R.toLazyByteString $ R.word32BE fileMagic

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
    else openYamlSlawInput1   (fcName file) rdr opts

--

data YInput = YInput
  { yinName   :: String
  , yinReader :: !FileReader
  }

data YOutput = YOutput
  { youtName   :: String
  , youtHandle :: !Handle
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
  lbs <- readBytes (yinReader yin) (fromIntegral sizeIn)
  let sizeOut = fromIntegral (L.length lbs) `min` sizeIn
  copyLazyByteStringToBuffer lbs bytePtr (fromIntegral sizeOut)
  poke sizeOutPtr sizeOut
  return OB_OK
yInputReadFunc' yin 'c' _ _ _ = do
  closeFileReader (yinReader yin)
  return OB_OK
yInputReadFunc' _ _ _ _ _ = return ZE_HS_INTERNAL_ERROR

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
  hPutBuf (youtHandle yout) (C.unConstPtr bytePtr) (fromIntegral size)
  return OB_OK
yOutputWriteFunc' yout 'f' _ _ = do
  hFlush (youtHandle yout)
  return OB_OK
yOutputWriteFunc' yout 'c' _ _ = do
  hClose (youtHandle yout)
  return OB_OK
yOutputWriteFunc' _ _ _ _ = return ZE_HS_INTERNAL_ERROR

--

openYamlSlawInput :: (HasCallStack, FileClass a, ToSlaw b)
                  => a -- ^ name (or handle) of file to open
                  -> b -- ^ options map/protein (currently none)
                  -> IO SlawInputStream
openYamlSlawInput file opts = withFrozenCallStack $ do
  let nam = fcName file
  eth <- fcOpenReadOrMap file
  rdr <- makeFileReader eth
  openYamlSlawInput1 nam rdr opts

openYamlSlawInput1
  :: (HasCallStack, ToSlaw b)
  => String
  -> FileReader
  -> b
  -> IO SlawInputStream
openYamlSlawInput1 = undefined

--

openYamlSlawOutput :: (FileClass a, ToSlaw b)
                   => a -- ^ name (or handle) of file to open
                   -> b -- ^ options map/protein
                   -> IO SlawOutputStream
openYamlSlawOutput = undefined
