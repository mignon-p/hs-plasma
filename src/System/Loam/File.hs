{-|
Module      : System.Loam.File
Description : Functions from ob-file.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Functions for dealing with the filesystem.
-}

module System.Loam.File
  ( Filename(..)
  , makeTempDir
  , withTempDir
  ) where

import Control.Exception
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B
import Data.Int
import Foreign.C.String
import Foreign.Ptr
import GHC.Stack
import qualified System.Directory.OsPath       as O

import Data.Slaw
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Filename
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal

foreign import capi safe "ze-hs-misc.h ze_hs_mkdtemp"
    c_hs_mkdtemp :: C.ConstCString -> Ptr Int64 -> IO CString

-- | Creates a new directory in @ob_tmp_dir@ whose filename begins
-- with the given prefix.  (So, overall form of the name will be
-- @ob_tmp_dir/prefixXXXXXX@, where @XXXXXX@ is an arbitrary sequence
-- of characters which makes the name unique.)
makeTempDir
  :: (HasCallStack, Filename a)
  => a -- ^ Prefix for directory name
  -> IO a
makeTempDir pfx = from8bitFn <$> makeTempDir0 callStack "makeTempDir" pfx

makeTempDir0 :: Filename a => CallStack -> String -> a -> IO B.ByteString
makeTempDir0 cs addn pfx = do
  initialize
  let erl  = Just $ fnAsErl pfx
  ptr <- C.useAsConstCString (to8bitFn pfx) $ \pfxPtr -> do
    withReturnedRetortCS EtOther (Just addn) erl cs $ \tortPtr -> do
      c_hs_mkdtemp pfxPtr tortPtr
  B.unsafePackMallocCString ptr

-- | Creates a temporary directory with 'makeTempDir', and then
-- remove it (and its contents) when the given IO action completes.
withTempDir
  :: (HasCallStack, Filename a)
  => a           -- ^ Prefix for directory name
  -> (a -> IO b) -- ^ IO action that takes the directory name
  -> IO b
withTempDir pfx func = do
  bracket (makeTempDir0 callStack "withTempDir" pfx) rmRfDir func'
  where
    func'   = func                 . from8bitFn
    rmRfDir = O.removePathForcibly . from8bitFn
