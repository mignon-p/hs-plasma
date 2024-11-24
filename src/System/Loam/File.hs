{-|
Module      : System.Loam.File
Description : Functions from ob-file.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.File
  ( makeTempDir
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

makeTempDir :: (HasCallStack, Filename a) => a -> IO a
makeTempDir pfx = from8bitFn <$> makeTempDir0 callStack "makeTempDir" pfx

makeTempDir0 :: Filename a => CallStack -> String -> a -> IO B.ByteString
makeTempDir0 cs addn pfx = do
  initialize
  let erl  = Just $ fnAsErl pfx
  ptr <- C.useAsConstCString (to8bitFn pfx) $ \pfxPtr -> do
    withReturnedRetortCS EtOther (Just addn) erl cs $ \tortPtr -> do
      c_hs_mkdtemp pfxPtr tortPtr
  B.unsafePackMallocCString ptr

withTempDir :: (HasCallStack, Filename a) => a -> (a -> IO b) -> IO b
withTempDir pfx func = do
  bracket (makeTempDir0 callStack "withTempDir" pfx) rmRfDir func'
  where
    func'   = func                 . from8bitFn
    rmRfDir = O.removePathForcibly . from8bitFn
