{-|
Module      : System.Loam.Version
Description : Functions from ob-vers.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Version
  ( VersionOfWhat(..)
  , SystemInfo(..)
  , x86Features
  , getVersion
  ) where

import qualified Data.ByteString.Unsafe as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import Data.Word
import Foreign.C

import System.Loam.Internal.Enums

foreign import capi "libLoam/c/ob-vers.h ob_x86_features"
    x86Features :: IO Word64

foreign import capi "libLoam/c/ob-vers.h ob_get_version"
    c_get_version :: CInt -> IO CString

mallocStr2txt :: CString -> IO T.Text
mallocStr2txt cs = T.decodeUtf8Lenient <$> B.unsafePackMallocCString cs

getVersion :: VersionOfWhat -> IO T.Text
getVersion v = c_get_version (versionOfWhat2int v) >>= mallocStr2txt
