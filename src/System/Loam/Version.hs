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
  , getVersion
  , SystemInfo(..)
  , getSystemInfo
  , x86Features
  , hasSse
  , hasSse2
  , hasSse3
  , hasPclmulqdq
  , hasSsse3
  , hasFma3
  , hasCx16
  , hasSse41
  , hasSse42
  , hasMovbe
  , hasPopcnt
  , hasAes
  , hasAvx
  , hasF16c
  , hasRdrnd
  ) where

import qualified Data.ByteString.Unsafe as B
import Data.Int
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import Data.Word
import Foreign.C

import System.Loam.Internal.Enums
import System.Loam.Internal.Initialize

foreign import capi "libLoam/c/ob-vers.h ob_x86_features"
    x86Features :: IO Word64

foreign import capi "libLoam/c/ob-vers.h ob_get_version"
    c_get_version :: CInt -> IO CString

foreign import capi "libLoam/c/ob-vers.h ob_get_system_info"
    c_get_system_info :: CInt -> IO Int64

mallocStr2txt :: CString -> IO T.Text
mallocStr2txt cs = T.decodeUtf8Lenient <$> B.unsafePackMallocCString cs

getVersion :: VersionOfWhat -> IO T.Text
getVersion v = do
  initialize
  c_get_version (versionOfWhat2int v) >>= mallocStr2txt

getSystemInfo :: SystemInfo -> IO Int64
getSystemInfo si = do
  initialize
  c_get_system_info $ systemInfo2int si
