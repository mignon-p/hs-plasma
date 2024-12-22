{-|
Module      : System.Loam.Version
Description : Functions from ob-vers.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Functions for getting information about the current system.  You can
get things like the g-speak version number and the yobuild version
number (neither of which mean anything in the current state that
Zeugma is in).  You can also get things like the OS version, compiler
version, and CPU model.
-}

module System.Loam.Version
  ( -- * String parameters
    VersionOfWhat(..)
  , getVersion
    -- * Integer parameters
  , SystemInfo(..)
  , getSystemInfo
    -- * CPUID
  , x86Features
    -- ** CPUID feature tests
    --
    -- | Each of these functions tests for the presence of a
    -- particular feature, when given the output of 'x86Features'.
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

-- | On x86 processors, returns the values of @ECX@ (in upper 32 bits)
-- and @EDX@ (in lower 32 bits) when @CPUID@ is called with @EAX=1@.
-- On non-x86 processors, returns 0.  See
-- <http://www.sandpile.org/x86/cpuid.htm>
foreign import capi "libLoam/c/ob-vers.h ob_x86_features"
    x86Features :: IO Word64

foreign import capi "libLoam/c/ob-vers.h ob_get_version"
    c_get_version :: CInt -> IO CString

foreign import capi "libLoam/c/ob-vers.h ob_get_system_info"
    c_get_system_info :: CInt -> IO Int64

mallocStr2txt :: CString -> IO T.Text
mallocStr2txt cs = T.decodeUtf8Lenient <$> B.unsafePackMallocCString cs

-- | Returns a system configuration string, as requested by
-- 'VersionOfWhat'.
getVersion :: VersionOfWhat -> IO T.Text
getVersion v = do
  initialize
  c_get_version (versionOfWhat2int v) >>= mallocStr2txt

-- | Returns an integer system parameter, as requested by
-- 'SystemInfo'.
getSystemInfo :: SystemInfo -> IO Int64
getSystemInfo si = do
  initialize
  c_get_system_info $ systemInfo2int si
