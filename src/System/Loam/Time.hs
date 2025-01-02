{-|
Module      : System.Loam.Time
Description : Functions from ob-time.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Functions for getting, formatting, and converting the time.
-}

module System.Loam.Time
  ( -- * Real time
    LoamTime
  , currentTime
  , formatTime
  , parseTime
    -- ** Converting
  , loamTimeToPosixTime
  , loamTimeToUtcTime
  , posixTimeToLoamTime
  , utcTimeToLoamTime
    -- * Monotonic time
  , MonotonicTime
  , monotonicTime
  ) where

-- import Control.Exception
import qualified Data.ByteString          as B
-- import Data.Default.Class
import Data.Int
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
-- import System.IO.Unsafe

import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Retorts

-- | Number of seconds (which may be fractional) since the
-- UNIX epoch.  (January 1, 1970 UTC)
type LoamTime = Double

-- | Number of nanoseconds since some fixed, but arbitrary,
-- point in time.
type MonotonicTime = Word64

-- | Returns the current UTC time as a 'LoamTime'.
foreign import capi unsafe "libLoam/c/ob-time.h ob_current_time"
    currentTime :: IO LoamTime

-- | Returns the number of nanoseconds since some fixed, but arbitrary,
-- point in time.  It will not be affected if the time-of-day is
-- adjusted (e. g. by NTP or by VMware Tools).  Therefore, it is a
-- good choice for measuring how long something takes, by calling it
-- before and after and taking the difference.
--
-- Although the unit is nanoseconds, note that the resolution is
-- probably not nanoseconds... could be as coarse as 15 milliseconds
-- on Windows, for example.
foreign import capi unsafe "libLoam/c/ob-time.h ob_monotonic_time"
    monotonicTime :: IO MonotonicTime

foreign import capi unsafe "libLoam/c/ob-time.h ob_format_time_f"
    c_format_time_f :: CString -> CSize -> Double -> IO ()

foreign import capi unsafe "libLoam/c/ob-time.h ob_strptime"
    c_strptime :: C.ConstCString -> Ptr Double -> IO Int64

bufSize :: Integral a => a
bufSize = 112

-- | Produces a textual representation of the given time,
-- such as @Dec 20, 2024 13:30:53.63@.
--
-- Note: Although 'LoamTime' is in UTC, the string representation
-- is in local time.  Therefore, the current time zone is used
-- implicitly.
formatTime :: LoamTime -> IO T.Text
formatTime t =
  allocaBytes bufSize $ \bufPtr -> do
    fillBytes bufPtr 0 bufSize
    c_format_time_f bufPtr bufSize t
    T.decodeUtf8Lenient <$> B.packCString bufPtr

-- | Given a string formatted with 'formatTime', such as
-- @Dec 20, 2024 13:30:53.63@, returns the corresponding
-- 'LoamTime'.
--
-- Note: Although 'LoamTime' is in UTC, the string representation
-- is in local time.  Therefore, the current time zone is used
-- implicitly.
parseTime :: HasCallStack => T.Text -> IO LoamTime
parseTime txt =
  C.useAsConstCString (T.encodeUtf8 txt) $ \ccs -> do
    let addn = Just "parseTime"
    alloca $ \dblPtr -> do
      poke dblPtr (-1)
      tort <- Retort <$> c_strptime ccs dblPtr
      throwRetortCS_ EtOther addn tort Nothing callStack
      peek dblPtr

-- | Convert 'LoamTime' to 'POSIXTime'.
loamTimeToPosixTime :: LoamTime -> POSIXTime
loamTimeToPosixTime = realToFrac

-- | Convert 'LoamTime' to 'UTCTime'.
loamTimeToUtcTime :: LoamTime -> UTCTime
loamTimeToUtcTime = posixSecondsToUTCTime . loamTimeToPosixTime

-- | Convert 'POSIXTime' to 'LoamTime'.
posixTimeToLoamTime :: POSIXTime -> LoamTime
posixTimeToLoamTime = realToFrac

-- | Convert 'UTCTime' to 'LoamTime'.
utcTimeToLoamTime :: UTCTime -> LoamTime
utcTimeToLoamTime = posixTimeToLoamTime . utcTimeToPOSIXSeconds
