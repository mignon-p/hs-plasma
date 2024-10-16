{-|
Module      : System.Loam.Time
Description : Functions from ob-time.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Time
  ( LoamTime
  , MonotonicTime
  , currentTime
  , monotonicTime
  , formatTime
  , parseTime
  ) where

-- import Control.Exception
import qualified Data.ByteString          as B
-- import Data.Default.Class
import Data.Int
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.IO.Unsafe

import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Retorts

type LoamTime = Double

type MonotonicTime = Word64

foreign import capi unsafe "libLoam/c/ob-time.h ob_current_time"
    currentTime :: IO LoamTime

foreign import capi unsafe "libLoam/c/ob-time.h ob_monotonic_time"
    monotonicTime :: IO MonotonicTime

foreign import capi unsafe "libLoam/c/ob-time.h ob_format_time_f"
    c_format_time_f :: CString -> CSize -> Double -> IO ()

foreign import capi unsafe "libLoam/c/ob-time.h ob_strptime"
    c_strptime :: C.ConstCString -> Ptr Double -> IO Int64

bufSize :: Integral a => a
bufSize = 112

formatTime :: LoamTime -> T.Text
formatTime t = unsafePerformIO $ do
  allocaBytes bufSize $ \bufPtr -> do
    fillBytes bufPtr 0 bufSize
    c_format_time_f bufPtr bufSize t
    T.decodeUtf8Lenient <$> B.packCString bufPtr

parseTime :: HasCallStack => T.Text -> Either PlasmaException LoamTime
parseTime txt = unsafePerformIO $ do
  C.useAsConstCString (T.encodeUtf8 txt) $ \ccs -> do
    alloca $ \dblPtr -> do
      poke dblPtr (-1)
      tort <- Retort <$> c_strptime ccs dblPtr
      case isSuccess tort of
        True  -> Right <$> peek dblPtr
        False -> do
          let addn = Just "parseTime"
          exc <- retortToPlasmaException EtOther addn tort Nothing
          return $ Left $ exc { peCallstack = Just callStack }
