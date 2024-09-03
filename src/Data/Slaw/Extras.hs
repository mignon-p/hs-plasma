{-|
Module      : Data.Slaw.Extras
Description : Additional Slaw functionality (just “overview” so far)
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Extras
  ( spewOverview
  ) where

import Control.Exception
-- import qualified Data.ByteString               as B
import Data.Default.Class
import Data.Either
import Data.Int
import qualified Data.Text                     as T
-- import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Builder        as R
-- import qualified Data.Text.Lazy.Builder.Int    as R
import Data.Word
-- import Foreign.C.String
-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Stack
import System.IO.Unsafe
import Text.Printf

import Data.Slaw
import Data.Slaw.Extras.Internal.SpewParser
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Marshal
-- import System.Loam.Retorts

foreign import capi "ze-hs-plasma.h ze_hs_plasma_spew_overview_to_string"
    c_spew_overview_to_string
      :: C.ConstPtr ()  -- bslaw  s
      -> Ptr Int64      -- int64 *len_ptr
      -> IO (Ptr ())    -- slaw   (return value)

spewOverview :: HasCallStack => Slaw -> LT.Text
spewOverview =
  fixAddrs . unsafePerformIO . withFrozenCallStack . spewOverview0

spewOverview0 :: HasCallStack => Slaw -> IO LT.Text
spewOverview0 slaw = do
  let slawErl = def { elSource = DsOther sErlStr }
      sErlStr = "<internal:spewOverview>"
  mslaw <- withSlaw slaw $ \slawPtr -> do
    withReturnedSlaw slawErl $ \lenPtr -> do
      c_spew_overview_to_string slawPtr lenPtr
  case fmap ŝee mslaw of
    Just (Left  exc) -> throwIO exc
    Just (Right txt) -> return txt
    Nothing -> do
      let msg = "slaw_spew_overview_to_string unexpectedly returned NULL"
      throwIO $ def { peType      = EtOther
                    , peMessage   = msg
                    , peCallstack = Just callStack
                    }

-- Output from slaw_spew_overview() looks like this:
--
--   slaw[5o.0x420003ca00]: LIST (2 elems): {
--    1: slaw[3o.0x420003ca08]: STR(13): "Hello, World!"
--    2: slaw[1o.0x420003ca20]: UNT16 = 37619
--    }
--
-- But for the Haskell binding, the addresses are meaningless,
-- because we just serialized the Slaw to memory solely for the
-- purpose of running slaw_spew_overview() on it, and the memory
-- will be freed by the time the call returns.
--
-- Therefore, we parse the spew output and convert the addresses
-- into offsets (from the beginning of the slaw), which are
-- actually useful.

fixAddrs :: LT.Text -> LT.Text
fixAddrs orig =
  let origLines = LT.lines orig
      eths      = map (parseSpewLine . LT.toStrict) origLines
      parsed    = rights eths
  in case parsed of
       []     -> orig
       (lo:_) ->
         let loAddr   = slAddr lo
             newlines = mempty : repeat (R.singleton '\n')
             builders = zipWith (refmtLine loAddr) newlines eths
         in R.toLazyText $ mconcat builders

refmtLine :: Word64 -> R.Builder -> Either T.Text SpewLine -> R.Builder
refmtLine _      nl (Left  txt) = nl <> R.fromText txt
refmtLine loAddr nl (Right sl ) =
  let offset = slAddr sl - loAddr
      hexStr = printf "%04x" offset
  in mconcat [ nl
             , slPrefix sl
             , R.fromString hexStr
             , slSuffix sl
             ]
