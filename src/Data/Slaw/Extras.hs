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
import qualified Data.ByteString.Builder       as R
import Data.Default.Class
import Data.Either
import Data.Int
import qualified Data.Text                     as T
-- import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Builder        as RT
-- import qualified Data.Text.Lazy.Builder.Int    as RT
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
import Data.Slaw.Internal
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

spewOverview0 :: HasCallStack => Slaw -> IO (LT.Text, Word64)
spewOverview0 slaw = do
  let slawErl            = def { elSource = DsOther sErlStr }
      sErlStr            = "<internal:spewOverview>"
      (slawBld, byteLen) = encodeSlawToBuilderAndLen nativeByteOrder slaw
      binSlaw            = R.toLazyByteString slawBld
  mslaw <- withBinarySlaw binSlaw $ \slawPtr -> do
    withReturnedSlaw slawErl $ \lenPtr -> do
      c_spew_overview_to_string slawPtr lenPtr
  case fmap ŝee mslaw of
    Just (Left  exc) -> throwIO exc
    Just (Right txt) -> return (txt, byteLen)
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

fixAddrs :: (LT.Text, Word64) -> LT.Text
fixAddrs (orig, nBytes) =
  let origLines = LT.lines orig
      eths      = map (parseSpewLine . LT.toStrict) origLines
      parsed    = rights eths
      maxStr    = printf "%x" nBytes
      nDigs     = length (maxStr :: String)
  in case parsed of
       []     -> orig
       (lo:_) ->
         let loAddr   = slAddr lo
             newlines = mempty : repeat (RT.singleton '\n')
             builders = zipWith (refmtLine loAddr nDigs) newlines eths
         in RT.toLazyText $ mconcat builders

refmtLine
  :: Word64     -- lowest address
  -> Int        -- number of hex digits to use for offset
  -> RT.Builder -- optional newline to go before this line
  -> Either T.Text SpewLine
  -> RT.Builder
refmtLine _      _     nl (Left  txt) = nl <> RT.fromText txt
refmtLine loAddr nDigs nl (Right sl ) =
  let offset = slAddr sl - loAddr
      hexStr = printf "%0*x" nDigs offset
  in mconcat [ nl
             , slPrefix sl
             , RT.fromString hexStr
             , slSuffix sl
             ]
