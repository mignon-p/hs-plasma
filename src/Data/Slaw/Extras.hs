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
-- import qualified Data.ByteString          as B
import Data.Default.Class
import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
import Data.Int
-- import Foreign.C.String
-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Stack

import Data.Slaw
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Marshal
-- import System.Loam.Retorts

foreign import capi "ze-hs-plasma.h ze_hs_plasma_spew_overview_to_string"
    c_spew_overview_to_string
      :: C.ConstPtr ()  -- bslaw  s
      -> Ptr Int64      -- int64 *len_ptr
      -> IO (Ptr ())    -- slaw   (return value)

spewOverview :: HasCallStack => Slaw -> IO T.Text
spewOverview slaw = withFrozenCallStack $ do
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
