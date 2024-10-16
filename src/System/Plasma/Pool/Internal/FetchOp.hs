{-|
Module      : System.Plasma.Pool.Internal.FetchOp
Description : FetchOp and FetchResult marshaling
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.FetchOp
  ( fieldsPerFetchRecord
  , FetchOp(..)
  , pokeFetchOp
  , FetchResult(..)
  , peekFetchResult
  ) where

import Control.DeepSeq
import Data.Default.Class
import Data.Hashable
import Data.Int
-- import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import GHC.Generics (Generic)
-- import Text.Printf

import Data.Slaw
-- import Data.Slaw.Internal
-- import Data.Slaw.Util
import System.Loam.Internal.Marshal
import System.Loam.Time
import System.Plasma.Pool.Internal.PoolName

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_IDX"
    c_field_idx :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_WANT_DESCRIPS"
    c_field_want_descrips :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_WANT_INGESTS"
    c_field_want_ingests :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_RUDE_OFFSET"
    c_field_rude_offset :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_RUDE_LENGTH"
    c_field_rude_length :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_TORT"
    c_field_tort :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_TS"
    c_field_ts :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_TOTAL_BYTES"
    c_field_total_bytes :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_DESCRIP_BYTES"
    c_field_descrip_bytes :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_INGEST_BYTES"
    c_field_ingest_bytes :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_RUDE_BYTES"
    c_field_rude_bytes :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_NUM_DESCRIPS"
    c_field_num_descrips :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_NUM_INGESTS"
    c_field_num_ingests :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_P"
    c_field_p :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_P_LEN"
    c_field_p_len :: Int

foreign import capi unsafe "ze-hs-fetch.h value ZE_HS_FETCH_MAX"
    fieldsPerFetchRecord :: Int

-- | Number of seconds since the UNIX epoch (January 1, 1970)
type PoolTimestamp = LoamTime

data FetchOp = FetchOp
  { foIdx          :: {-# UNPACK #-} !Int64
  , foWantDescrips ::                !Bool
  , foWantIngests  ::                !Bool
  , foRudeOffset   :: {-# UNPACK #-} !Int64
  , foRudeLength   :: {-# UNPACK #-} !Int64
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable FetchOp where
  typeName _ = "FetchOp"

instance Default FetchOp where
  def = FetchOp
    { foIdx          = 0
    , foWantDescrips = False
    , foWantIngests  = False
    , foRudeOffset   = 0
    , foRudeLength   = 0
    }

data FetchResult = FetchResult
  { frIdx          :: {-# UNPACK #-} !Int64
  , frTort         :: {-# UNPACK #-} !Int64
  , frTimestamp    :: {-# UNPACK #-} !PoolTimestamp
  , frTotalBytes   :: {-# UNPACK #-} !Int64
  , frDescripBytes :: {-# UNPACK #-} !Int64
  , frIngestBytes  :: {-# UNPACK #-} !Int64
  , frRudeBytes    :: {-# UNPACK #-} !Int64
  , frNumDescrips  :: {-# UNPACK #-} !Int64
  , frNumIngests   :: {-# UNPACK #-} !Int64
  , frProtein      ::                Slaw
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable FetchResult where
  typeName _ = "FetchResult"

instance Default FetchResult where
  def = FetchResult
    { frIdx          = 0
    , frTort         = 0
    , frTimestamp    = 0
    , frTotalBytes   = 0
    , frDescripBytes = 0
    , frIngestBytes  = 0
    , frRudeBytes    = 0
    , frNumDescrips  = 0
    , frNumIngests   = 0
    , frProtein      = SlawNil
    }

pokeInt64Elem :: Ptr Int64 -> Int -> Int64 -> IO ()
pokeInt64Elem = pokeElemOff

peekInt64Elem :: Ptr Int64 -> Int -> IO Int64
peekInt64Elem = peekElemOff

{-
pokeFloat64Elem :: Ptr Double -> Int -> Double -> IO ()
pokeFloat64Elem = pokeElemOff
-}

peekFloat64Elem :: Ptr Double -> Int -> IO Double
peekFloat64Elem = peekElemOff

pokeFetchOp :: FetchOp -> Ptr Int64 -> IO ()
pokeFetchOp fo ptr = do
  let fb = fromBool
  pokeInt64Elem ptr c_field_idx                (foIdx          fo)
  pokeInt64Elem ptr c_field_want_descrips (fb $ foWantDescrips fo)
  pokeInt64Elem ptr c_field_want_ingests  (fb $ foWantIngests  fo)
  pokeInt64Elem ptr c_field_rude_offset        (foRudeOffset   fo)
  pokeInt64Elem ptr c_field_rude_length        (foRudeLength   fo)

peekFetchResult :: PoolName -> Ptr Int64 -> IO FetchResult
peekFetchResult pool ptr = do
  let fltPtr = castPtr ptr
  xxIdx          <- peekInt64Elem   ptr    c_field_idx
  xxTort         <- peekInt64Elem   ptr    c_field_tort
  xxTs           <- peekFloat64Elem fltPtr c_field_ts
  xxTotalBytes   <- peekInt64Elem   ptr    c_field_total_bytes
  xxDescripBytes <- peekInt64Elem   ptr    c_field_descrip_bytes
  xxIngestBytes  <- peekInt64Elem   ptr    c_field_ingest_bytes
  xxRudeBytes    <- peekInt64Elem   ptr    c_field_rude_bytes
  xxNumDescrips  <- peekInt64Elem   ptr    c_field_num_descrips
  xxNumIngests   <- peekInt64Elem   ptr    c_field_num_ingests
  xxP            <- peekInt64Elem   ptr    c_field_p
  proteinLen     <- peekInt64Elem   ptr    c_field_p_len

  let proteinPtr = intPtrToPtr $ fromIntegral xxP
      erl        = erlFromPoolIdx pool xxIdx

  prot <- unsafePackMallocSlaw erl (proteinPtr, proteinLen)

  return $ FetchResult
    { frIdx          = xxIdx
    , frTort         = xxTort
    , frTimestamp    = xxTs
    , frTotalBytes   = xxTotalBytes
    , frDescripBytes = xxDescripBytes
    , frIngestBytes  = xxIngestBytes
    , frRudeBytes    = xxRudeBytes
    , frNumDescrips  = xxNumDescrips
    , frNumIngests   = xxNumIngests
    , frProtein      = prot
  }
