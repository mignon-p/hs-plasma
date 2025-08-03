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
  , PoolTimestamp
  , FetchOp(..)
  -- , pokeFetchOp
  , pokeFetchOps
  , FetchResult(..)
  -- , peekFetchResult
  , peekFetchResults
  ) where

import Control.DeepSeq
import Control.Monad
import Data.Default.Class
import Data.Hashable
import Data.Int
-- import Data.Word
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
-- import Text.Printf

import Data.Slaw
-- import Data.Slaw.Internal
import Data.Slaw.Util
import System.Loam.Internal.Marshal
import System.Loam.Retorts
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

-- | Number of seconds since the UNIX epoch (January 1, 1970).
-- (May be a fractional number of seconds.)
--
-- Timestamps are stored in the pool alongside each protein, and
-- indicate the time at which the protein was written to the pool.
--
-- See 'System.Loam.Time.loamTimeToPosixTime' and
-- 'System.Loam.Time.loamTimeToUtcTime' for converting pool
-- timestamps to types used by the "Data.Time.Clock" module in
-- the Haskell @time@ library.
type PoolTimestamp = LoamTime

-- | Record used as an argument to 'System.Plasma.Pool.fetch' and
-- 'System.Plasma.Pool.fetch''.
--
-- Indicates the index to fetch, and also indicates which parts
-- of the protein to fetch.  It's possible to choose whether to
-- return the descrips and ingests, and some, all, or none of the
-- rude data.
--
-- For example, setting 'foRudeOffset' to @Just 0@, and setting
-- 'foRudeLength' to @Nothing@, would return all of the rude data,
-- if any is present.
data FetchOp = FetchOp
  { -- | Index of the protein to fetch.
    foIdx          :: {-# UNPACK #-} !Int64
    -- | Whether to include the descrips in the result.
  , foWantDescrips :: !Bool
    -- | Whether to include the ingests in the result.
  , foWantIngests  :: !Bool
    -- | Byte offset within the rude data to start fetching.
    -- 'Nothing' means no rude data.
  , foRudeOffset   :: Maybe Int64
    -- | Maximum number of bytes of rude data to fetch.
    -- 'Nothing' means “until end”.
  , foRudeLength   :: Maybe Int64
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable FetchOp where
  typeName _ = "FetchOp"

-- | default is to get the whole protein
instance Default FetchOp where
  def = FetchOp
    { foIdx          = 0
    , foWantDescrips = True
    , foWantIngests  = True
    , foRudeOffset   = Just 0
    , foRudeLength   = Nothing
    }

-- | This is the type returned by 'System.Plasma.Pool.fetch' and
-- 'System.Plasma.Pool.fetch''.
--
-- It contains the protein, index, and timestamp, much like
-- 'System.Plasma.Pool.RetProtein'.  (Except that 'frProtein' may
-- be a trimmed-down version of the protein, depending on which
-- options were selected in t'FetchOp'.)
--
-- In addition, it also contains some statistics about the full protein,
-- regardless of which parts of the protein were actually returned.
data FetchResult = FetchResult
  { -- | The index at which the protein was actually located in the
    -- pool.  This may be different than 'foIdx', if the “clamp”
    -- option was specified to 'System.Plasma.Pool.fetch' or
    -- 'System.Plasma.Pool.fetch''.
    frIdx          :: {-# UNPACK #-} !PoolIndex
    -- | The time at which the protein was originally written
    -- to the pool.
  , frTimestamp    :: {-# UNPACK #-} !PoolTimestamp
    -- | Total number of bytes in the full protein.
  , frTotalBytes   :: {-# UNPACK #-} !Int64
    -- | Number of bytes in the “descrips” of the full protein.
  , frDescripBytes :: {-# UNPACK #-} !Int64
    -- | Number of bytes in the “ingests” of the full protein.
  , frIngestBytes  :: {-# UNPACK #-} !Int64
    -- | Number of bytes of rude data in the full protein.
  , frRudeBytes    :: {-# UNPACK #-} !Int64
    -- | Number of elements in the “descrips” list.
    -- 'Nothing' if descrips is not a list.
  , frNumDescrips  :: Maybe Int64
    -- | Number of key/value pairs in the “ingests” map.
    -- 'Nothing' if ingests is not a map.
  , frNumIngests   :: Maybe Int64
    -- | The returned protein, which may be a cut-down version of the
    -- actual protein in the pool, depending on the options specified
    -- in t'FetchOp'.
  , frProtein      :: Slaw
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable FetchResult where
  typeName _ = "FetchResult"

instance Default FetchResult where
  def = FetchResult
    { frIdx          = 0
    , frTimestamp    = 0
    , frTotalBytes   = 0
    , frDescripBytes = 0
    , frIngestBytes  = 0
    , frRudeBytes    = 0
    , frNumDescrips  = Nothing
    , frNumIngests   = Nothing
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
  pokeInt64Elem ptr c_field_rude_offset        (foRudeOffset   fo ?> -1)
  pokeInt64Elem ptr c_field_rude_length        (foRudeLength   fo ?> -1)

stride :: Int
stride = fieldsPerFetchRecord * sizeOf (0 :: Int64)

pokeFetchOps :: [FetchOp] -> Ptr Int64 -> IO ()
pokeFetchOps fops ptr = do
  forM_ (zip fops [0..]) $ \(fop, n) -> do
    pokeFetchOp fop $ ptr `plusPtr` (stride * n)

peekFetchResult
  :: String
  -> CallStack
  -> PoolName
  -> Ptr Int64
  -> IO (Either PlasmaException FetchResult)
peekFetchResult loc cs pool ptr = do
  idx  <-            peekInt64Elem ptr c_field_idx
  tort <- Retort <$> peekInt64Elem ptr c_field_tort
  if isSuccess tort
    then Right <$> peekFetchResult1 pool ptr
    else Left  <$> makeExc   loc cs pool idx tort

makeExc
  :: String
  -> CallStack
  -> PoolName
  -> PoolIndex
  -> Retort
  -> IO PlasmaException
makeExc loc cs pool idx tort = do
  let erl = erlFromPoolIdx pool idx
  pe <- retortToPlasmaException EtPools (Just loc) tort (Just erl)
  return $ pe { peCallstack = Just cs }

peekFetchResult1 :: PoolName -> Ptr Int64 -> IO FetchResult
peekFetchResult1 pool ptr = do
  let fltPtr = castPtr ptr
  xxIdx          <- peekInt64Elem   ptr    c_field_idx
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
    , frTimestamp    = xxTs
    , frTotalBytes   = xxTotalBytes
    , frDescripBytes = xxDescripBytes
    , frIngestBytes  = xxIngestBytes
    , frRudeBytes    = xxRudeBytes
    , frNumDescrips  = negNothing xxNumDescrips
    , frNumIngests   = negNothing xxNumIngests
    , frProtein      = prot
  }

negNothing :: Int64 -> Maybe Int64
negNothing x
  | x < 0     = Nothing
  | otherwise = Just x

peekFetchResults
  :: String
  -> CallStack
  -> PoolName
  -> Ptr Int64
  -> Int
  -> IO [Either PlasmaException FetchResult]
peekFetchResults loc cs pool ptr count = do
  forM [0 .. (count - 1)] $ \n -> do
    peekFetchResult loc cs pool $ ptr `plusPtr` (stride * n)
