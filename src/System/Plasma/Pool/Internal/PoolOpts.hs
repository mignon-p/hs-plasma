{-|
Module      : System.Plasma.Pool.Internal.PoolOpts
Description : Stuff related to pool options
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.PoolOpts
  ( small
  , medium
  , large
  , huge
    --
  , typeAndOptions
  ) where

import Data.Default.Class
import qualified Data.Text                as T
import Data.Word

import Data.Slaw
-- import Data.Slaw.Internal
import Data.Slaw.IO.Internal.Options
import Data.Slaw.Path
import Data.Slaw.Util

foreign import capi unsafe "libPlasma/c/pool_options.h value POOL_SIZE_SMALL"
    c_pool_size_small :: Word64

foreign import capi unsafe "libPlasma/c/pool_options.h value POOL_SIZE_MEDIUM"
    c_pool_size_medium :: Word64

foreign import capi unsafe "libPlasma/c/pool_options.h value POOL_SIZE_LARGE"
    c_pool_size_large :: Word64

foreign import capi unsafe "libPlasma/c/pool_options.h value POOL_SIZE_HUGE"
    c_pool_size_huge :: Word64

size2opts :: Word64 -> PoolCreateOptions
size2opts sz = def { pcoType = Just kMmap
                   , pcoSize = Just (fromIntegral sz)
                   }

-- | 1 Megabyte @mmap@ pool.
small :: PoolCreateOptions
small = size2opts c_pool_size_small

-- | 10 Megabyte @mmap@ pool.
medium :: PoolCreateOptions
medium = size2opts c_pool_size_medium

-- | 100 Megabyte @mmap@ pool.
large :: PoolCreateOptions
large = size2opts c_pool_size_large

-- | 2 Gigabyte @mmap@ pool.
huge :: PoolCreateOptions
huge = size2opts c_pool_size_huge

typeAndOptions :: ToSlaw a => a -> (T.Text, Slaw)
typeAndOptions opts = (typ, s)
  where
    s         = š opts `prefLeft` dfltOpts
    typ       = (s !? kType) ?> kMmap

dfltOpts :: Slaw
dfltOpts = SlawMap [ (š kType, š kMmap)
                   , (š kSize, š c_pool_size_small)
                   ]
