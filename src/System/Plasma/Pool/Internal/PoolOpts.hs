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

small :: PoolCreateOptions
small = size2opts c_pool_size_small

medium :: PoolCreateOptions
medium = size2opts c_pool_size_medium

large :: PoolCreateOptions
large = size2opts c_pool_size_large

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
