{-|
Module      : System.Loam.Internal.FgnTypes
Description : Types for representing C types in pointers
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Internal.FgnTypes
  ( FgnLogLvl
  , FgnRand
  , FgnSlaw
  , FgnSlawIn
  , FgnSlawOut
  , FgnCtx
  , FgnHose
  , FgnRawHose
  ) where

data {-# CTYPE "libLoam/c/ob-log.h"  "ob_log_level" #-} FgnLogLvl
data {-# CTYPE "libLoam/c/ob-rand.h" "ob_rand_t"    #-} FgnRand

data {-# CTYPE "libPlasma/c/slaw.h"  "struct _slaw" #-} FgnSlaw
data {-# CTYPE "ze-hs-slawio.h"      "ze_hs_input"  #-} FgnSlawIn
data {-# CTYPE "ze-hs-slawio.h"      "ze_hs_output" #-} FgnSlawOut

data {-# CTYPE "libPlasma/c/pool.h" "struct pool_context_struct" #-} FgnCtx
data {-# CTYPE "ze-hs-hose.h"       "ze_hs_hose"                 #-} FgnHose
data {-# CTYPE "libPlasma/c/pool.h" "struct pool_hose_struct"    #-} FgnRawHose
