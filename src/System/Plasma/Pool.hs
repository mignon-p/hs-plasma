{-|
Module      : System.Plasma.Pool
Description : Functions from pool.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool
  ( -- * Pool names
    PoolName(..)
  , toPoolName
  , fromPoolName
  , (+/)
  , isPoolNameValid
    -- ** Parsing pool names
  , ParsedPoolName
  , PoolLocation
  , PoolAuthority
  , parsePoolName
  , makePoolName
    -- * Standard pool sizes
  , small
  , medium
  , large
  , huge
  ) where

import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolOpts
