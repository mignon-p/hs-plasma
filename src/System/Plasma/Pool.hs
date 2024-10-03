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
    -- ** Parsing pool names
  , ParsedPoolUri(..)
  , PoolLocation(..)
  , PoolAuthority(..)
  , parsePoolUri
  , makePoolUri
    -- ** Checking validity of pool names
  , isPoolUriValid
  , isParsedPoolUriValid
  , isPoolPathValid
  , isPoolHostValid
    -- ** Constants for “scheme”
  , kLocal
  , kTcp
  , kTcpo
  , kTcps
    -- * Standard pool sizes
  , small
  , medium
  , large
  , huge
  ) where

import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolOpts
