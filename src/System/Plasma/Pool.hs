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
    -- * Pool contexts
  , ContextOptions(..)
    -- * Pool creation options
  , PoolCreateOptions(..)
  , StrOrInt(..)
  , kMmap
    -- ** Standard pool sizes
  , small
  , medium
  , large
  , huge
  ) where

import Data.Slaw.IO.Internal.Options
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolOpts
