{-|
Module      : System.Loam.Version
Description : Functions from ob-vers.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Version
  ( VersionOfWhat(..)
  , SystemInfo(..)
  , x86Features
  ) where

import Data.Word

import System.Loam.Internal.Enums

foreign import capi "libLoam/c/ob-vers.h ob_x86_features"
    x86Features :: IO Word64
