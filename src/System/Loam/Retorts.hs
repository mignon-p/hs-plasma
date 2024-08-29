{-|
Module      : System.Loam.Retorts
Description : Error codes returned by libPlasma
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Retorts
  ( -- * Retorts
    Retort(..)              -- re-export from slaw package
  , RetortInfo(..)
  , isSuccess
  , isFailure
  , getRetortInfo
    -- * Exceptions
  , PlasmaException(..)     -- re-export from slaw package
  , displayPlasmaException  -- re-export from slaw package
  , PlasmaExceptionType(..) -- re-export from slaw package
  , ErrLocation(..)         -- re-export from slaw package
  , displayErrLocation      -- re-export from slaw package
  , DataSource(..)          -- re-export from slaw package
  , displayDataSource       -- re-export from slaw package
    -- ** Throwing retorts as exceptions
  , retortToPlasmaException
  , throwRetort
  , throwRetort'
  ) where

import Data.Slaw
import System.Loam.Retorts.Internal
