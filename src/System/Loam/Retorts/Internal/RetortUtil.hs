{-|
Module      : System.Loam.Retorts.Internal.RetortUtil
Description : Utilities for retorts and exceptions
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Retorts.Internal.RetortUtil
  ( ioeToPe
  ) where

import Control.Applicative
import Control.Exception
-- import Data.Default.Class
import Data.List
import Data.Maybe
import GHC.Stack
import System.IO.Error

import Data.Slaw
-- import System.Loam.Retorts
-- import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.IoeRetorts

ioeToPe
  :: PlasmaExceptionType
  -> Maybe CallStack
  -> IOException
  -> PlasmaException
ioeToPe et mcs ioe =
  let ioet  = ioeGetErrorType   ioe
      loc   = ioeGetLocation    ioe
      msg   = ioeGetErrorString ioe
      h     = ioeGetHandle      ioe
      fn    = ioeGetFileName    ioe

      tort  = ioetToRetort      ioet
      ds1   = fmap DsFile fn
      ds2   = fmap (DsOther . show) h
      ds    = ds1 <|> ds2
      parts = mapMaybe strOrNothing [loc, msg]

  in PlasmaException
     { peType      = et
     , peRetort    = Just tort
     , peMessage   = intercalate ": " parts
     , peCallstack = mcs
     , peLocation  = fmap (`ErrLocation` Nothing) ds
     }

strOrNothing :: String -> Maybe String
strOrNothing "" = Nothing
strOrNothing s  = Just s
