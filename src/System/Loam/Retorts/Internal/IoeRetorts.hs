{-|
Module      : System.Loam.Retorts.Internal.IoeRetorts
Description : Converting Haskell's IOException to/from retorts
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}

module System.Loam.Retorts.Internal.IoeRetorts
  ( ioetToRetort
  , retortToIoet
  ) where

import System.IO.Error

import Data.Slaw
import System.Loam.Retorts.Constants

msgToRetort :: String -> Retort
msgToRetort "already exists"          = IOERR_ALREADY_EXISTS
msgToRetort "resource busy"           = IOERR_ALREADY_IN_USE
msgToRetort "does not exist"          = IOERR_DOES_NOT_EXIST
msgToRetort "end of file"             = IOERR_EOF
msgToRetort "resource exhausted"      = IOERR_FULL
msgToRetort "hardware fault"          = IOERR_HARDWARE_FAULT
msgToRetort "illegal operation"       = IOERR_ILLEGAL_OPERATION
msgToRetort "inappropriate type"      = IOERR_INAPPROPRIATE_TYPE
msgToRetort "interrupted"             = IOERR_INTERRUPTED
msgToRetort "invalid argument"        = IOERR_INVALID_ARGUMENT
msgToRetort "permission denied"       = IOERR_PERMISSION
msgToRetort "protocol error"          = IOERR_PROTOCOL_ERROR
msgToRetort "resource vanished"       = IOERR_RESOURCE_VANISHED
msgToRetort "system error"            = IOERR_SYSTEM_ERROR
msgToRetort "timeout"                 = IOERR_TIMEOUT
msgToRetort "unsatisfied constraints" = IOERR_UNSATISFIED_CONSTRAINTS
msgToRetort "unsupported operation"   = IOERR_UNSUPPORTED_OPERATION
msgToRetort "user error"              = IOERR_USER
msgToRetort _                         = IOERR_UNKNOWN

ioetToRetort :: IOErrorType -> Retort
ioetToRetort ioet
  | isAlreadyExistsErrorType    ioet = IOERR_ALREADY_EXISTS
  | isAlreadyInUseErrorType     ioet = IOERR_ALREADY_IN_USE
  | isDoesNotExistErrorType     ioet = IOERR_DOES_NOT_EXIST
  | isEOFErrorType              ioet = IOERR_EOF
  | isFullErrorType             ioet = IOERR_FULL
  | isIllegalOperationErrorType ioet = IOERR_ILLEGAL_OPERATION
  | isPermissionErrorType       ioet = IOERR_PERMISSION
#if defined(VERSION_base) && MIN_VERSION_base(4,14,0)
  | isResourceVanishedErrorType ioet = IOERR_RESOURCE_VANISHED
#endif
  | isUserErrorType             ioet = IOERR_USER
  | otherwise                        = msgToRetort $ show ioet

retortToIoet :: Retort -> Maybe IOErrorType
retortToIoet IOERR_ALREADY_EXISTS    = Just alreadyExistsErrorType
retortToIoet IOERR_ALREADY_IN_USE    = Just alreadyInUseErrorType
retortToIoet IOERR_DOES_NOT_EXIST    = Just doesNotExistErrorType
retortToIoet IOERR_EOF               = Just eofErrorType
retortToIoet IOERR_FULL              = Just fullErrorType
retortToIoet IOERR_ILLEGAL_OPERATION = Just illegalOperationErrorType
retortToIoet IOERR_PERMISSION        = Just permissionErrorType
#if defined(VERSION_base) && MIN_VERSION_base(4,14,0)
retortToIoet IOERR_RESOURCE_VANISHED = Just resourceVanishedErrorType
#endif
retortToIoet IOERR_USER              = Just userErrorType
retortToIoet _                       = Nothing
