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
msgToRetort "already exists"          = ZE_HS_IOE_ALREADY_EXISTS
msgToRetort "resource busy"           = ZE_HS_IOE_ALREADY_IN_USE
msgToRetort "does not exist"          = ZE_HS_IOE_DOES_NOT_EXIST
msgToRetort "end of file"             = ZE_HS_IOE_EOF
msgToRetort "resource exhausted"      = ZE_HS_IOE_FULL
msgToRetort "hardware fault"          = ZE_HS_IOE_HARDWARE_FAULT
msgToRetort "illegal operation"       = ZE_HS_IOE_ILLEGAL_OPERATION
msgToRetort "inappropriate type"      = ZE_HS_IOE_INAPPROPRIATE_TYPE
msgToRetort "interrupted"             = ZE_HS_IOE_INTERRUPTED
msgToRetort "invalid argument"        = ZE_HS_IOE_INVALID_ARGUMENT
msgToRetort "permission denied"       = ZE_HS_IOE_PERMISSION
msgToRetort "protocol error"          = ZE_HS_IOE_PROTOCOL_ERROR
msgToRetort "resource vanished"       = ZE_HS_IOE_RESOURCE_VANISHED
msgToRetort "system error"            = ZE_HS_IOE_SYSTEM_ERROR
msgToRetort "timeout"                 = ZE_HS_IOE_TIMEOUT
msgToRetort "unsatisfied constraints" = ZE_HS_IOE_UNSATISFIED_CONSTRAINTS
msgToRetort "unsupported operation"   = ZE_HS_IOE_UNSUPPORTED_OPERATION
msgToRetort "user error"              = ZE_HS_IOE_USER
msgToRetort _                         = ZE_HS_IOE_UNKNOWN

ioetToRetort :: IOErrorType -> Retort
ioetToRetort ioet
  | isAlreadyExistsErrorType    ioet = ZE_HS_IOE_ALREADY_EXISTS
  | isAlreadyInUseErrorType     ioet = ZE_HS_IOE_ALREADY_IN_USE
  | isDoesNotExistErrorType     ioet = ZE_HS_IOE_DOES_NOT_EXIST
  | isEOFErrorType              ioet = ZE_HS_IOE_EOF
  | isFullErrorType             ioet = ZE_HS_IOE_FULL
  | isIllegalOperationErrorType ioet = ZE_HS_IOE_ILLEGAL_OPERATION
  | isPermissionErrorType       ioet = ZE_HS_IOE_PERMISSION
#if defined(VERSION_base) && MIN_VERSION_base(4,14,0)
  | isResourceVanishedErrorType ioet = ZE_HS_IOE_RESOURCE_VANISHED
#endif
  | isUserErrorType             ioet = ZE_HS_IOE_USER
  | otherwise                        = msgToRetort $ show ioet

retortToIoet :: Retort -> Maybe IOErrorType
retortToIoet ZE_HS_IOE_ALREADY_EXISTS    = Just alreadyExistsErrorType
retortToIoet ZE_HS_IOE_ALREADY_IN_USE    = Just alreadyInUseErrorType
retortToIoet ZE_HS_IOE_DOES_NOT_EXIST    = Just doesNotExistErrorType
retortToIoet ZE_HS_IOE_EOF               = Just eofErrorType
retortToIoet ZE_HS_IOE_FULL              = Just fullErrorType
retortToIoet ZE_HS_IOE_ILLEGAL_OPERATION = Just illegalOperationErrorType
retortToIoet ZE_HS_IOE_PERMISSION        = Just permissionErrorType
#if defined(VERSION_base) && MIN_VERSION_base(4,14,0)
retortToIoet ZE_HS_IOE_RESOURCE_VANISHED = Just resourceVanishedErrorType
#endif
retortToIoet ZE_HS_IOE_USER              = Just userErrorType
retortToIoet _                           = Nothing
