{-|
Module      : System.Loam.Retorts.Internal.Descriptions
Description : Descriptions from ob-retorts.h and plasma-retorts.h
Copyright   : © Mignon Pelletier, 2024-2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Retorts.Internal.Descriptions
  ( tortTuples
  ) where

import qualified Data.Text                as T

import Data.Slaw
import System.Loam.Retorts.Constants

tortTuples :: [(Retort, T.Text, T.Text, Maybe PlasmaExceptionType)]
tortTuples =
  [ ( OB_OK
    , "OB_OK"
    , "The canonical success code, which conveys no further \
      \information"
    , Nothing
    )
  , ( OB_NO_MEM
    , "OB_NO_MEM"
    , "malloc failed, or similar"
    , Nothing
    )
  , ( OB_BAD_INDEX
    , "OB_BAD_INDEX"
    , "out-of-bounds access"
    , Just EtInvalidArgument
    )
  , ( OB_ARGUMENT_WAS_NULL
    , "OB_ARGUMENT_WAS_NULL"
    , "function was not expecting a NULL argument, but it was nice \
      \enough to tell you instead of segfaulting"
    , Just EtInvalidArgument
    )
  , ( OB_NOT_FOUND
    , "OB_NOT_FOUND"
    , "not the droids you're looking for"
    , Just EtNotFound
    )
  , ( OB_INVALID_ARGUMENT
    , "OB_INVALID_ARGUMENT"
    , "argument badness other than NULL or out-of-bounds"
    , Just EtInvalidArgument
    )
  , ( OB_UNKNOWN_ERR
    , "OB_UNKNOWN_ERR"
    , "There was no way to determine what the error was, or the \
      \error is so esoteric that nobody has bothered allocating a \
      \code for it yet"
    , Nothing
    )
  , ( OB_INADEQUATE_CLASS
    , "OB_INADEQUATE_CLASS"
    , "wrong parentage"
    , Nothing
    )
  , ( OB_ALREADY_PRESENT
    , "OB_ALREADY_PRESENT"
    , "You tried to add something that was already there"
    , Nothing
    )
  , ( OB_EMPTY
    , "OB_EMPTY"
    , "There was nothing there. (e. g. popping from an empty stack)"
    , Just EtNotFound
    )
  , ( OB_INVALID_OPERATION
    , "OB_INVALID_OPERATION"
    , "You tried to do something that was not allowed"
    , Nothing
    )
  , ( OB_DISCONNECTED
    , "OB_DISCONNECTED"
    , "The link to whatever-you-were-talking-to has been severed"
    , Nothing
    )
  , ( OB_VERSION_MISMATCH
    , "OB_VERSION_MISMATCH"
    , "Illegal mixing of different versions of Plasma headers and \
      \shared libs"
    , Nothing
    )
  , ( OB_PARSE_ERROR
    , "OB_PARSE_ERROR"
    , "Unable to parse the given string"
    , Nothing
    )
  , ( OB_STOP
    , "OB_STOP"
    , "not an error, but don't continue"
    , Nothing
    )
  , ( OB_NOTHING_TO_DO
    , "OB_NOTHING_TO_DO"
    , "Things are already in the state you requested them to be in"
    , Nothing
    )
  , ( OB_YES
    , "OB_YES"
    , "Success, and the answer was \"yes\""
    , Nothing
    )
  , ( OB_NO
    , "OB_NO"
    , "Success, and the answer was \"no\""
    , Nothing
    )
  , ( OB_BOUNCE
    , "OB_BOUNCE"
    , "Situation is fine, but you've proceeded far enough"
    , Nothing
    )
  , ( SLAW_CORRUPT_PROTEIN
    , "SLAW_CORRUPT_PROTEIN"
    , "Protein is invalid/corrupt"
    , Just EtCorruptSlaw
    )
  , ( SLAW_CORRUPT_SLAW
    , "SLAW_CORRUPT_SLAW"
    , "Slaw is invalid/corrupt"
    , Just EtCorruptSlaw
    )
  , ( SLAW_FABRICATOR_BADNESS
    , "SLAW_FABRICATOR_BADNESS"
    , "Internal error (I think?)"
    , Nothing
    )
  , ( SLAW_NOT_NUMERIC
    , "SLAW_NOT_NUMERIC"
    , "Attempted to coerce a non-numeric slaw to a number"
    , Just EtTypeMismatch
    )
  , ( SLAW_RANGE_ERR
    , "SLAW_RANGE_ERR"
    , "Value is outside the range supported by the requested type"
    , Just EtRangeError
    )
  , ( SLAW_UNIDENTIFIED_SLAW
    , "SLAW_UNIDENTIFIED_SLAW"
    , "An unknown/unsupported slaw was encountered"
    , Just EtCorruptSlaw
    )
  , ( SLAW_WRONG_LENGTH
    , "SLAW_WRONG_LENGTH"
    , "Incorrect length when coercing slaw to vector"
    , Just EtTypeMismatch
    )
  , ( SLAW_NOT_FOUND
    , "SLAW_NOT_FOUND"
    , "Requested item was not found"
    , Just EtNotFound
    )
  , ( SLAW_ALIAS_NOT_SUPPORTED
    , "SLAW_ALIAS_NOT_SUPPORTED"
    , "YAML aliases are not allowed in slaw files"
    , Just EtSlawIO
    )
  , ( SLAW_BAD_TAG
    , "SLAW_BAD_TAG"
    , "Unexpected tag when parsing slaw from YAML file"
    , Just EtSlawIO
    )
  , ( SLAW_END_OF_FILE
    , "SLAW_END_OF_FILE"
    , "End of file was encountered"
    , Just EtSlawIO
    )
  , ( SLAW_PARSING_BADNESS
    , "SLAW_PARSING_BADNESS"
    , "Error parsing slaw from YAML file"
    , Just EtSlawIO
    )
  , ( SLAW_WRONG_FORMAT
    , "SLAW_WRONG_FORMAT"
    , "File does not seem to be a binary slaw file"
    , Just EtSlawIO
    )
  , ( SLAW_WRONG_VERSION
    , "SLAW_WRONG_VERSION"
    , "Binary file is not a supported version"
    , Just EtSlawIO
    )
  , ( SLAW_YAML_ERR
    , "SLAW_YAML_ERR"
    , "File was not valid YAML"
    , Just EtSlawIO
    )
  , ( SLAW_NO_YAML
    , "SLAW_NO_YAML"
    , "libPlasma was built without YAML support"
    , Just EtSlawIO
    )
  , ( POOL_NO_POOLS_DIR
    , "POOL_NO_POOLS_DIR"
    , "Couldn't find a directory to put pools in"
    , Just EtPools
    )
  , ( POOL_FILE_BADTH
    , "POOL_FILE_BADTH"
    , "Some file-related op failed"
    , Just EtPools
    )
  , ( POOL_NULL_HOSE
    , "POOL_NULL_HOSE"
    , "pool_hose passed was NULL"
    , Just EtInvalidArgument
    )
  , ( POOL_SEMAPHORES_BADTH
    , "POOL_SEMAPHORES_BADTH"
    , "Problem with semaphores"
    , Just EtPools
    )
  , ( POOL_MMAP_BADTH
    , "POOL_MMAP_BADTH"
    , "mmap didn't work"
    , Just EtPools
    )
  , ( POOL_INAPPROPRIATE_FILESYSTEM
    , "POOL_INAPPROPRIATE_FILESYSTEM"
    , "User tried to create an mmap pool on NFS"
    , Just EtPools
    )
  , ( POOL_IN_USE
    , "POOL_IN_USE"
    , "Tried to delete (or rename) a pool that was still in use"
    , Just EtPools
    )
  , ( POOL_TYPE_BADTH
    , "POOL_TYPE_BADTH"
    , "Unknown pool type"
    , Just EtPools
    )
  , ( POOL_CONFIG_BADTH
    , "POOL_CONFIG_BADTH"
    , "Pool config file problem"
    , Just EtPools
    )
  , ( POOL_WRONG_VERSION
    , "POOL_WRONG_VERSION"
    , "Unexpected pool-version in config file"
    , Just EtPools
    )
  , ( POOL_CORRUPT
    , "POOL_CORRUPT"
    , "Something about the pool itself is bad/invalid"
    , Just EtPools
    )
  , ( POOL_POOLNAME_BADTH
    , "POOL_POOLNAME_BADTH"
    , "Invalid pool name"
    , Just EtInvalidArgument
    )
  , ( POOL_IMPOSSIBLE_RENAME
    , "POOL_IMPOSSIBLE_RENAME"
    , "Trying to rename a local pool to a network pool, or similar \
      \nonsense"
    , Just EtInvalidArgument
    )
  , ( POOL_FIFO_BADTH
    , "POOL_FIFO_BADTH"
    , "Problem with fifos"
    , Just EtPools
    )
  , ( POOL_INVALID_SIZE
    , "POOL_INVALID_SIZE"
    , "The size specified for a pool was not a number or beyond \
      \bounds"
    , Just EtInvalidArgument
    )
  , ( POOL_NO_SUCH_POOL
    , "POOL_NO_SUCH_POOL"
    , "No pool with this name"
    , Just EtNotFound
    )
  , ( POOL_EXISTS
    , "POOL_EXISTS"
    , "Attempted to create existing pool"
    , Just EtPools
    )
  , ( POOL_ILLEGAL_NESTING
    , "POOL_ILLEGAL_NESTING"
    , "Attempted to create pool \"foo/bar\" when pool \"foo\" \
      \exists, or vice versa"
    , Just EtPools
    )
  , ( POOL_PROTOCOL_ERROR
    , "POOL_PROTOCOL_ERROR"
    , "Something unexpected happened in the network pool protocol"
    , Just EtPools
    )
  , ( POOL_NO_SUCH_PROTEIN
    , "POOL_NO_SUCH_PROTEIN"
    , "The requested protein was not available"
    , Just EtNotFound
    )
  , ( POOL_AWAIT_TIMEDOUT
    , "POOL_AWAIT_TIMEDOUT"
    , "Await period expired"
    , Just EtPools
    )
  , ( POOL_AWAIT_WOKEN
    , "POOL_AWAIT_WOKEN"
    , "Await cancelled by wake()"
    , Just EtPools
    )
  , ( POOL_WAKEUP_NOT_ENABLED
    , "POOL_WAKEUP_NOT_ENABLED"
    , "Attempted to wake a hose without having previously enabled \
      \wakeup"
    , Just EtPools
    )
  , ( POOL_PROTEIN_BIGGER_THAN_POOL
    , "POOL_PROTEIN_BIGGER_THAN_POOL"
    , "Protein bigger than pool"
    , Just EtPools
    )
  , ( POOL_FROZEN
    , "POOL_FROZEN"
    , "Tried to deposit to a \"frozen\" pool"
    , Just EtPools
    )
  , ( POOL_FULL
    , "POOL_FULL"
    , "Tried to deposit to full pool that does not allow wrapping"
    , Just EtPools
    )
  , ( POOL_NOT_A_PROTEIN
    , "POOL_NOT_A_PROTEIN"
    , "Tried to deposit a non-protein slaw"
    , Just EtTypeMismatch
    )
  , ( POOL_NOT_A_PROTEIN_OR_MAP
    , "POOL_NOT_A_PROTEIN_OR_MAP"
    , "The options slaw was not a protein or map"
    , Just EtTypeMismatch
    )
  , ( POOL_CONF_WRITE_BADTH
    , "POOL_CONF_WRITE_BADTH"
    , "Writing config file failed"
    , Just EtPools
    )
  , ( POOL_CONF_READ_BADTH
    , "POOL_CONF_READ_BADTH"
    , "Reading config file failed"
    , Just EtPools
    )
  , ( POOL_SEND_BADTH
    , "POOL_SEND_BADTH"
    , "Problem sending over network"
    , Just EtPools
    )
  , ( POOL_RECV_BADTH
    , "POOL_RECV_BADTH"
    , "Problem reading over network"
    , Just EtPools
    )
  , ( POOL_UNEXPECTED_CLOSE
    , "POOL_UNEXPECTED_CLOSE"
    , "Remote end closed socket unexpectedly"
    , Just EtPools
    )
  , ( POOL_SOCK_BADTH
    , "POOL_SOCK_BADTH"
    , "Problem making network socket"
    , Just EtPools
    )
  , ( POOL_SERVER_BUSY
    , "POOL_SERVER_BUSY"
    , "Network pool server busy"
    , Just EtPools
    )
  , ( POOL_SERVER_UNREACH
    , "POOL_SERVER_UNREACH"
    , "Network pool server unreachable"
    , Just EtPools
    )
  , ( POOL_ALREADY_GANG_MEMBER
    , "POOL_ALREADY_GANG_MEMBER"
    , "Pool hose already part of a gang"
    , Just EtPools
    )
  , ( POOL_NOT_A_GANG_MEMBER
    , "POOL_NOT_A_GANG_MEMBER"
    , "Pool hose is not a member of a given gang"
    , Just EtPools
    )
  , ( POOL_EMPTY_GANG
    , "POOL_EMPTY_GANG"
    , "pool_next_multi() called on an empty gang"
    , Just EtPools
    )
  , ( POOL_NULL_GANG
    , "POOL_NULL_GANG"
    , "A NULL gang was passed to any of the gang functions"
    , Just EtInvalidArgument
    )
  , ( POOL_UNSUPPORTED_OPERATION
    , "POOL_UNSUPPORTED_OPERATION"
    , "The pool type does not support what you want to do to it"
    , Just EtPools
    )
  , ( POOL_INVALIDATED_BY_FORK
    , "POOL_INVALIDATED_BY_FORK"
    , "A hose created before a fork is no longer valid in the child"
    , Just EtPools
    )
  , ( POOL_NO_TLS
    , "POOL_NO_TLS"
    , "libPlasma was built without TLS support, or server does not \
      \support it"
    , Just EtPools
    )
  , ( POOL_TLS_REQUIRED
    , "POOL_TLS_REQUIRED"
    , "client does not want to use TLS, but server requires it"
    , Just EtPools
    )
  , ( POOL_TLS_ERROR
    , "POOL_TLS_ERROR"
    , "Something went wrong with TLS... not very specific"
    , Just EtPools
    )
  , ( POOL_CREATED
    , "POOL_CREATED"
    , "A pool was successfully created"
    , Just EtPools
    )
  , ( HSPLASMA_ALREADY_CLOSED
    , "HSPLASMA_ALREADY_CLOSED"
    , "already closed"
    , Nothing
    )
  , ( HSPLASMA_INTERNAL_ERROR
    , "HSPLASMA_INTERNAL_ERROR"
    , "internal error"
    , Nothing
    )
  , ( IOERR_ALREADY_EXISTS
    , "IOERR_ALREADY_EXISTS"
    , "already exists"
    , Nothing
    )
  , ( IOERR_ALREADY_IN_USE
    , "IOERR_ALREADY_IN_USE"
    , "resource busy"
    , Nothing
    )
  , ( IOERR_DOES_NOT_EXIST
    , "IOERR_DOES_NOT_EXIST"
    , "does not exist"
    , Nothing
    )
  , ( IOERR_EOF
    , "IOERR_EOF"
    , "end of file"
    , Nothing
    )
  , ( IOERR_FULL
    , "IOERR_FULL"
    , "resource exhausted"
    , Nothing
    )
  , ( IOERR_HARDWARE_FAULT
    , "IOERR_HARDWARE_FAULT"
    , "hardware fault"
    , Nothing
    )
  , ( IOERR_ILLEGAL_OPERATION
    , "IOERR_ILLEGAL_OPERATION"
    , "illegal operation"
    , Nothing
    )
  , ( IOERR_INAPPROPRIATE_TYPE
    , "IOERR_INAPPROPRIATE_TYPE"
    , "inappropriate type"
    , Nothing
    )
  , ( IOERR_INTERRUPTED
    , "IOERR_INTERRUPTED"
    , "interrupted"
    , Nothing
    )
  , ( IOERR_INVALID_ARGUMENT
    , "IOERR_INVALID_ARGUMENT"
    , "invalid argument"
    , Nothing
    )
  , ( IOERR_PERMISSION
    , "IOERR_PERMISSION"
    , "permission denied"
    , Nothing
    )
  , ( IOERR_PROTOCOL_ERROR
    , "IOERR_PROTOCOL_ERROR"
    , "protocol error"
    , Nothing
    )
  , ( IOERR_RESOURCE_VANISHED
    , "IOERR_RESOURCE_VANISHED"
    , "resource vanished"
    , Nothing
    )
  , ( IOERR_SYSTEM_ERROR
    , "IOERR_SYSTEM_ERROR"
    , "system error"
    , Nothing
    )
  , ( IOERR_TIMEOUT
    , "IOERR_TIMEOUT"
    , "timeout"
    , Nothing
    )
  , ( IOERR_UNKNOWN
    , "IOERR_UNKNOWN"
    , "unspecified error"
    , Nothing
    )
  , ( IOERR_UNSATISFIED_CONSTRAINTS
    , "IOERR_UNSATISFIED_CONSTRAINTS"
    , "unsatisfied constraints"
    , Nothing
    )
  , ( IOERR_UNSUPPORTED_OPERATION
    , "IOERR_UNSUPPORTED_OPERATION"
    , "unsupported operation"
    , Nothing
    )
  , ( IOERR_USER
    , "IOERR_USER"
    , "user error"
    , Nothing
    )
  ]
