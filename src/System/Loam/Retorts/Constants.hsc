{-|
Module      : System.Loam.Retorts.Constants
Description : Constants from ob-retorts.h and plasma-retorts.h
Copyright   : © Mignon Pelletier, 2024-2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Pattern synonyms for specific 'Retort' values.
-}

{-# LANGUAGE PatternSynonyms            #-}

module System.Loam.Retorts.Constants
  ( -- * General retorts
    pattern OB_OK
  , pattern OB_NO_MEM
  , pattern OB_BAD_INDEX
  , pattern OB_ARGUMENT_WAS_NULL
  , pattern OB_NOT_FOUND
  , pattern OB_INVALID_ARGUMENT
  , pattern OB_UNKNOWN_ERR
  , pattern OB_INADEQUATE_CLASS
  , pattern OB_ALREADY_PRESENT
  , pattern OB_EMPTY
  , pattern OB_INVALID_OPERATION
  , pattern OB_DISCONNECTED
  , pattern OB_VERSION_MISMATCH
  , pattern OB_PARSE_ERROR
  , pattern OB_STOP
  , pattern OB_NOTHING_TO_DO
  , pattern OB_YES
  , pattern OB_NO
  , pattern OB_BOUNCE
    -- * Slaw retorts
  , pattern SLAW_CORRUPT_PROTEIN
  , pattern SLAW_CORRUPT_SLAW
  , pattern SLAW_FABRICATOR_BADNESS
  , pattern SLAW_NOT_NUMERIC
  , pattern SLAW_RANGE_ERR
  , pattern SLAW_UNIDENTIFIED_SLAW
  , pattern SLAW_WRONG_LENGTH
  , pattern SLAW_NOT_FOUND
  , pattern SLAW_ALIAS_NOT_SUPPORTED
  , pattern SLAW_BAD_TAG
  , pattern SLAW_END_OF_FILE
  , pattern SLAW_PARSING_BADNESS
  , pattern SLAW_WRONG_FORMAT
  , pattern SLAW_WRONG_VERSION
  , pattern SLAW_YAML_ERR
  , pattern SLAW_NO_YAML
    -- * Pool retorts
  , pattern POOL_NO_POOLS_DIR
  , pattern POOL_FILE_BADTH
  , pattern POOL_NULL_HOSE
  , pattern POOL_SEMAPHORES_BADTH
  , pattern POOL_MMAP_BADTH
  , pattern POOL_INAPPROPRIATE_FILESYSTEM
  , pattern POOL_IN_USE
  , pattern POOL_TYPE_BADTH
  , pattern POOL_CONFIG_BADTH
  , pattern POOL_WRONG_VERSION
  , pattern POOL_CORRUPT
  , pattern POOL_POOLNAME_BADTH
  , pattern POOL_IMPOSSIBLE_RENAME
  , pattern POOL_FIFO_BADTH
  , pattern POOL_INVALID_SIZE
  , pattern POOL_NO_SUCH_POOL
  , pattern POOL_EXISTS
  , pattern POOL_ILLEGAL_NESTING
  , pattern POOL_PROTOCOL_ERROR
  , pattern POOL_NO_SUCH_PROTEIN
  , pattern POOL_AWAIT_TIMEDOUT
  , pattern POOL_AWAIT_WOKEN
  , pattern POOL_WAKEUP_NOT_ENABLED
  , pattern POOL_PROTEIN_BIGGER_THAN_POOL
  , pattern POOL_FROZEN
  , pattern POOL_FULL
  , pattern POOL_NOT_A_PROTEIN
  , pattern POOL_NOT_A_PROTEIN_OR_MAP
  , pattern POOL_CONF_WRITE_BADTH
  , pattern POOL_CONF_READ_BADTH
  , pattern POOL_SEND_BADTH
  , pattern POOL_RECV_BADTH
  , pattern POOL_UNEXPECTED_CLOSE
  , pattern POOL_SOCK_BADTH
  , pattern POOL_SERVER_BUSY
  , pattern POOL_SERVER_UNREACH
  , pattern POOL_ALREADY_GANG_MEMBER
  , pattern POOL_NOT_A_GANG_MEMBER
  , pattern POOL_EMPTY_GANG
  , pattern POOL_NULL_GANG
  , pattern POOL_UNSUPPORTED_OPERATION
  , pattern POOL_INVALIDATED_BY_FORK
  , pattern POOL_NO_TLS
  , pattern POOL_TLS_REQUIRED
  , pattern POOL_TLS_ERROR
  , pattern POOL_CREATED
    -- * hs-plasma retorts
    --
    -- | Retorts that are specific to @hs-plasma@.
  , pattern HSPLASMA_ALREADY_CLOSED
  , pattern HSPLASMA_INTERNAL_ERROR
    -- * IOError retorts
    --
    -- | Retorts that encapsulate a Haskell 'System.IO.Error.IOError'.
  , pattern IOERR_ALREADY_EXISTS
  , pattern IOERR_ALREADY_IN_USE
  , pattern IOERR_DOES_NOT_EXIST
  , pattern IOERR_EOF
  , pattern IOERR_FULL
  , pattern IOERR_HARDWARE_FAULT
  , pattern IOERR_ILLEGAL_OPERATION
  , pattern IOERR_INAPPROPRIATE_TYPE
  , pattern IOERR_INTERRUPTED
  , pattern IOERR_INVALID_ARGUMENT
  , pattern IOERR_PERMISSION
  , pattern IOERR_PROTOCOL_ERROR
  , pattern IOERR_RESOURCE_VANISHED
  , pattern IOERR_SYSTEM_ERROR
  , pattern IOERR_TIMEOUT
  , pattern IOERR_UNKNOWN
  , pattern IOERR_UNSATISFIED_CONSTRAINTS
  , pattern IOERR_UNSUPPORTED_OPERATION
  , pattern IOERR_USER
  ) where

import Data.Slaw (Retort(..))

#include "libLoam/c/ob-retorts.h"
#include "libPlasma/c/plasma-retorts.h"
#include "ze-hs-retorts.h"

-- | __Success code:__ The canonical success code, which
-- conveys no further information.
pattern OB_OK                :: Retort
pattern OB_OK                =  Retort (#{const OB_OK})

-- | __Failure code:__ @malloc()@ failed, or similar.
pattern OB_NO_MEM            :: Retort
pattern OB_NO_MEM            =  Retort (#{const OB_NO_MEM})

-- | __Failure code:__ Out-of-bounds access.
pattern OB_BAD_INDEX         :: Retort
pattern OB_BAD_INDEX         =  Retort (#{const OB_BAD_INDEX})

-- | __Failure code:__ Function was not expecting a @NULL@
-- argument, but it was nice enough to tell you instead of
-- segfaulting.
pattern OB_ARGUMENT_WAS_NULL :: Retort
pattern OB_ARGUMENT_WAS_NULL =  Retort (#{const OB_ARGUMENT_WAS_NULL})

-- | __Failure code:__ Not the droids you're looking for.
pattern OB_NOT_FOUND         :: Retort
pattern OB_NOT_FOUND         =  Retort (#{const OB_NOT_FOUND})

-- | __Failure code:__ Argument badness other than @NULL@ or
-- out-of-bounds.
pattern OB_INVALID_ARGUMENT  :: Retort
pattern OB_INVALID_ARGUMENT  =  Retort (#{const OB_INVALID_ARGUMENT})

-- | __Failure code:__ There was no way to determine what the
-- error was, or the error is so esoteric that nobody has
-- bothered allocating a code for it yet.
pattern OB_UNKNOWN_ERR       :: Retort
pattern OB_UNKNOWN_ERR       =  Retort (#{const OB_UNKNOWN_ERR})

-- | __Failure code:__ Wrong parentage.
pattern OB_INADEQUATE_CLASS  :: Retort
pattern OB_INADEQUATE_CLASS  =  Retort (#{const OB_INADEQUATE_CLASS})

-- | __Failure code:__ You tried to add something that was
-- already there.
pattern OB_ALREADY_PRESENT   :: Retort
pattern OB_ALREADY_PRESENT   =  Retort (#{const OB_ALREADY_PRESENT})

-- | __Failure code:__ There was nothing there. (e. g. popping
-- from an empty stack)
pattern OB_EMPTY             :: Retort
pattern OB_EMPTY             =  Retort (#{const OB_EMPTY})

-- | __Failure code:__ You tried to do something that was not
-- allowed.
pattern OB_INVALID_OPERATION :: Retort
pattern OB_INVALID_OPERATION =  Retort (#{const OB_INVALID_OPERATION})

-- | __Failure code:__ The link to whatever-you-were-talking-to
-- has been severed.
pattern OB_DISCONNECTED      :: Retort
pattern OB_DISCONNECTED      =  Retort (#{const OB_DISCONNECTED})

-- | __Failure code:__ Illegal mixing of different versions of
-- g-speak headers and shared libs.
pattern OB_VERSION_MISMATCH  :: Retort
pattern OB_VERSION_MISMATCH  =  Retort (#{const OB_VERSION_MISMATCH})

-- | __Failure code:__ Unable to parse the given string.
pattern OB_PARSE_ERROR       :: Retort
pattern OB_PARSE_ERROR       =  Retort (#{const OB_PARSE_ERROR})

-- | __Success code:__ Not an error, but don't continue.
pattern OB_STOP              :: Retort
pattern OB_STOP              =  Retort (#{const OB_STOP})

-- | __Success code:__ Things are already in the state you
-- requested them to be in.
pattern OB_NOTHING_TO_DO     :: Retort
pattern OB_NOTHING_TO_DO     =  Retort (#{const OB_NOTHING_TO_DO})

-- | __Success code:__ Success, and the answer was “yes”
pattern OB_YES               :: Retort
pattern OB_YES               =  Retort (#{const OB_YES})

-- | __Success code:__ Success, and the answer was “no”
pattern OB_NO                :: Retort
pattern OB_NO                =  Retort (#{const OB_NO})

-- | __Success code:__ Situation is fine, but you've proceeded
-- far enough.
pattern OB_BOUNCE            :: Retort
pattern OB_BOUNCE            =  Retort (#{const OB_BOUNCE})

-- | __Failure code:__ Protein is invalid/corrupt.
pattern SLAW_CORRUPT_PROTEIN :: Retort
pattern SLAW_CORRUPT_PROTEIN =  Retort (#{const SLAW_CORRUPT_PROTEIN})

-- | __Failure code:__ Slaw is invalid/corrupt.
pattern SLAW_CORRUPT_SLAW    :: Retort
pattern SLAW_CORRUPT_SLAW    =  Retort (#{const SLAW_CORRUPT_SLAW})

-- | __Failure code:__ Internal error (I think?)
pattern SLAW_FABRICATOR_BADNESS :: Retort
pattern SLAW_FABRICATOR_BADNESS =
  Retort (#{const SLAW_FABRICATOR_BADNESS})

-- | __Failure code:__ Attempted to coerce a non-numeric slaw
-- to a number.
pattern SLAW_NOT_NUMERIC     :: Retort
pattern SLAW_NOT_NUMERIC     =  Retort (#{const SLAW_NOT_NUMERIC})

-- | __Failure code:__ Value is outside the range supported by
-- the requested type.
pattern SLAW_RANGE_ERR       :: Retort
pattern SLAW_RANGE_ERR       =  Retort (#{const SLAW_RANGE_ERR})

-- | __Failure code:__ An unknown/unsupported slaw was
-- encountered.
pattern SLAW_UNIDENTIFIED_SLAW :: Retort
pattern SLAW_UNIDENTIFIED_SLAW =
  Retort (#{const SLAW_UNIDENTIFIED_SLAW})

-- | __Failure code:__ Incorrect length when coercing slaw to
-- vector.
pattern SLAW_WRONG_LENGTH    :: Retort
pattern SLAW_WRONG_LENGTH    =  Retort (#{const SLAW_WRONG_LENGTH})

-- | __Failure code:__ Requested item was not found.
pattern SLAW_NOT_FOUND       :: Retort
pattern SLAW_NOT_FOUND       =  Retort (#{const SLAW_NOT_FOUND})

-- | __Failure code:__ YAML aliases are not allowed in slaw
-- files.
pattern SLAW_ALIAS_NOT_SUPPORTED :: Retort
pattern SLAW_ALIAS_NOT_SUPPORTED =
  Retort (#{const SLAW_ALIAS_NOT_SUPPORTED})

-- | __Failure code:__ Unexpected tag when parsing slaw from
-- YAML file.
pattern SLAW_BAD_TAG         :: Retort
pattern SLAW_BAD_TAG         =  Retort (#{const SLAW_BAD_TAG})

-- | __Failure code:__ End of file was encountered.
pattern SLAW_END_OF_FILE     :: Retort
pattern SLAW_END_OF_FILE     =  Retort (#{const SLAW_END_OF_FILE})

-- | __Failure code:__ Error parsing slaw from YAML file.
pattern SLAW_PARSING_BADNESS :: Retort
pattern SLAW_PARSING_BADNESS =  Retort (#{const SLAW_PARSING_BADNESS})

-- | __Failure code:__ File does not seem to be a binary slaw
-- file.
pattern SLAW_WRONG_FORMAT    :: Retort
pattern SLAW_WRONG_FORMAT    =  Retort (#{const SLAW_WRONG_FORMAT})

-- | __Failure code:__ Binary file is not a supported version.
pattern SLAW_WRONG_VERSION   :: Retort
pattern SLAW_WRONG_VERSION   =  Retort (#{const SLAW_WRONG_VERSION})

-- | __Failure code:__ File was not valid YAML.
pattern SLAW_YAML_ERR        :: Retort
pattern SLAW_YAML_ERR        =  Retort (#{const SLAW_YAML_ERR})

-- | __Failure code:__ libPlasma was built without YAML
-- support.
pattern SLAW_NO_YAML         :: Retort
pattern SLAW_NO_YAML         =  Retort (#{const SLAW_NO_YAML})

-- | __Failure code:__ Couldn't find a directory to put pools
-- in.
pattern POOL_NO_POOLS_DIR    :: Retort
pattern POOL_NO_POOLS_DIR    =  Retort (#{const POOL_NO_POOLS_DIR})

-- | __Failure code:__ Some file-related op failed.
pattern POOL_FILE_BADTH      :: Retort
pattern POOL_FILE_BADTH      =  Retort (#{const POOL_FILE_BADTH})

-- | __Failure code:__ @pool_hose@ passed was @NULL@.
pattern POOL_NULL_HOSE       :: Retort
pattern POOL_NULL_HOSE       =  Retort (#{const POOL_NULL_HOSE})

-- | __Failure code:__ Problem with semaphores.
pattern POOL_SEMAPHORES_BADTH :: Retort
pattern POOL_SEMAPHORES_BADTH =
  Retort (#{const POOL_SEMAPHORES_BADTH})

-- | __Failure code:__ @mmap@ didn't work.
pattern POOL_MMAP_BADTH      :: Retort
pattern POOL_MMAP_BADTH      =  Retort (#{const POOL_MMAP_BADTH})

-- | __Failure code:__ User tried to create an @mmap@ pool on
-- NFS.
pattern POOL_INAPPROPRIATE_FILESYSTEM :: Retort
pattern POOL_INAPPROPRIATE_FILESYSTEM =
  Retort (#{const POOL_INAPPROPRIATE_FILESYSTEM})

-- | __Failure code:__ Tried to delete (or rename) a pool that
-- was still in use.
pattern POOL_IN_USE          :: Retort
pattern POOL_IN_USE          =  Retort (#{const POOL_IN_USE})

-- | __Failure code:__ Unknown pool type.
pattern POOL_TYPE_BADTH      :: Retort
pattern POOL_TYPE_BADTH      =  Retort (#{const POOL_TYPE_BADTH})

-- | __Failure code:__ Pool config file problem.
pattern POOL_CONFIG_BADTH    :: Retort
pattern POOL_CONFIG_BADTH    =  Retort (#{const POOL_CONFIG_BADTH})

-- | __Failure code:__ Unexpected pool-version in config file.
pattern POOL_WRONG_VERSION   :: Retort
pattern POOL_WRONG_VERSION   =  Retort (#{const POOL_WRONG_VERSION})

-- | __Failure code:__ Something about the pool itself is
-- bad/invalid.
pattern POOL_CORRUPT         :: Retort
pattern POOL_CORRUPT         =  Retort (#{const POOL_CORRUPT})

-- | __Failure code:__ Invalid pool name.
pattern POOL_POOLNAME_BADTH  :: Retort
pattern POOL_POOLNAME_BADTH  =  Retort (#{const POOL_POOLNAME_BADTH})

-- | __Failure code:__ Trying to rename a local pool to a
-- network pool, or similar nonsense.
pattern POOL_IMPOSSIBLE_RENAME :: Retort
pattern POOL_IMPOSSIBLE_RENAME =
  Retort (#{const POOL_IMPOSSIBLE_RENAME})

-- | __Failure code:__ Problem with fifos.
pattern POOL_FIFO_BADTH      :: Retort
pattern POOL_FIFO_BADTH      =  Retort (#{const POOL_FIFO_BADTH})

-- | __Failure code:__ The size specified for a pool was not a
-- number or beyond bounds.
pattern POOL_INVALID_SIZE    :: Retort
pattern POOL_INVALID_SIZE    =  Retort (#{const POOL_INVALID_SIZE})

-- | __Failure code:__ No pool with this name.
pattern POOL_NO_SUCH_POOL    :: Retort
pattern POOL_NO_SUCH_POOL    =  Retort (#{const POOL_NO_SUCH_POOL})

-- | __Failure code:__ Attempted to create existing pool.
pattern POOL_EXISTS          :: Retort
pattern POOL_EXISTS          =  Retort (#{const POOL_EXISTS})

-- | __Failure code:__ Attempted to create pool “foo/bar”
-- when pool “foo” exists, or vice versa.
pattern POOL_ILLEGAL_NESTING :: Retort
pattern POOL_ILLEGAL_NESTING =  Retort (#{const POOL_ILLEGAL_NESTING})

-- | __Failure code:__ Something unexpected happened in the
-- network pool protocol.
pattern POOL_PROTOCOL_ERROR  :: Retort
pattern POOL_PROTOCOL_ERROR  =  Retort (#{const POOL_PROTOCOL_ERROR})

-- | __Failure code:__ The requested protein was not available.
pattern POOL_NO_SUCH_PROTEIN :: Retort
pattern POOL_NO_SUCH_PROTEIN =  Retort (#{const POOL_NO_SUCH_PROTEIN})

-- | __Failure code:__ Await period expired.
pattern POOL_AWAIT_TIMEDOUT  :: Retort
pattern POOL_AWAIT_TIMEDOUT  =  Retort (#{const POOL_AWAIT_TIMEDOUT})

-- | __Failure code:__ Await cancelled by @wake()@
pattern POOL_AWAIT_WOKEN     :: Retort
pattern POOL_AWAIT_WOKEN     =  Retort (#{const POOL_AWAIT_WOKEN})

-- | __Failure code:__ Attempted to wake a hose without having
-- previously enabled wakeup.
pattern POOL_WAKEUP_NOT_ENABLED :: Retort
pattern POOL_WAKEUP_NOT_ENABLED =
  Retort (#{const POOL_WAKEUP_NOT_ENABLED})

-- | __Failure code:__ Protein bigger than pool.
pattern POOL_PROTEIN_BIGGER_THAN_POOL :: Retort
pattern POOL_PROTEIN_BIGGER_THAN_POOL =
  Retort (#{const POOL_PROTEIN_BIGGER_THAN_POOL})

-- | __Failure code:__ Tried to deposit to a “frozen” pool.
pattern POOL_FROZEN          :: Retort
pattern POOL_FROZEN          =  Retort (#{const POOL_FROZEN})

-- | __Failure code:__ Tried to deposit to full pool that does
-- not allow wrapping.
pattern POOL_FULL            :: Retort
pattern POOL_FULL            =  Retort (#{const POOL_FULL})

-- | __Failure code:__ Tried to deposit a non-protein slaw.
pattern POOL_NOT_A_PROTEIN   :: Retort
pattern POOL_NOT_A_PROTEIN   =  Retort (#{const POOL_NOT_A_PROTEIN})

-- | __Failure code:__ The options slaw was not a protein or
-- map.
pattern POOL_NOT_A_PROTEIN_OR_MAP :: Retort
pattern POOL_NOT_A_PROTEIN_OR_MAP =
  Retort (#{const POOL_NOT_A_PROTEIN_OR_MAP})

-- | __Failure code:__ Writing config file failed.
pattern POOL_CONF_WRITE_BADTH :: Retort
pattern POOL_CONF_WRITE_BADTH =
  Retort (#{const POOL_CONF_WRITE_BADTH})

-- | __Failure code:__ Reading config file failed.
pattern POOL_CONF_READ_BADTH :: Retort
pattern POOL_CONF_READ_BADTH =  Retort (#{const POOL_CONF_READ_BADTH})

-- | __Failure code:__ Problem sending over network.
pattern POOL_SEND_BADTH      :: Retort
pattern POOL_SEND_BADTH      =  Retort (#{const POOL_SEND_BADTH})

-- | __Failure code:__ Problem reading over network.
pattern POOL_RECV_BADTH      :: Retort
pattern POOL_RECV_BADTH      =  Retort (#{const POOL_RECV_BADTH})

-- | __Failure code:__ Remote end closed socket unexpectedly.
pattern POOL_UNEXPECTED_CLOSE :: Retort
pattern POOL_UNEXPECTED_CLOSE =
  Retort (#{const POOL_UNEXPECTED_CLOSE})

-- | __Failure code:__ Problem making network socket.
pattern POOL_SOCK_BADTH      :: Retort
pattern POOL_SOCK_BADTH      =  Retort (#{const POOL_SOCK_BADTH})

-- | __Failure code:__ Network pool server busy.
pattern POOL_SERVER_BUSY     :: Retort
pattern POOL_SERVER_BUSY     =  Retort (#{const POOL_SERVER_BUSY})

-- | __Failure code:__ Network pool server unreachable.
pattern POOL_SERVER_UNREACH  :: Retort
pattern POOL_SERVER_UNREACH  =  Retort (#{const POOL_SERVER_UNREACH})

-- | __Failure code:__ Pool hose already part of a gang.
pattern POOL_ALREADY_GANG_MEMBER :: Retort
pattern POOL_ALREADY_GANG_MEMBER =
  Retort (#{const POOL_ALREADY_GANG_MEMBER})

-- | __Failure code:__ Pool hose is not a member of a given
-- gang.
pattern POOL_NOT_A_GANG_MEMBER :: Retort
pattern POOL_NOT_A_GANG_MEMBER =
  Retort (#{const POOL_NOT_A_GANG_MEMBER})

-- | __Failure code:__ @pool_next_multi()@ called on an empty
-- gang.
pattern POOL_EMPTY_GANG      :: Retort
pattern POOL_EMPTY_GANG      =  Retort (#{const POOL_EMPTY_GANG})

-- | __Failure code:__ A @NULL@ gang was passed to any of the
-- gang functions.
pattern POOL_NULL_GANG       :: Retort
pattern POOL_NULL_GANG       =  Retort (#{const POOL_NULL_GANG})

-- | __Failure code:__ The pool type does not support what you
-- want to do to it.
pattern POOL_UNSUPPORTED_OPERATION :: Retort
pattern POOL_UNSUPPORTED_OPERATION =
  Retort (#{const POOL_UNSUPPORTED_OPERATION})

-- | __Failure code:__ A hose created before a @fork()@ is no
-- longer valid in the child.
pattern POOL_INVALIDATED_BY_FORK :: Retort
pattern POOL_INVALIDATED_BY_FORK =
  Retort (#{const POOL_INVALIDATED_BY_FORK})

-- | __Failure code:__ libPlasma was built without TLS support,
-- or server does not support it.
pattern POOL_NO_TLS          :: Retort
pattern POOL_NO_TLS          =  Retort (#{const POOL_NO_TLS})

-- | __Failure code:__ Client does not want to use TLS, but
-- server requires it.
pattern POOL_TLS_REQUIRED    :: Retort
pattern POOL_TLS_REQUIRED    =  Retort (#{const POOL_TLS_REQUIRED})

-- | __Failure code:__ Something went wrong with TLS... not
-- very specific.
pattern POOL_TLS_ERROR       :: Retort
pattern POOL_TLS_ERROR       =  Retort (#{const POOL_TLS_ERROR})

-- | __Success code:__ A pool was successfully created.
pattern POOL_CREATED         :: Retort
pattern POOL_CREATED         =  Retort (#{const POOL_CREATED})

-- | __Failure code:__ Already closed.
pattern HSPLASMA_ALREADY_CLOSED :: Retort
pattern HSPLASMA_ALREADY_CLOSED =
  Retort (#{const HSPLASMA_ALREADY_CLOSED})

-- | __Failure code:__ Internal error.
pattern HSPLASMA_INTERNAL_ERROR :: Retort
pattern HSPLASMA_INTERNAL_ERROR =
  Retort (#{const HSPLASMA_INTERNAL_ERROR})

-- | __Failure code:__ Already exists.
pattern IOERR_ALREADY_EXISTS :: Retort
pattern IOERR_ALREADY_EXISTS =  Retort (#{const IOERR_ALREADY_EXISTS})

-- | __Failure code:__ Resource busy.
pattern IOERR_ALREADY_IN_USE :: Retort
pattern IOERR_ALREADY_IN_USE =  Retort (#{const IOERR_ALREADY_IN_USE})

-- | __Failure code:__ Does not exist.
pattern IOERR_DOES_NOT_EXIST :: Retort
pattern IOERR_DOES_NOT_EXIST =  Retort (#{const IOERR_DOES_NOT_EXIST})

-- | __Failure code:__ End of file.
pattern IOERR_EOF            :: Retort
pattern IOERR_EOF            =  Retort (#{const IOERR_EOF})

-- | __Failure code:__ Resource exhausted.
pattern IOERR_FULL           :: Retort
pattern IOERR_FULL           =  Retort (#{const IOERR_FULL})

-- | __Failure code:__ Hardware fault.
pattern IOERR_HARDWARE_FAULT :: Retort
pattern IOERR_HARDWARE_FAULT =  Retort (#{const IOERR_HARDWARE_FAULT})

-- | __Failure code:__ Illegal operation.
pattern IOERR_ILLEGAL_OPERATION :: Retort
pattern IOERR_ILLEGAL_OPERATION =
  Retort (#{const IOERR_ILLEGAL_OPERATION})

-- | __Failure code:__ Inappropriate type.
pattern IOERR_INAPPROPRIATE_TYPE :: Retort
pattern IOERR_INAPPROPRIATE_TYPE =
  Retort (#{const IOERR_INAPPROPRIATE_TYPE})

-- | __Failure code:__ Interrupted.
pattern IOERR_INTERRUPTED    :: Retort
pattern IOERR_INTERRUPTED    =  Retort (#{const IOERR_INTERRUPTED})

-- | __Failure code:__ Invalid argument.
pattern IOERR_INVALID_ARGUMENT :: Retort
pattern IOERR_INVALID_ARGUMENT =
  Retort (#{const IOERR_INVALID_ARGUMENT})

-- | __Failure code:__ Permission denied.
pattern IOERR_PERMISSION     :: Retort
pattern IOERR_PERMISSION     =  Retort (#{const IOERR_PERMISSION})

-- | __Failure code:__ Protocol error.
pattern IOERR_PROTOCOL_ERROR :: Retort
pattern IOERR_PROTOCOL_ERROR =  Retort (#{const IOERR_PROTOCOL_ERROR})

-- | __Failure code:__ Resource vanished.
pattern IOERR_RESOURCE_VANISHED :: Retort
pattern IOERR_RESOURCE_VANISHED =
  Retort (#{const IOERR_RESOURCE_VANISHED})

-- | __Failure code:__ System error.
pattern IOERR_SYSTEM_ERROR   :: Retort
pattern IOERR_SYSTEM_ERROR   =  Retort (#{const IOERR_SYSTEM_ERROR})

-- | __Failure code:__ Timeout.
pattern IOERR_TIMEOUT        :: Retort
pattern IOERR_TIMEOUT        =  Retort (#{const IOERR_TIMEOUT})

-- | __Failure code:__ Unspecified error.
pattern IOERR_UNKNOWN        :: Retort
pattern IOERR_UNKNOWN        =  Retort (#{const IOERR_UNKNOWN})

-- | __Failure code:__ Unsatisfied constraints.
pattern IOERR_UNSATISFIED_CONSTRAINTS :: Retort
pattern IOERR_UNSATISFIED_CONSTRAINTS =
  Retort (#{const IOERR_UNSATISFIED_CONSTRAINTS})

-- | __Failure code:__ Unsupported operation.
pattern IOERR_UNSUPPORTED_OPERATION :: Retort
pattern IOERR_UNSUPPORTED_OPERATION =
  Retort (#{const IOERR_UNSUPPORTED_OPERATION})

-- | __Failure code:__ User error.
pattern IOERR_USER           :: Retort
pattern IOERR_USER           =  Retort (#{const IOERR_USER})
