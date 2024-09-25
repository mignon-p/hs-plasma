{-|
Module      : System.Loam.Internal.Enums
Description : Enumerations from ob-vers.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Internal.Enums
  ( VersionOfWhat(..)
  , versionOfWhat2int
  , SystemInfo(..)
  , systemInfo2int
  , SyslogPriority(..)
  , syslogPriority2int
  , StandardDir(..)
  , standardDir2int
  , LogFlag(..)
  , logFlag2w32
  , hasSse
  , hasSse2
  , hasSse3
  , hasPclmulqdq
  , hasSsse3
  , hasFma3
  , hasCx16
  , hasSse41
  , hasSse42
  , hasMovbe
  , hasPopcnt
  , hasAes
  , hasAvx
  , hasF16c
  , hasRdrnd
  ) where

import Control.DeepSeq
import Data.Bits
import Data.Hashable
import Data.Word
import Foreign.C.Types
import GHC.Generics (Generic)

#include "libLoam/c/ob-dirs.h"
#include "libLoam/c/ob-log.h"
#include "libLoam/c/ob-vers.h"
#include "ze-hs-syslog.h"

data VersionOfWhat =
    GspeakVersion
  | CompilerVersion
  | OsVersion
  | KernelVersion
  | LibcVersion
  | CpuModel
  | YobuildVersion
  | MachineModel
  | AbiVersion
  | BuildConfiguration
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

versionOfWhat2int :: VersionOfWhat -> CInt
versionOfWhat2int GspeakVersion      = #{const OB_VERSION_OF_GSPEAK}
versionOfWhat2int CompilerVersion    = #{const OB_VERSION_OF_COMPILER}
versionOfWhat2int OsVersion          = #{const OB_VERSION_OF_OS}
versionOfWhat2int KernelVersion      = #{const OB_VERSION_OF_KERNEL}
versionOfWhat2int LibcVersion        = #{const OB_VERSION_OF_LIBC}
versionOfWhat2int CpuModel           = #{const OB_VERSION_OF_CPU}
versionOfWhat2int YobuildVersion     = #{const OB_VERSION_OF_YOBUILD}
versionOfWhat2int MachineModel       = #{const OB_VERSION_OF_MACHINE}
versionOfWhat2int AbiVersion         = #{const OB_VERSION_OF_ABI}
versionOfWhat2int BuildConfiguration = #{const OB_BUILD_CONFIGURATION}

data SystemInfo =
    SysNumCores
  | SysCpuMhz
  | SysPhysicalMegabytes
  | SysSwapMegabytes
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

systemInfo2int :: SystemInfo -> CInt
systemInfo2int SysNumCores          = #{const OB_SYSINFO_NUM_CORES}
systemInfo2int SysCpuMhz            = #{const OB_SYSINFO_CPU_MHZ}
systemInfo2int SysPhysicalMegabytes = #{const OB_SYSINFO_PHYSICAL_MEGABYTES}
systemInfo2int SysSwapMegabytes     = #{const OB_SYSINFO_VIRTUAL_MEGABYTES}

data SyslogPriority =
    LogEmerg
  | LogAlert
  | LogCrit
  | LogErr
  | LogWarning
  | LogNotice
  | LogInfo
  | LogDebug
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

syslogPriority2int :: SyslogPriority -> CInt
syslogPriority2int LogEmerg   = #{const LOG_EMERG}
syslogPriority2int LogAlert   = #{const LOG_ALERT}
syslogPriority2int LogCrit    = #{const LOG_CRIT}
syslogPriority2int LogErr     = #{const LOG_ERR}
syslogPriority2int LogWarning = #{const LOG_WARNING}
syslogPriority2int LogNotice  = #{const LOG_NOTICE}
syslogPriority2int LogInfo    = #{const LOG_INFO}
syslogPriority2int LogDebug   = #{const LOG_DEBUG}

data StandardDir =
    -- | Non-changing “resource” files like fonts, images, videos.
    SharePath
    -- | Configuration files that the user might edit (@layout.V@)
  | EtcPath
    -- | Files created automatically to hold state (pids, pools)
  | VarPath
    -- | Directory for creating temporary/scratch files.
  | TmpDir
    -- | Override the location for pools.
  | PoolsDir
    -- | Shouldn't be needed at runtime, but @yobuild@ is here.
  | YobuildDir
    -- | The @--prefix@ specified to “configure”
  | PrefixDir
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

standardDir2int :: StandardDir -> CInt
standardDir2int SharePath  = #{const ob_share_path}
standardDir2int EtcPath    = #{const ob_etc_path}
standardDir2int VarPath    = #{const ob_var_path}
standardDir2int TmpDir     = #{const ob_tmp_dir}
standardDir2int PoolsDir   = #{const ob_pools_dir}
standardDir2int YobuildDir = #{const ob_yobuild_dir}
standardDir2int PrefixDir  = #{const ob_prefix_dir}

data LogFlag =
    -- | Log to the specified file descriptor.
    DstFd
    -- | Log to syslog()
  | DstSyslog
    -- | Call the specified callback.
  | DstCallback
    -- | Report this message via valgrind (and cause the valgrind run to fail)
  | DstValgrind
    -- | Print out a stack trace along with the message.
  | FlgStackTrace
    -- | Print out date and time along with the message.
  | FlgShowTime
    -- | Print the line number and full filename.
  | FlgShowWhereFull
    -- | Print the line number and file basename.
  | FlgShowWhere
    -- | Print the location code, if nonzero.
  | FlgShowCode
    -- | Print the location code, if nonzero, or line number and file basename if code is zero.
  | FlgShowCodeOrWhere
    -- | Print the process ID.
  | FlgShowPid
    -- | Print the program name.
  | FlgShowProg
    -- | Print the thread id.
  | FlgShowTid
    -- | Print the thread id if it is not the main thread.
  | FlgShowTidNonmain
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

logFlag2w32 :: LogFlag -> Word32
logFlag2w32 DstFd              = #{const OB_DST_FD}
logFlag2w32 DstSyslog          = #{const OB_DST_SYSLOG}
logFlag2w32 DstCallback        = #{const OB_DST_CALLBACK}
logFlag2w32 DstValgrind        = #{const OB_DST_VALGRIND}
logFlag2w32 FlgStackTrace      = #{const OB_FLG_STACK_TRACE}
logFlag2w32 FlgShowTime        = #{const OB_FLG_SHOW_TIME}
logFlag2w32 FlgShowWhereFull   = #{const OB_FLG_SHOW_WHERE_FULL}
logFlag2w32 FlgShowWhere       = #{const OB_FLG_SHOW_WHERE}
logFlag2w32 FlgShowCode        = #{const OB_FLG_SHOW_CODE}
logFlag2w32 FlgShowCodeOrWhere = #{const OB_FLG_SHOW_CODE_OR_WHERE}
logFlag2w32 FlgShowPid         = #{const OB_FLG_SHOW_PID}
logFlag2w32 FlgShowProg        = #{const OB_FLG_SHOW_PROG}
logFlag2w32 FlgShowTid         = #{const OB_FLG_SHOW_TID}
logFlag2w32 FlgShowTidNonmain  = #{const OB_FLG_SHOW_TID_NONMAIN}

hasSse, hasSse2, hasSse3, hasPclmulqdq, hasSsse3 :: Word64 -> Bool
hasSse       = (`testBit` 25)
hasSse2      = (`testBit` 26)
hasSse3      = (`testBit` 32)
hasPclmulqdq = (`testBit` 33)
hasSsse3     = (`testBit` 41)

hasFma3, hasCx16, hasSse41, hasSse42, hasMovbe :: Word64 -> Bool
hasFma3      = (`testBit` 44)
hasCx16      = (`testBit` 45)
hasSse41     = (`testBit` 51)
hasSse42     = (`testBit` 52)
hasMovbe     = (`testBit` 54)

hasPopcnt, hasAes, hasAvx, hasF16c, hasRdrnd :: Word64 -> Bool
hasPopcnt    = (`testBit` 55)
hasAes       = (`testBit` 57)
hasAvx       = (`testBit` 60)
hasF16c      = (`testBit` 61)
hasRdrnd     = (`testBit` 62)
