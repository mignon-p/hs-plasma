{-|
Module      : System.Loam.Internal.Enums
Description : Enumerations from ob-vers.h, ob-dirs.h, and ob-log.h
Copyright   : © Mignon Pelletier, 2024-2025
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
  , SyslogFlag(..)
  , syslogFlag2int
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
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Generics (Generic)

import Data.Slaw

#include "libLoam/c/ob-dirs.h"
#include "libLoam/c/ob-log.h"
#include "libLoam/c/ob-vers.h"
#include "ze-hs-syslog.h"

-- | Which version string to return.
data VersionOfWhat =
    PlasmaVersion
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

instance Nameable VersionOfWhat where
  typeName _ = "VersionOfWhat"

versionOfWhat2int :: VersionOfWhat -> CInt
versionOfWhat2int PlasmaVersion      = #{const OB_VERSION_OF_GSPEAK}
versionOfWhat2int CompilerVersion    = #{const OB_VERSION_OF_COMPILER}
versionOfWhat2int OsVersion          = #{const OB_VERSION_OF_OS}
versionOfWhat2int KernelVersion      = #{const OB_VERSION_OF_KERNEL}
versionOfWhat2int LibcVersion        = #{const OB_VERSION_OF_LIBC}
versionOfWhat2int CpuModel           = #{const OB_VERSION_OF_CPU}
versionOfWhat2int YobuildVersion     = #{const OB_VERSION_OF_YOBUILD}
versionOfWhat2int MachineModel       = #{const OB_VERSION_OF_MACHINE}
versionOfWhat2int AbiVersion         = #{const OB_VERSION_OF_ABI}
versionOfWhat2int BuildConfiguration = #{const OB_BUILD_CONFIGURATION}

-- | Which integer parameter to return.
data SystemInfo =
    SysNumCores
  | SysCpuMhz
  | SysPhysicalMegabytes
  | SysSwapMegabytes
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

instance Nameable SystemInfo where
  typeName _ = "SystemInfo"

systemInfo2int :: SystemInfo -> CInt
systemInfo2int SysNumCores          = #{const OB_SYSINFO_NUM_CORES}
systemInfo2int SysCpuMhz            = #{const OB_SYSINFO_CPU_MHZ}
systemInfo2int SysPhysicalMegabytes = #{const OB_SYSINFO_PHYSICAL_MEGABYTES}
systemInfo2int SysSwapMegabytes     = #{const OB_SYSINFO_VIRTUAL_MEGABYTES}

-- | Priority passed to @syslog()@.
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

instance Default SyslogPriority where
  def = LogInfo

instance Nameable SyslogPriority where
  typeName _ = "SyslogPriority"

syslogPriority2int :: SyslogPriority -> Int32
syslogPriority2int LogEmerg   = #{const LOG_EMERG}
syslogPriority2int LogAlert   = #{const LOG_ALERT}
syslogPriority2int LogCrit    = #{const LOG_CRIT}
syslogPriority2int LogErr     = #{const LOG_ERR}
syslogPriority2int LogWarning = #{const LOG_WARNING}
syslogPriority2int LogNotice  = #{const LOG_NOTICE}
syslogPriority2int LogInfo    = #{const LOG_INFO}
syslogPriority2int LogDebug   = #{const LOG_DEBUG}

-- | Options passed to @openlog()@.
data SyslogFlag =
    LogPid
  | LogCons
  | LogOdelay
  | LogNdelay
  | LogNowait
  | LogPerror
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

instance Nameable SyslogFlag where
  typeName _ = "SyslogFlag"

syslogFlag2int :: SyslogFlag -> CInt
syslogFlag2int LogPid    = #{const LOG_PID}
syslogFlag2int LogCons   = #{const LOG_CONS}
syslogFlag2int LogOdelay = #{const LOG_ODELAY}
syslogFlag2int LogNdelay = #{const LOG_NDELAY}
syslogFlag2int LogNowait = #{const LOG_NOWAIT}
syslogFlag2int LogPerror = #{const LOG_PERROR}

-- | Standard directory to return.
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

instance Nameable StandardDir where
  typeName _ = "StandardDir"

standardDir2int :: StandardDir -> CInt
standardDir2int SharePath  = #{const ob_share_path}
standardDir2int EtcPath    = #{const ob_etc_path}
standardDir2int VarPath    = #{const ob_var_path}
standardDir2int TmpDir     = #{const ob_tmp_dir}
standardDir2int PoolsDir   = #{const ob_pools_dir}
standardDir2int YobuildDir = #{const ob_yobuild_dir}
standardDir2int PrefixDir  = #{const ob_prefix_dir}

-- | Flags to control logging.
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

instance Nameable LogFlag where
  typeName _ = "LogFlag"

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

-- | Test for Streaming SIMD Extensions.
--
-- <https://en.wikipedia.org/wiki/Streaming_SIMD_Extensions>
hasSse       :: Word64 -> Bool
hasSse       = (`testBit` 25)

-- | Test for Streaming SIMD Extensions 2.
--
-- <https://en.wikipedia.org/wiki/SSE2>
hasSse2      :: Word64 -> Bool
hasSse2      = (`testBit` 26)

-- | Test for Streaming SIMD Extensions 3.
--
-- <https://en.wikipedia.org/wiki/SSE3>
hasSse3      :: Word64 -> Bool
hasSse3      = (`testBit` 32)

-- | Test for Carry-less Multiplication.
--
-- <https://en.wikipedia.org/wiki/CLMUL_instruction_set>
hasPclmulqdq :: Word64 -> Bool
hasPclmulqdq = (`testBit` 33)

-- | Test for Supplemental Streaming SIMD Extensions 3.
--
-- <https://en.wikipedia.org/wiki/SSSE3>
hasSsse3     :: Word64 -> Bool
hasSsse3     = (`testBit` 41)

-- | Test for Fused Multiply-Add.
--
-- <https://en.wikipedia.org/wiki/FMA_instruction_set#FMA3_instruction_set>
hasFma3      :: Word64 -> Bool
hasFma3      = (`testBit` 44)

-- | Test for CMPXCHG16B instruction.
--
-- <https://www.felixcloutier.com/x86/cmpxchg8b:cmpxchg16b>
hasCx16      :: Word64 -> Bool
hasCx16      = (`testBit` 45)

-- | Test for Streaming SIMD Extensions 4.1.
--
-- <https://en.wikipedia.org/wiki/SSE4.1>
hasSse41     :: Word64 -> Bool
hasSse41     = (`testBit` 51)

-- | Test for Streaming SIMD Extensions 4.2.
--
-- <https://en.wikipedia.org/wiki/SSE4.2>
hasSse42     :: Word64 -> Bool
hasSse42     = (`testBit` 52)

-- | Test for MOVBE instruction.
--
-- <https://www.felixcloutier.com/x86/movbe>
hasMovbe     :: Word64 -> Bool
hasMovbe     = (`testBit` 54)

-- | Test for POPCNT instruction.
--
-- <https://www.felixcloutier.com/x86/popcnt>
hasPopcnt    :: Word64 -> Bool
hasPopcnt    = (`testBit` 55)

-- | Test for Advanced Encryption Standard.
--
-- <https://en.wikipedia.org/wiki/AES_instruction_set>
hasAes       :: Word64 -> Bool
hasAes       = (`testBit` 57)

-- | Test for Advanced Vector Extensions.
--
-- <https://en.wikipedia.org/wiki/Advanced_Vector_Extensions>
hasAvx       :: Word64 -> Bool
hasAvx       = (`testBit` 60)

-- | Test for F16C (previously CVT16).
--
-- <https://en.wikipedia.org/wiki/F16C>
hasF16c      :: Word64 -> Bool
hasF16c      = (`testBit` 61)

-- | Test for RDRAND instruction.
--
-- <https://en.wikipedia.org/wiki/RDRAND>
hasRdrnd     :: Word64 -> Bool
hasRdrnd     = (`testBit` 62)
