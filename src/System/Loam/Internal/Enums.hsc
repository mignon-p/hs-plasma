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

import Data.Bits
import Data.Word
import Foreign.C.Types

#include "libLoam/c/ob-vers.h"

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
