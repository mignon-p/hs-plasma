{-|
Module      : System.Loam.Internal.ConstPtr
Description : Portable wrapper for Foreign.C.ConstPtr
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Loam.Internal.ConstPtr
  ( ConstPtr(..)
  , ConstCString
  , nullConstPtr
  ) where

import Foreign.C.Types (CChar(..))
import Foreign.Ptr

#if defined(VERSION_base) && MIN_VERSION_base(4,18,0)

import Foreign.C.ConstPtr (ConstPtr(..))

#else

newtype ConstPtr a = ConstPtr { unConstPtr :: Ptr a }
                   deriving newtype (Storable, Show, Eq, Ord)

#endif

type ConstCString = ConstPtr CChar

nullConstPtr :: ConstPtr a
nullConstPtr = ConstPtr nullPtr
