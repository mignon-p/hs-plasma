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
  , ConstCStringLen
  , nullConstPtr
  , castConstPtr
  , useAsConstCString
  , useAsConstCStringLen
  ) where

import Data.Bifunctor
import qualified Data.ByteString          as B
import qualified Data.ByteString.Unsafe   as B
import Foreign.C.Types (CChar(..))
import Foreign.Ptr

#if defined(VERSION_base) && MIN_VERSION_base(4,18,0)

import Foreign.C.ConstPtr (ConstPtr(..))

#else

import Foreign.Storable (Storable(..))

newtype ConstPtr a = ConstPtr { unConstPtr :: Ptr a }
                   deriving newtype (Storable, Show, Eq, Ord)

#endif

type ConstCString    = ConstPtr CChar
type ConstCStringLen = (ConstCString, Int)

nullConstPtr :: ConstPtr a
nullConstPtr = ConstPtr nullPtr

castConstPtr :: ConstPtr a -> ConstPtr b
castConstPtr = ConstPtr . castPtr . unConstPtr

useAsConstCString
  :: B.ByteString
  -> (ConstCString -> IO a)
  -> IO a
useAsConstCString bs f = B.useAsCString bs (f . ConstPtr)

useAsConstCStringLen
  :: B.ByteString
  -> (ConstCStringLen -> IO a)
  -> IO a
useAsConstCStringLen bs f =
  B.unsafeUseAsCStringLen bs (f . first ConstPtr)
