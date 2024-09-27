{-|
Module      : System.Loam.Internal.Misc
Description : Miscellaneous utility functions
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Internal.Misc
  ( natToDashedHex
  , ptrToIntegral
  , fPtrToIntegral
  , fmtPtr
  , fmtForeignPtr
  ) where

import Data.Bits
import Data.List
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Numeric.Natural
import Text.Printf

natToDashedHex :: Natural -> String
natToDashedHex =
  intercalate "-" . map (printf "%04x") . reverse . nat2w16s

nat2w16s :: Natural -> [Word16]
nat2w16s n = fromIntegral n : nxt
  where n'  = n `shiftR` 16
        nxt = if n' > 0
              then nat2w16s n'
              else []

{-# INLINE ptrToIntegral #-}
ptrToIntegral :: Integral a => Ptr b -> a
ptrToIntegral = fromIntegral . ptrToWordPtr

{-# INLINE fPtrToIntegral #-}
fPtrToIntegral :: Integral a => ForeignPtr b -> a
fPtrToIntegral = fromIntegral . ptrToWordPtr . unsafeForeignPtrToPtr

fmtPtr :: Ptr a -> String
fmtPtr = natToDashedHex . ptrToIntegral

fmtForeignPtr :: ForeignPtr a -> String
fmtForeignPtr = fmtPtr . unsafeForeignPtrToPtr
