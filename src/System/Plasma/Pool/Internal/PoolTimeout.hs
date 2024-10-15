{-|
Module      : System.Plasma.Pool.Internal.PoolTimeout
Description : Pool timeouts
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.PoolTimeout
  ( PoolTimeout(..)
  , encodePoolTimeout
  ) where

import Control.DeepSeq
import Data.Default.Class
import Data.Hashable
import GHC.Generics (Generic)
import Text.Printf

import Data.Slaw
import Data.Slaw.Internal
import Data.Slaw.Util

foreign import capi unsafe "libPlasma/c/pool.h value POOL_WAIT_FOREVER"
    c_wait_forever :: Double

foreign import capi unsafe "libPlasma/c/pool.h value POOL_NO_WAIT"
    c_no_wait :: Double

data PoolTimeout = NoWait
                 | Timeout {-# UNPACK #-} !Double -- ^ in seconds
                 | WaitForever
                 deriving (Show, Generic, NFData)

instance Eq PoolTimeout where
  x == y = tToDbl x == tToDbl y

instance Ord PoolTimeout where
  x `compare` y = tToDbl x ?? tToDbl y

instance Hashable PoolTimeout where
  salt `hashWithSalt` x = salt ## tToDbl x

instance Default PoolTimeout where
  def = WaitForever

instance Nameable PoolTimeout where
  typeName _ = "PoolTimeout"

zero :: Double
zero = 0

infinity :: Double
infinity = 1 / 0

tToDbl :: PoolTimeout -> Double
tToDbl NoWait      = zero
tToDbl WaitForever = infinity
tToDbl (Timeout t) = t

dblToT :: Double -> PoolTimeout
dblToT n
  | n == 0                = NoWait
  | isInfinite n && n > 0 = WaitForever
  | otherwise             = Timeout n

instance Num PoolTimeout where
  x + y       = dblToT $ tToDbl x + tToDbl y
  x - y       = dblToT $ tToDbl x - tToDbl y
  x * y       = dblToT $ tToDbl x * tToDbl y
  abs         = dblToT . abs    . tToDbl
  signum      = dblToT . signum . tToDbl
  fromInteger = dblToT . fromInteger

instance Fractional PoolTimeout where
  x / y        = dblToT $ tToDbl x / tToDbl y
  fromRational = dblToT . fromRational

instance Real PoolTimeout where
  toRational   = toRational . tToDbl

instance PrintfArg PoolTimeout where
  formatArg s fFmt =
    let t    = chrToType $ fmtChar fFmt
        fFmr = fmtTimeout s t
    in fFmr fFmt

fmtTimeout :: PoolTimeout -> FmtType -> FieldFormatter
fmtTimeout NoWait      FmtFloat  = formatRealFloat zero
fmtTimeout WaitForever FmtFloat  = formatRealFloat infinity
fmtTimeout (Timeout t) FmtString = formatString $ show t ++ " seconds"
fmtTimeout (Timeout t) _         = formatRealFloat t
fmtTimeout NoWait      _         = formatString "no wait"
fmtTimeout WaitForever _         = formatString "wait forever"

encodePoolTimeout :: PoolTimeout -> Either String Double
encodePoolTimeout NoWait      = Right c_no_wait
encodePoolTimeout WaitForever = Right c_wait_forever
encodePoolTimeout (Timeout t)
  | isNaN t             = Left "timeout was NaN"
  | t < 0               = Left "timeout was negative"
  | isInfinite t        = Right c_wait_forever
  | otherwise           = Right t
