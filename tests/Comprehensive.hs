{-|
Module      : Comprehensive
Description : Construct a single protein that uses most slaw features
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Comprehensive (comprehensiveProtein) where

import qualified Data.ByteString.Lazy as L
import Data.Ratio
import qualified Data.Vector.Storable as S

import Data.Slaw

comprehensiveProtein :: Slaw
comprehensiveProtein = SlawProtein (Just n15) (Just ing) rude
  where
    ing = SlawMap [ ("nil",                SlawNil)
                  , ("false",              SlawBool   False)
                  , ("true",               SlawBool   True)
                  , ("string",             "⊡")
                  , ("cons",               SlawCons "wee" "str")
                  , ("numeric singletons", SlawMap  numSing)
                  , ("numeric arrays",     SlawMap  numArr)
                  , ("proteins",           SlawList proteins)
                  ]
    rude = L.pack [1..123]

proteins :: [Slaw]
proteins = [ SlawProtein des ing (rude n)
           | des <- maybeNil
           , ing <- maybeNil
           , n   <- [0..16]
           ]
  where
    maybeNil = [Nothing, Just SlawNil]
    rude n   = L.pack $ take n [1..]

someIntegers :: [Integer]
someIntegers = map (* 0x11c964cde3bda953) [1..]

someRationals :: [Rational]
someRationals = [ num % 16 | num <- [1..] ]

ndForSize :: Int -> [NumericData]
ndForSize n =
  [ NumInt8    $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumInt16   $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumInt32   $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumInt64   $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumUnt8    $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumUnt16   $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumUnt32   $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumUnt64   $ S.fromList $ map fromInteger  $ take n someIntegers
  , NumFloat32 $ S.fromList $ map fromRational $ take n someRationals
  , NumFloat64 $ S.fromList $ map fromRational $ take n someRationals
  ]

numForFmt :: NumericFormat -> [Slaw]
numForFmt nf = [ SlawNumeric nf nd | nd <- ndForSize n ]
  where n = numericFormatSize nf

singNumFmts :: [NumericFormat]
singNumFmts = map f numFmts
  where f x = x { nfArray = False }

numSing :: [(Slaw, Slaw)]
numSing = map f ss
  where
    f s = (š (describeSlaw s), s)
    ss  = concatMap numForFmt singNumFmts

numFmts :: [NumericFormat]
numFmts = realFmts ++ cplxFmts
  where
    realFmts = [ nfr { nfVector = vt } | vt <- [VtScalar .. Vt5mv] ]
    cplxFmts = [ nfc { nfVector = vt } | vt <- [VtScalar .. Vt4  ] ]
    nfr = NumericFormat { nfArray   = True
                        , nfComplex = False
                        , nfVector  = VtScalar
                        }
    nfc = nfr { nfComplex = True }

numData :: [NumericData]
numData =
  [ NumInt8    $ S.fromList $ map fromInteger [1..96]
  , NumInt16   $ S.fromList $ map fromInteger [1..96]
  , NumInt32   $ S.fromList $ map fromInteger [1..96]
  , NumInt64   $ S.fromList $ map fromInteger [1..96]
  , NumUnt8    $ S.fromList $ map fromInteger [1..96]
  , NumUnt16   $ S.fromList $ map fromInteger [1..96]
  , NumUnt32   $ S.fromList $ map fromInteger [1..96]
  , NumUnt64   $ S.fromList $ map fromInteger [1..96]
  , NumFloat32 $ S.fromList $ map fromInteger [1..96]
  , NumFloat64 $ S.fromList $ map fromInteger [1..96]
  ]

numArr :: [(Slaw, Slaw)]
numArr = map f ss
  where
    f s = (š (describeSlaw s), s)
    ss  = [ SlawNumeric nf nd | nf <- numFmts, nd <- numData ]

n15 :: Slaw
n15 = SlawList [ "one",    "two",    "three",    "four",     "five"
               , "six",    "seven",  "eight",    "nine",     "ten"
               , "eleven", "twelve", "thirteen", "fourteen", "fifteen"
               ]
