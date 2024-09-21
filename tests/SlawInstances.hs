{-|
Module      : SlawInstances
Description : Arbitrary instance for Slaw
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module SlawInstances where

-- import Data.Bits
import qualified Data.ByteString.Lazy    as L
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector.Storable    as S
import Test.QuickCheck
-- import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Unicode as U
import Text.Printf

import Data.Slaw

decrease :: Double -> Int -> Int
decrease factor x = floor (factor * fromIntegral x)

arbHalf :: Arbitrary a => Gen a
arbHalf = scale (decrease 0.4) arbitrary

uniqMap :: [(Slaw, Slaw)] -> Slaw
uniqMap = SlawMap . removeDups

slawString :: Gen Slaw
slawString = do
  str <- U.string
  let txt = LT.pack str
  return $ SlawString $ LT.encodeUtf8 txt

numericFormat :: Gen NumericFormat
numericFormat = do
  isArray   <- arbitrary
  isComplex <- arbitrary
  let upperBound = if isComplex then Vt4 else Vt5mv
  vt        <- chooseEnum (VtScalar, upperBound)
  return $ NumericFormat { nfArray   = isArray
                         , nfVector  = vt
                         , nfComplex = isComplex
                         }

flt32 :: Gen Float
flt32 = do
  x <- arbitrary
  let str = printf "%.6g" (x :: Float)
  return $ read str

flt64 :: Gen Double
flt64 = do
  x <- arbitrary
  let str = printf "%.15g" (x :: Double)
  return $ read str

numericData :: Int -> Gen NumericData
numericData len = oneof
  [
        (NumInt8    . S.fromList) <$> vectorOf len arbitrary
  ,     (NumInt16   . S.fromList) <$> vectorOf len arbitrary
  ,     (NumInt32   . S.fromList) <$> vectorOf len arbitrary
  ,     (NumInt64   . S.fromList) <$> vectorOf len arbitrary
  ,     (NumUnt8    . S.fromList) <$> vectorOf len arbitrary
  ,     (NumUnt16   . S.fromList) <$> vectorOf len arbitrary
  ,     (NumUnt32   . S.fromList) <$> vectorOf len arbitrary
  ,     (NumUnt64   . S.fromList) <$> vectorOf len arbitrary
  ,     (NumFloat32 . S.fromList) <$> vectorOf len flt32
  ,     (NumFloat64 . S.fromList) <$> vectorOf len flt64
  ]

slawNumeric :: Gen Slaw
slawNumeric = do
  nf <- numericFormat
  breadth <- if nfArray nf
             then sized (chooseInt . (0,))
             else return 1
  let cplxSize = if nfComplex nf then 2 else 1
      vectSize = vectorSize (nfVector nf)
  nd <- numericData (breadth * cplxSize * vectSize)
  return $ SlawNumeric nf nd

depth0 :: Gen Slaw
depth0 = frequency
  [ (1, return SlawNil)
  , (2, SlawBool <$> arbitrary)
  , (3, slawString)
  , (3, slawNumeric)
  ]

depth1 :: Gen Slaw
depth1 = frequency
  [ (5,  SlawCons <$> depth0 <*> depth0)
  , (10, SlawList <$> listOf depth0)
  , (1,  return $ SlawList $ replicate 100 SlawNil)
  , (3,  uniqMap  <$> listOf shallowPair)
  ]

deepSlaw :: Gen Slaw
deepSlaw = frequency
  [ (2, SlawCons <$> arbHalf <*> arbHalf)
  , (2, SlawList <$> listOf arbHalf)
  , (1, uniqMap  <$> listOf deepPair)
  , (2, slawProtein)
  ]

deepPair :: Gen (Slaw, Slaw)
deepPair = (,) <$> arbHalf <*> arbHalf

shallowPair :: Gen (Slaw, Slaw)
shallowPair = (,) <$> depth0 <*> depth0

slawProtein :: Gen Slaw
slawProtein = do
  des <- frequency [ (5, (Just . SlawList) <$> listOf slawString)
                   , (1, Just <$> arbHalf)
                   , (1, return Nothing)
                   ]
  ing <- frequency [ (5, Just <$> stringMap)
                   , (1, Just <$> arbHalf)
                   , (1, return Nothing)
                   ]
  rude <- listOf $ chooseInt (0, 255)
  return $ SlawProtein des ing $ L.pack $ map fromIntegral rude

stringMap :: Gen Slaw
stringMap = uniqMap <$> listOf stringEntry

stringEntry :: Gen (Slaw, Slaw)
stringEntry = do
  key   <- slawString
  value <- frequency [ (1, depth0)
                     , (1, depth1)
                     ]
  return (key, value)

shrinkString :: L.ByteString -> [L.ByteString]
shrinkString lbs
  | L.null lbs = []
  | otherwise  =
      let lbs' = L.drop 1 lbs
      in shrinkString lbs' ++ [lbs']

shrSlawList :: [Slaw] -> [Slaw]
shrSlawList [] = []
shrSlawList xs =
  concatMap shrink xs ++ xs ++ map SlawList (reverse (drop 1 (tails xs)))

shrSlawMap :: [(Slaw, Slaw)] -> [Slaw]
shrSlawMap []    = []
shrSlawMap pairs = shrunkTerms ++ subMaps
  where
    shrunkTerms = concatMap shrinkPairToMap pairs
    subMaps     = map SlawMap (reverse (drop 1 (tails pairs)))

shrinkPair :: (Slaw, Slaw) -> [Slaw]
shrinkPair (car, cdr) =
  shrink car ++ shrink cdr ++ [car, cdr] ++ shrunkBoth
  where shrunkBoth = zipWith SlawCons (shrink car) (shrink cdr)

shrinkPairToMap :: (Slaw, Slaw) -> [Slaw]
shrinkPairToMap = map f . shrinkPair
  where f (SlawCons car cdr) = SlawMap [(car, cdr)]
        f x                  = x

shrSlawNum :: NumericFormat -> NumericData -> [Slaw]
shrSlawNum nf nd
  | lengthNumericData nd == 0 = []
  | otherwise                 = catMaybes $ simpler ++ [unArray]
  where
    unVector  x = x { nfVector  = VtScalar }
    unComplex x = x { nfComplex = False    }
    simpler     = map (simplifyFormat nf nd) [ unVector . unComplex
                                             , unVector
                                             , unComplex
                                             ]
    unArray     = if nfArray nf
                  then Just $ SlawNumeric (nf { nfArray = False }) singl
                  else Nothing
    singl       = sliceNumericData 0 bsize nd
    bsize       = cplxSize * vectSize
    cplxSize    = if nfComplex nf then 2 else 1
    vectSize    = vectorSize (nfVector nf)

simplifyFormat :: NumericFormat
               -> NumericData
               -> (NumericFormat -> NumericFormat)
               -> Maybe Slaw
simplifyFormat nf nd fun =
  if nf' == nf
  then Nothing
  else Just $ SlawNumeric (nf' { nfArray = True }) nd
  where nf' = fun nf

instance Arbitrary Slaw where
  arbitrary = do
    frequency
      [ (3, depth0)
      , (3, depth1)
      , (3, deepSlaw)
      , (3, stringMap)
      ]

  shrink (SlawString s) = map SlawString $ shrinkString s
  shrink (SlawCons car cdr)  = shrinkPair (car, cdr)
  shrink (SlawList xs)       = shrSlawList xs
  shrink (SlawMap pairs)     = shrSlawMap pairs
  shrink (SlawNumeric nf nd) = shrSlawNum nf nd
  shrink _ = []

instance Arbitrary ByteOrder where
  arbitrary = chooseEnum (minBound, maxBound)
