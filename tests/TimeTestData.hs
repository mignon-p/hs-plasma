{-|
Module      : TimeTestData
Description : Test data for testing System.Loam.Time
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module TimeTestData
  ( TimeTest(..)
  , timeTestData
  ) where

import qualified Data.Text                as T

import System.Loam.Retorts
import System.Loam.Retorts.Constants

data TimeTest = GoodTime { ttIn   :: !T.Text
                         , ttOut  :: !T.Text
                         }
              | BadTime  { ttIn   :: !T.Text
                         , ttTort :: !Retort
                         }
              deriving (Eq, Show)

timeTestData :: [TimeTest]
timeTestData =
  [ GoodTime "Dec 20, 2024 13:30:53.63 "    "Dec 20, 2024 13:30:53.63"
  , GoodTime "Dec 20, 2024 13:30:53.63"     "Dec 20, 2024 13:30:53.63"
  , GoodTime "Dec 20, 2024 13:30:53.631 "   "Dec 20, 2024 13:30:53.63"
  , GoodTime "Dec 20, 2024 13:30:53.631"    "Dec 20, 2024 13:30:53.63"
  , GoodTime "Dec 20, 2024 13:30:53.50 "    "Dec 20, 2024 13:30:53.50"
  , GoodTime "Dec 20, 2024 13:30:53.50"     "Dec 20, 2024 13:30:53.50"
  , GoodTime "Dec 20, 2024 13:30:53.5 "     "Dec 20, 2024 13:30:53.50"
  , GoodTime "Dec 20, 2024 13:30:53.5"      "Dec 20, 2024 13:30:53.50"
  , GoodTime "Dec 20, 2024 13:30:53 "       "Dec 20, 2024 13:30:53.00"
  , GoodTime "Dec 20, 2024 13:30:53"        "Dec 20, 2024 13:30:53.00"
  , BadTime  "Dec 32, 2024 13:30:53.63"     OB_PARSE_ERROR
  , BadTime  "Dec 20, 2024 25:30:53.63"     OB_PARSE_ERROR
  , BadTime  "Dec 20, 2024 13:30:xx.63"     OB_PARSE_ERROR
  , BadTime  "Dec 20, 2024 13:30:53.xx"     OB_PARSE_ERROR
  , BadTime  "Dec 20, 867-5309 13:30:53.63" OB_PARSE_ERROR
  , BadTime  "Blob 20, 2024 13:30:53.63"    OB_PARSE_ERROR
  , BadTime  ".63"                          OB_PARSE_ERROR
  , BadTime  ""                             OB_PARSE_ERROR
  ]
