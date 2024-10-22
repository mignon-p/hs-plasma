{-|
Module      : PoolTestFixture
Description : Test fixture for pools
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module PoolTestFixture
  ( poolTestFixture
  , tstProteins
  , tstFops
  , expProteins
  ) where

-- import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy     as L
-- import Data.Char
-- import Data.Complex
import Data.Default.Class
-- import Data.Either
import Data.Int
-- import qualified Data.IntMap.Strict       as IM
-- import Data.List
-- import qualified Data.Map.Strict          as M
-- import Data.Maybe
-- import qualified Data.Set                 as S
-- import qualified Data.Text                as T
-- import qualified Data.Vector              as V
-- import qualified Data.Vector.Storable     as S
import Data.Word
-- import Foreign.Storable
-- import Numeric.Half
-- import System.Directory
-- import System.Environment
-- import System.IO
-- import Test.Tasty
-- import Test.Tasty.HUnit

import Data.Slaw
-- import Data.Slaw.IO
-- import Data.Slaw.IO.Yaml
-- import Data.Slaw.Path
-- import Data.Slaw.Semantic
-- import Data.Slaw.Util
-- import System.Loam.File
-- import System.Loam.Hash
-- import System.Loam.Rand
-- import System.Loam.Retorts.Constants
import System.Plasma.Pool

mkProt :: L.ByteString -> Word8 -> Slaw
mkProt name num = SlawProtein (Just des) (Just ing) name
  where
    des = SlawList ["test", SlawString name]
    ing = SlawMap  [("n", š num)]

noDes :: Slaw -> Slaw
noDes p = p { slawDescrips = Nothing }

noIng :: Slaw -> Slaw
noIng p = p { slawIngests  = Nothing }

trimRude :: Slaw -> Int64 -> Int64 -> Slaw
trimRude p start len = p { slawRudeData = rude }
  where rude = L.take len $ L.drop start $ slawRudeData p

tstProteins :: [Slaw]
tstProteins = [p1, p2, p3, p4, p5]
  where
    p1 = mkProt "keratin"    1
    p2 = mkProt "hemoglobin" 2
    p3 = mkProt "casein"     3
    p4 = mkProt "insulin"    4
    p5 = mkProt "lactase"    5

tstFops :: [FetchOp]
tstFops =
  [ def { foIdx          = 0 }
  , def { foIdx          = 1
        , foRudeOffset   = Nothing
        }
  , def { foIdx          = 2
        , foWantDescrips = False
        }
  , def { foIdx          = 3
        , foWantIngests  = False
        }
  , def { foIdx          = 4
        , foRudeLength   = Just 4
        }
  , def { foIdx          = 0
        , foRudeOffset   = Just 1
        }
  , def { foIdx          = 1
        , foRudeOffset   = Just 2
        , foRudeLength   = Just 5
        }
  ]

expProteins :: [Slaw]
expProteins =
  [ p1
  , trimRude p2 0 0
  , noDes    p3
  , noIng    p4
  , trimRude p5 0 4
  , trimRude p1 1 6
  , trimRude p2 2 5
  ]
  where [p1, p2, p3, p4, p5] = tstProteins

poolTestFixture :: (Hose -> [RetProtein] -> IO a) -> IO a
poolTestFixture action = do
  withTemporaryPool def Nothing small $ \pool -> do
    withHose def "" pool $ \hose -> do
      depositTestProteins hose >>= action hose

depositTestProteins :: Hose -> IO [RetProtein]
depositTestProteins hose = do
  forM tstProteins $ \p -> do
    (actualIdx, ts) <- deposit hose p
    return $ RetProtein p actualIdx ts
