{-|
Module      : GangTest
Description : Test pool gangs
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module GangTest
  ( testGangs
  ) where

import Control.Monad
import Data.Default.Class
import Data.List
import qualified Data.Map.Strict          as M
import Data.Maybe
import qualified Data.Text                as T
import GHC.Stack
import Test.Tasty.HUnit

-- import Data.Slaw
import Data.Slaw.Util
import System.Plasma.Pool

type GangMap = M.Map T.Text T.Text

makeHoses :: String -> Int -> IO [(PoolName, Hose)]
makeHoses pfx num = do
  forM [1..num] $ \n -> do
    let name  = pfx ++ show n
        pName = toPoolName name
        hName = toText     name
    hose <- participateCreatingly def hName pName small
    return (pName, hose)

addHose :: Gang -> Hose -> GangMap -> IO GangMap
addHose gang hose gm = do
  joinGang gang hose
  return $ M.insert (hoseName hose) (gangName gang) gm

addHoses :: Gang -> [Hose] -> GangMap -> IO GangMap
addHoses _    []          !gm = return gm
addHoses gang (hose:rest) !gm = do
  gm' <- addHose gang hose gm
  addHoses gang rest gm'

validateGang :: HasCallStack => Gang -> GangMap -> Assertion
validateGang gang gm = do
  let gName    = gangName gang
      expected = mapMaybe (checkPair gName) $ M.toList gm
  hoses <- getGangMembers gang
  let actual = map hoseName hoses
  sort expected @=? sort actual

checkPair :: T.Text -> (T.Text, T.Text) -> Maybe T.Text
checkPair expected (hName, gName)
  | expected == gName = Just hName
  | otherwise         = Nothing

testGangs :: Assertion
testGangs = do
  sharks <- newGang "Sharks"
  jets   <- newGang "Jets"

  validateGang sharks M.empty
  validateGang jets   M.empty

  sharksPairs <- makeHoses "sharks" 10
  jetsPairs   <- makeHoses "jets"   10

  gm1 <- addHoses sharks (map snd sharksPairs) M.empty
  gm2 <- addHoses jets   (map snd jetsPairs)   gm1

  validateGang sharks gm2
  validateGang jets   gm2

  -- TODO more stuff

  withdrawAll sharks
  withdrawAll jets

  validateGang sharks M.empty
  validateGang jets   M.empty

  mapM_ (dispose def) $ map fst $ sharksPairs ++ jetsPairs
