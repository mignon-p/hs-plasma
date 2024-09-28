{-|
Module      : Main
Description : test plasma package
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}

import Control.Monad
-- import qualified Data.ByteString.Lazy     as L
-- import Data.Char
-- import Data.Complex
import Data.Default.Class
-- import Data.Either
import Data.Int
-- import qualified Data.IntMap.Strict       as IM
-- import Data.List
-- import qualified Data.Map.Strict          as M
-- import qualified Data.Text                as T
-- import qualified Data.Vector              as V
-- import qualified Data.Vector.Storable     as S
-- import Data.Word
-- import Foreign.Storable
-- import Numeric.Half
-- import System.Directory
import System.Environment
-- import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck    as QC
import qualified Test.QuickCheck.Monadic  as QC

import Data.Slaw
-- import Data.Slaw.IO
import Data.Slaw.IO.Yaml
-- import Data.Slaw.Path
-- import Data.Slaw.Semantic

import Comprehensive
import PlasmaTestUtil
import SlawInstances ()

main :: IO ()
main = do
  setIfNotSet "TASTY_COLOR" "always"
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

setIfNotSet :: String -> String -> IO ()
setIfNotSet var val = do
  old <- lookupEnv var
  case old of
    Just (_:_) -> return ()
    _          -> setEnv var val

qcProps :: TestTree
qcProps = testGroup "QuickCheck tests"
  [ QC.testProperty "round-trip IO (yaml)"     $ rtIoProp
  , QC.testProperty "round-trip (yaml string)" $ rtStrProp
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
  [ testCase "slaw-io"                    $ testSlawIO
  ]

rtIoProp :: Slaw -> QC.Property
rtIoProp s = QC.monadicIO $ do
  roundTripIOwr fpQC [s] def False

rtStrProp :: Slaw -> QC.Property
rtStrProp s = Right [s] QC.=== roundTripYamlStr [s]

testSlawIO :: Assertion
testSlawIO = do
  let pairs = [ ( def { wyoComment    = Just com
                      , wyoDirectives = Just dir
                      }
                , useName
                )
              | com     <- [minBound..maxBound]
              , dir     <- [minBound..maxBound]
              , useName <- [minBound..maxBound]
              ]
  forM_ pairs $ \(wyo, useName) -> do
    let slawx = [š wyo, SlawNil, š (5 :: Int64), comprehensiveProtein]
    roundTripIOwr fpHU slawx wyo useName

  let testFiles = [ "arf-coord"
                  , "example"
                  , "kp_enter"
                  , "protein"
                  ]
      testDir   = "test-files/"

  forM_ testFiles $ \fn -> do
    checkSlawRead2 (testDir ++ fn ++ ".yaml") (testDir ++ fn ++ ".slaw")
