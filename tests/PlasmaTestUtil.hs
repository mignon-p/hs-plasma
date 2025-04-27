{-|
Module      : PlasmaTestUtil
Description : Helper functions for testing slaw IO
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}

module PlasmaTestUtil
  ( AssEqFunc
  , IoFunc
  , PropIO
  , fpHU
  , fpQC
  , roundTripIOwr
  , checkSlawRead
  , checkSlawRead2
  , roundTripYamlStr
  , roundTripCtxOpts
  ) where

import Control.Monad
-- import qualified Data.ByteString          as B
-- import qualified Data.ByteString.Lazy     as L
-- import Data.Default.Class
import GHC.Stack
import System.Directory
-- import System.Environment
import System.IO
import System.IO.Unsafe
-- import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Monadic  as QC
-- import Test.Tasty
import Test.Tasty.HUnit

import Data.Slaw
import Data.Slaw.IO
import Data.Slaw.IO.Yaml
import System.Plasma.Pool

type AssEqFunc m = forall a. (HasCallStack, Eq a, Show a) => String -> a -> a -> m ()
type IoFunc    m = forall a. IO a -> m a
type FuncPair  m = (AssEqFunc m, IoFunc m)
type PropIO      = QC.PropertyM IO

fpHU :: FuncPair IO
fpHU = (assertEqual, id)

fpQC :: FuncPair PropIO
fpQC = (qcAssEq, QC.run)

qcAssEq :: AssEqFunc PropIO
qcAssEq msg x y = do
  let msg' = msg ++ ": " ++ show x ++ " ≠ " ++ show y
  QC.assertWith (x == y) msg'

tmpDir :: FilePath
tmpDir = unsafePerformIO getTemporaryDirectory

roundTripIOwr :: (HasCallStack, Monad m)
              => FuncPair m
              -> [Slaw]
              -> WriteYamlOptions
              -> Bool
              -> m ()
roundTripIOwr (assEq, io) ss wyo useName = do
  (fname, h) <- io $ openBinaryTempFile tmpDir "test.yaml"
  io $ if useName
       then writeYamlSlawFile          h  wyo ss
       else writeYamlSlawFile (NoClose h) wyo ss

  ss' <- io $ if useName
              then readYamlSlawFile fname ()
              else hSeek h AbsoluteSeek 0 >> readYamlSlawFile h ()
  io $ removeFile fname

  let len  = length ss
      len' = length ss'
      pfx1 = concat [", useName = ", show useName, ", ", show wyo]
  assEq ("length" ++ pfx1) len len'

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = concat ["slaw #", show (i :: Int), pfx1]
    assEq pfx s s'

checkSlawRead :: HasCallStack
              => FilePath
              -> [Slaw]
              -> IO ()
checkSlawRead fname ss = do
  ss' <- readSlawFile fname ()
  let nExpected = length ss
      nActual   = length ss'
  assertEqual (fname ++ ":length") nExpected nActual

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = fname ++ ":slaw #" ++ show (i :: Int)
    assertEqual pfx s s'

checkSlawRead2 :: HasCallStack
               => FilePath
               -> FilePath
               -> IO ()
checkSlawRead2 fnActual fnExpected =
  readSlawFile fnExpected () >>= checkSlawRead fnActual

roundTripYamlStr :: [Slaw] -> Either PlasmaException [Slaw]
roundTripYamlStr ss =
  slawToYamlString ss () >>= (`slawFromYamlString` ())

roundTripCtxOpts :: ContextOptions -> ContextOptions
roundTripCtxOpts opts = unsafePerformIO $ do
  ctx <- newContext "" opts
  getContextOptions ctx
