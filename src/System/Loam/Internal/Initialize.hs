{-|
Module      : System.Loam.Internal.Initialize
Description : Some things to do once before other stuff
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Internal.Initialize
  ( initialize
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString          as B
-- import Data.Default.Class
import Data.Int
import Data.List
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Data.Word
-- import Foreign.C.String
import Foreign.C.Types
-- import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
-- import GHC.Stack
import System.Environment
import System.IO.Unsafe

import Data.Slaw
import qualified System.Loam.Internal.ConstPtr as C
-- import System.Loam.Retorts

foreign import capi  "libLoam/c/ob-util.h ob_set_program_arguments"
    c_set_program_arguments :: CInt -> C.ConstPtr C.ConstCString -> IO ()

setProgArgs :: IO ()
setProgArgs = do
  argv0 <- getProgName
  argv  <- getArgs
  spa0 $ map (T.encodeUtf8 . T.pack) (argv0 : argv)

spa0 :: [B.ByteString] -> IO ()
spa0 = spa1 []

spa1 :: [C.ConstCString] -> [B.ByteString] -> IO ()
spa1 ptrs [] = withArrayLen (reverse ptrs) spa2
spa1 ptrs (bs:rest) =
  C.useAsConstCString bs $ \bsPtr -> spa1 (bsPtr : ptrs) rest

spa2 :: Int -> Ptr C.ConstCString -> IO ()
spa2 argc argv = do
  c_set_program_arguments (fromIntegral argc) (C.ConstPtr argv)

checkSizes :: IO ()
checkSizes = do
  let quad = [4]
      oct  = [8]

  chkSize (0 :: Word64)  oct
  chkSize (0 :: Int64)   oct
  chkSize (0 :: Double)  oct
  chkSize (0 :: CDouble) oct

  chkSize (0 :: Word32)  quad
  chkSize (0 :: Int32)   quad
  chkSize (0 :: Float)   quad
  chkSize (0 :: CFloat)  quad

  chkSize (0 :: Word)    (quad ++ oct)

  let nativeSize = [sizeOf (0 :: Word)]
  chkSize (0 :: Int)     nativeSize

chkSize
  :: (Storable a, Nameable a)
  => a
  -> [Int]
  -> IO ()
chkSize x expected = do
  let sz = sizeOf x
  when (not $ sz `elem` expected) $ do
    let msg1 = concat [ "sizeOf "
                      , typeName x
                      , " is "
                      , show sz
                      , ", but expected "
                      ]
        msg2 = intercalate " or " $ map show expected
    fail $ msg1 ++ msg2

initialize1 :: IO ()
initialize1 = do
  checkSizes
  setProgArgs

{-# NOINLINE initialize0 #-}
initialize0 :: ()
initialize0 = unsafePerformIO initialize1

initialize :: IO ()
initialize = evaluate initialize0
