{-|
Module      : System.Loam.Dirs
Description : Functions from ob-dirs.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Dirs
  ( Filename
  , StandardDir(..)
  , getStandardPath
  , splitStandardPath
  , resolveStandardPath
  , searchStandardPath
  ) where

import Data.Int
import qualified Data.Text                as T
-- import Data.Word
import Foreign.C.Types
import Foreign.Ptr

import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Enums
import System.Loam.Internal.Filename
-- import System.Loam.Internal.StringMarshal

foreign import capi unsafe "libLoam/c/ob-dirs.h value OB_PATH_CHAR"
    c_path_char :: CChar

foreign import capi "libLoam/c/ob-dirs.h ob_get_standard_path"
    c_get_standard_path :: CInt -> IO C.ConstCString

foreign import capi "ze-hs-plasma.h ze_hs_plasma_search_standard_path"
    c_search_standard_path
      :: CInt
      -> C.ConstCString
      -> C.ConstCString
      -> Int64
      -> Ptr Int64
      -> IO (Ptr ())

getStandardPath :: Filename f => StandardDir -> IO (Maybe f)
getStandardPath = undefined

splitStandardPath :: Filename f => StandardDir -> IO [f]
splitStandardPath = undefined

resolveStandardPath
  :: Filename f
  => StandardDir
  -> f
  -> T.Text
  -> IO (Maybe f)
resolveStandardPath = undefined

searchStandardPath
  :: Filename f
  => StandardDir
  -> f
  -> T.Text
  -> IO [f]
searchStandardPath = undefined
