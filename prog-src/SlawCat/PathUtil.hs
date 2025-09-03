{-|
Module      : SlawCat.PathUtil
Description : make filenames prettier for display
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module SlawCat.PathUtil
  ( substTilde
  , isStdInOut
  , fixName
  ) where

import Data.List
import Data.Maybe
import qualified Data.Text                as T
import System.Environment
import System.FilePath
import System.IO.Unsafe

import SlawCat.Types

{-# NOINLINE home #-}
home :: FilePath
home = fromMaybe "" $ unsafePerformIO $ lookupEnv "HOME"

substTilde :: FilePath -> FilePath
substTilde orig
  | isRelative orig              = orig
  | null home || isRelative home = orig
  | tilde                        = '~' : rest
  | otherwise                    = orig
  where h     = dropTrailingPathSeparator home
        hLen  = length h
        rest  = drop hLen orig
        slash = case rest of
                  ('/' :_) -> True
                  ('\\':_) -> True
                  _        -> False
        tilde = h `isPrefixOf` orig && slash

isStdInOut :: FilePath -> Bool
isStdInOut ""  = True
isStdInOut "-" = True
isStdInOut _   = False

fixName :: FilePath -> IoDir -> T.Text
fixName name _        | not (isStdInOut name) = T.pack $ substTilde name
fixName _    DirInput                         = "stdin"
fixName _    DirOutput                        = "stdout"
