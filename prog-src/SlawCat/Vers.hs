{-|
Module      : SlawCat.Vers
Description : version information which is displayed with "--version"
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}
{-# OPTIONS_GHC -Wno-unused-imports     #-}

module SlawCat.Vers ( scVers ) where

import Data.Char
import qualified Data.Text                as T
import Data.Version
import System.Info

import System.Loam.Version

cvers :: (T.Text, T.Text)
#if defined(VERSION_base) && MIN_VERSION_base(4,15,0)
cvers = (T.pack compilerName, (T.pack . showVersion) fullCompilerVersion)
#elif defined(TOOL_VERSION_ghc)
cvers = ("ghc", TOOL_VERSION_ghc)
#else
cvers = (T.pack compilerName, (T.pack . showVersion) compilerVersion)
#endif

vers :: [(T.Text, T.Text)]
vers =
  [ cvers
#ifdef VERSION_base
  , ("base", VERSION_base)
#endif
#ifdef VERSION_hs_slaw
  , ("hs-slaw", VERSION_hs_slaw)
#endif
#ifdef VERSION_hs_plasma
  , ("hs-plasma", VERSION_hs_plasma)
#endif
  ]

handleEmptyPkg :: T.Text -> T.Text
handleEmptyPkg pkg
  | T.null pkg = "libPlasma"
  | otherwise  = pkg

handleEmptyVers :: T.Text -> T.Text
handleEmptyVers v
  | T.null vv = "(unknown?)"
  | otherwise = vv
  where vv = T.stripStart v

scVers :: IO [(T.Text, T.Text)]
scVers = do
  plasmaVers <- getVersion GspeakVersion
  let (pkgTxt, versTxt) = T.span (not . isSpace) $ T.strip plasmaVers
      pair1 = (handleEmptyPkg pkgTxt, handleEmptyVers versTxt)
      pair2 = ("platform", T.pack (os ++ "/" ++ arch))
  return $ reverse $ pair2 : pair1 : vers
