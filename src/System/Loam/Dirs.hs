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

import Control.Exception
import qualified Data.ByteString          as B
import Data.Default.Class
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
-- import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Stack

import Data.Slaw
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Enums
import System.Loam.Internal.Filename
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Marshal

foreign import capi unsafe "libLoam/c/ob-dirs.h value OB_PATH_CHAR"
    c_path_char :: CChar

foreign import capi "libLoam/c/ob-dirs.h ob_get_standard_path"
    c_get_standard_path :: CInt -> IO C.ConstCString

foreign import capi "ze-hs-misc.h ze_hs_search_standard_path"
    c_search_standard_path
      :: CInt             -- ob_standard_dir dir
      -> C.ConstCString   -- const char     *filename
      -> C.ConstCString   -- const char     *searchspec
      -> Int64            -- int64           max_to_return
      -> Ptr Int64        -- ob_retort      *retort_ptr
      -> Ptr SlawLen      -- int64          *len_ptr
      -> IO (Ptr FgnSlaw) -- slaw            (return value)

getStandardPath :: Filename f => StandardDir -> IO f
getStandardPath sd = from8bitFn <$> getStandardPathBS sd

splitStandardPath :: Filename f => StandardDir -> IO [f]
splitStandardPath sd = do
  bs <- getStandardPathBS sd
  let dirs = if "Dir" `isSuffixOf` show sd
             then [bs]
             else B.split (fromIntegral c_path_char) bs
  return $ map from8bitFn $ filter (not . B.null) dirs

getStandardPathBS :: StandardDir -> IO B.ByteString
getStandardPathBS sd = do
  cs <- c_get_standard_path $ standardDir2int sd
  if cs == C.nullConstPtr
    then return B.empty
    else B.packCString (C.unConstPtr cs)

resolveStandardPath
  :: (HasCallStack, Filename f)
  => StandardDir
  -> f
  -> T.Text
  -> IO (Maybe f)
resolveStandardPath sd fn spec = withFrozenCallStack $ do
  listToMaybe <$> searchStandardPath0 wh sd fn spec 1
  where wh = "resolveStandardPath"

searchStandardPath
  :: (HasCallStack, Filename f)
  => StandardDir
  -> f
  -> T.Text
  -> IO [f]
searchStandardPath sd fn spec =
  withFrozenCallStack $ searchStandardPath0 wh sd fn spec reasonableLimit
  where
    wh = "searchStandardPath"
    -- Limit the number of filenames returned, just in
    -- case things get out of hand for some reason.
    reasonableLimit = 1024

searchStandardPath0
  :: (HasCallStack, Filename f)
  => String
  -> StandardDir
  -> f
  -> T.Text
  -> Int64
  -> IO [f]
searchStandardPath0 wh sd fn spec limit = do
  mslaw <- searchStandardPath1 wh sd fn spec limit
  case fmap ŝee mslaw of
    Nothing            -> return [] -- I don't think this can happen?
    Just (Left  exc  ) -> throwIO exc
    Just (Right bsLst) ->
      return $ map from8bitFn $ filter (not . B.null) bsLst

searchStandardPath1
  :: (HasCallStack, Filename f)
  => String
  -> StandardDir
  -> f
  -> T.Text
  -> Int64
  -> IO (Maybe Slaw)
searchStandardPath1 wh sd fn spec limit = do
  let sd'     = standardDir2int  sd
      fn'     = to8bitFn         fn
      spec'   = T.encodeUtf8     spec
      addn    = Just             wh
      erl     = Just $           def { elSource = DsOther erlStr  }
      slawErl =                  def { elSource = DsOther sErlStr }
      erlStr  = "searchspec " ++ show spec
      sErlStr = "<internal:"  ++ wh ++ ">"
  C.useAsConstCString fn' $ \fnPtr -> do
    C.useAsConstCString spec' $ \specPtr -> do
      withReturnedSlaw slawErl $ \lenPtr -> do
        withReturnedRetort EtOther addn erl $ \tortPtr -> do
          c_search_standard_path sd' fnPtr specPtr limit tortPtr lenPtr
