{-|
Module      : System.Loam.Dirs
Description : Functions from ob-dirs.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Functions for dealing with standard paths.
-}

module System.Loam.Dirs
  ( StandardDir(..)
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
import System.Loam.Internal.Initialize
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

-- | Returns the pathname for one of the standard directories
-- enumerated in 'StandardDir'.  These are determined by checking
-- various environment variables and then falling back to a hardcoded
-- path.
getStandardPath :: Filename f => StandardDir -> IO f
getStandardPath sd = from8bitFn <$> getStandardPathBS sd

-- | Like 'getStandardPath', but for 'StandardDir's which are
-- paths (rather than single directories), splits them into
-- multiple directories on the colon character.  (Semicolon
-- on Windows.)
splitStandardPath :: Filename f => StandardDir -> IO [f]
splitStandardPath sd = do
  bs <- getStandardPathBS sd
  let dirs = if "Dir" `isSuffixOf` show sd
             then [bs]
             else B.split (fromIntegral c_path_char) bs
  return $ map from8bitFn $ filter (not . B.null) dirs

getStandardPathBS :: StandardDir -> IO B.ByteString
getStandardPathBS sd = do
  initialize
  cs <- c_get_standard_path $ standardDir2int sd
  if cs == C.nullConstPtr
    then return B.empty
    else B.packCString (C.unConstPtr cs)

-- | Returns the first instance of the filename in the search path
-- specified by 'StandardDir' which meets the criteria specified in
-- the searchspec.
--
-- The searchspec may contain the following characters:
--
--     * @d@ directory specified by path exists
--     * @r@ directory specified by path is readable
--     * @w@ directory specified by path is writable
--     * @c@ directory specified by path can be created
--     * @l@ directory specified by path is on a local (non-NFS) filesystem
--     * @E@ filename exists (as anything)
--     * @F@ filename exists as a regular file
--     * @D@ filename exists as a directory
--     * @R@ filename is readable
--     * @W@ filename is writable
--
-- Concatenating multiple letters together is an “and”, meaning that a
-- single component of the path must fulfill all the specified
-- conditions in order to match.  So, @Rw@ would mean the file is
-- readable, and exists in a writable directory.
--
-- The character @|@ can be used to mean “or”.  “and” binds more
-- tightly than “or”.  So, @RF|w@ means either the specified filename
-- is readable and is a regular file, or the directory in the path is
-- writable.
--
-- The character @,@ means “start over and go through all the path
-- components again”.  @|@ binds more tightly than @,@.  So, @RF,w,c@
-- means first check all the directories in the path to see if any of
-- them contain a readable, regular file named filename.  Then, go
-- through all the directories in the path and see if any of them are
-- writable.  Then, go through all the directories in the path and see
-- if any of them can be created.
--
-- Symbolic links are not treated specially.  (in other words, they
-- are always followed.)
--
-- If the given filename is absolute, then you just get it back as-is,
-- without searching the path.
--
-- Returns 'Nothing' if the filename is not found in the path.
resolveStandardPath
  :: (HasCallStack, Filename f)
  => StandardDir -- ^ standard path to search
  -> f           -- ^ file to search for
  -> T.Text      -- ^ search specification
  -> IO (Maybe f)
resolveStandardPath sd fn spec =
  listToMaybe <$> searchStandardPath0 wh sd fn spec 1 callStack
  where wh = "resolveStandardPath"

-- | Like 'resolveStandardPath', but returns all matches,
-- rather than just the first one.
searchStandardPath
  :: (HasCallStack, Filename f)
  => StandardDir -- ^ standard path to search
  -> f           -- ^ file to search for
  -> T.Text      -- ^ search specification
  -> IO [f]
searchStandardPath sd fn spec =
  searchStandardPath0 wh sd fn spec reasonableLimit callStack
  where
    wh = "searchStandardPath"
    -- Limit the number of filenames returned, just in
    -- case things get out of hand for some reason.
    reasonableLimit = 1024

searchStandardPath0
  :: Filename f
  => String
  -> StandardDir
  -> f
  -> T.Text
  -> Int64
  -> CallStack
  -> IO [f]
searchStandardPath0 wh sd fn spec limit cs = do
  mslaw <- searchStandardPath1 wh sd fn spec limit cs
  case fmap ŝee mslaw of
    Nothing            -> return [] -- I don't think this can happen?
    Just (Left  exc  ) -> throwIO exc
    Just (Right bsLst) ->
      return $ map from8bitFn $ filter (not . B.null) bsLst

searchStandardPath1
  :: Filename f
  => String
  -> StandardDir
  -> f
  -> T.Text
  -> Int64
  -> CallStack
  -> IO (Maybe Slaw)
searchStandardPath1 wh sd fn spec limit cs = do
  initialize
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
        withReturnedRetortCS EtOther addn erl cs $ \tortPtr -> do
          c_search_standard_path sd' fnPtr specPtr limit tortPtr lenPtr
