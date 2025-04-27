{-|
Module      : System.Loam.Util
Description : Functions from ob-util.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Some totally miscellaneous stuff that doesn't fit anywhere else.
-}

module System.Loam.Util
  ( generateUuid
  , getUserName
  , getProgName
  , setProgName
  ) where

-- import Control.Exception
import qualified Data.ByteString          as B
-- import Data.Default.Class
import Data.Int
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Foreign.C.String
-- import Foreign.C.Types
import Foreign.Marshal.Alloc
-- import Foreign.Ptr
import GHC.Stack

import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.Initialize
import System.Loam.Retorts

foreign import capi "libLoam/c/ob-util.h ob_generate_uuid"
    c_generate_uuid :: CString -> IO Int64

foreign import capi  "libLoam/c/ob-util.h ob_get_user_name"
    c_get_user_name :: IO C.ConstCString

foreign import capi  "libLoam/c/ob-util.h ob_get_prog_name"
    c_get_prog_name :: IO C.ConstCString

foreign import capi  "libLoam/c/ob-util.h ob_set_prog_name"
    c_set_prog_name :: C.ConstCString -> IO ()

-- | Generate a random (Version 4) UUID, as a string
-- containing 32 hex digits and 4 dashes.
generateUuid :: HasCallStack => IO T.Text
generateUuid = do
  initialize
  allocaBytes 40 $ \ptr -> do
    tort <- Retort <$> c_generate_uuid ptr
    throwRetortCS_ EtOther (Just "generateUuid") tort Nothing callStack
    T.decodeUtf8Lenient <$> B.packCString ptr

-- | Fetch the current user's username, or “unknown” on error.
getUserName :: IO T.Text
getUserName = initialize >> c_get_user_name >>= constCStrToTxt

-- | Return the name of the current program.  Can be set with
-- 'setProgName', but defaults to @argv[0]@.
getProgName :: IO T.Text
getProgName = initialize >> c_get_prog_name >>= constCStrToTxt

-- | Sets the name returned by 'getProgName'.
setProgName :: T.Text -> IO ()
setProgName txt = do
  initialize
  B.useAsCString (T.encodeUtf8 txt) $ \ptr -> do
    c_set_prog_name (C.ConstPtr ptr)

constCStrToTxt :: C.ConstCString -> IO T.Text
constCStrToTxt ccs = do
  if ccs == C.nullConstPtr
    then return T.empty
    else T.decodeUtf8Lenient <$> B.packCString (C.unConstPtr ccs)
