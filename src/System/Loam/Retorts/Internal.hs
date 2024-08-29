{-|
Module      : System.Loam.Retorts.Internal
Description : Internal retort stuff
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Retorts.Internal
  ( RetortInfo(..)
  , RetortMap
  , retortMap
  , getRetortStringFromC
  ) where

import Control.DeepSeq
import qualified Data.ByteString.Unsafe   as B
import Data.Hashable
import Data.Int
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
-- import Foreign.C
-- import Foreign.Ptr
import GHC.Generics (Generic)

import Data.Slaw
import qualified System.Loam.Internal.ConstPtr as C
-- import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.Descriptions

foreign import capi "libLoam/c/ob-retorts.h ob_error_string_literal"
    c_error_string_literal :: Int64 -> IO C.ConstCString

data RetortInfo = RetortInfo
  { riName :: T.Text
  , riDesc :: T.Text
  , riType :: Maybe PlasmaExceptionType
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

type RetortMap = M.Map Retort RetortInfo

retortMap :: RetortMap
retortMap = M.fromList $ map f tortTuples
  where f (r, n, d, t) = (r, RetortInfo n d t)

getRetortStringFromC :: Retort -> IO T.Text
getRetortStringFromC (Retort r) = do
  cs <- c_error_string_literal r
  if cs == C.nullConstPtr
    then return T.empty
    else T.decodeUtf8Lenient <$> B.unsafePackCString (C.unConstPtr cs)
