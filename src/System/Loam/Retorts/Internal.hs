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
  , isSuccess
  , isFailure
  , getRetortInfo
  , retortToPlasmaException
  , throwRetort
  , throwRetort'
  ) where

import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.Unsafe   as B
import Data.Default.Class
import Data.Hashable
import Data.Int
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Foreign.C.Types (CInt(..))
import Foreign.C.Error
-- import Foreign.Ptr
import GHC.Generics (Generic)
import GHC.Stack

import Data.Slaw
import Data.Slaw.Internal ((?>))
import qualified System.Loam.Internal.ConstPtr as C
-- import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.Descriptions

foreign import capi "libLoam/c/ob-retorts.h ob_error_string_literal"
    c_error_string_literal :: Int64 -> IO C.ConstCString

foreign import capi unsafe "libLoam/c/ob-retorts.h ob_retort_to_errno"
    c_retort_to_errno :: Int64 -> CInt

data RetortInfo = RetortInfo
  { riName :: T.Text
  , riDesc :: T.Text
  , riType :: Maybe PlasmaExceptionType
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default RetortInfo where
  def = RetortInfo
        { riName = T.empty
        , riDesc = T.empty
        , riType = Nothing
        }

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

getRetortString :: Retort -> IO T.Text
getRetortString r = do
  txt <- getRetortStringFromC r
  if T.null txt
    then return $ T.pack $ "Retort " ++ show (unRetort r)
    else return txt

getRetortInfo :: Retort -> IO RetortInfo
getRetortInfo r =
  case r `M.lookup` retortMap of
    Just ri -> return ri
    Nothing -> mkRetortInfo <$> getRetortString r

mkRetortInfo :: T.Text -> RetortInfo
mkRetortInfo txt = def { riName = txt }

{-# INLINE isSuccess #-}
isSuccess :: Retort -> Bool
isSuccess (Retort r) = r >= 0

{-# INLINE isFailure #-}
isFailure :: Retort -> Bool
isFailure (Retort r) = r < 0

retortToPlasmaException :: PlasmaExceptionType -- default exception type
                        -> Maybe String -- additional information/loc
                        -> Retort
                        -> Maybe ErrLocation -- file or pool
                        -> IO PlasmaException
retortToPlasmaException et addn r erl = do
  ri <- getRetortInfo r
  let desc = riDesc ri
      s1   = case addn of
               Nothing -> ""
               Just s  -> s ++ ": "
      s2   = T.unpack $ riName ri
      s3   = if T.null desc
             then ""
             else " (" ++ T.unpack desc ++ ")"
  return $ def { peType     = riType ri ?> et
               , peRetort   = Just r
               , peMessage  = concat [s1, s2, s3]
               , peLocation = erl
               }

throwRetort :: HasCallStack
            => PlasmaExceptionType -- default exception type
            -> Maybe String        -- additional information/loc
            -> Retort
            -> Maybe ErrLocation -- file or pool
            -> IO ()             -- returns if retort is a success code
throwRetort et addn r erl = withFrozenCallStack $ do
  throwRetort' et addn r erl
  return ()

throwRetort' :: HasCallStack
             => PlasmaExceptionType -- default exception type
             -> Maybe String        -- additional information/loc
             -> Retort
             -> Maybe ErrLocation -- file or pool
             -> IO Retort         -- returns if retort is a success code
throwRetort' et addn r erl
  | isSuccess r = return r
  | otherwise   = withFrozenCallStack $ do
      let eno = c_retort_to_errno (unRetort r)
      if eno > 0
        then throwErrnoHelper addn (Errno eno) erl
        else throwRetortHelper et addn r erl

throwRetortHelper :: HasCallStack
                  => PlasmaExceptionType -- default exception type
                  -> Maybe String        -- additional information/loc
                  -> Retort
                  -> Maybe ErrLocation   -- file or pool
                  -> IO a
throwRetortHelper et addn r erl = do
  pe <- retortToPlasmaException et addn r erl
  throwIO $ pe { peCallstack = Just callStack }

throwErrnoHelper :: HasCallStack
                 => Maybe String        -- additional information/loc
                 -> Errno
                 -> Maybe ErrLocation
                 -> IO a
throwErrnoHelper addn eno erl = do
  let loc   = case addn of
                Just loc' -> loc'
                Nothing   -> locFromStack $ getCallStack callStack
      fname = case erl of
                Nothing                     -> Nothing
                Just (ErrLocation DsNone _) -> Nothing
                Just erl'                -> Just $ displayErrLocation erl'
      ioe   = errnoToIOError loc eno Nothing fname
  throwIO ioe

isInternal :: String -> Bool
isInternal "throwRetort"  = True
isInternal "throwRetort'" = True
isInternal _              = False

locFromStack :: [([Char], SrcLoc)] -> String
locFromStack [] = "plasma"
locFromStack [(name, sloc)]
  | isInternal name = prettySrcLoc sloc
  | otherwise       = name
locFromStack ((name, _):rest)
  | isInternal name = locFromStack rest
  | otherwise       = name
