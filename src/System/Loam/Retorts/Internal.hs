{-|
Module      : System.Loam.Retorts.Internal
Description : Internal retort stuff
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

-- TODO: Might want to just merge this into System.Loam.Retorts,
-- if there aren't any functions we don't want to expose to the user.

module System.Loam.Retorts.Internal
  ( RetortInfo(..)
  , isSuccess
  , isFailure
  , getRetortInfo
  , retortToErrno
  , retortToPlasmaException
  , throwRetort
  , throwRetort'
  ) where

import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString          as B
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
import System.IO.Error

import Data.Slaw
import Data.Slaw.Util
import qualified System.Loam.Internal.ConstPtr as C
-- import System.Loam.Retorts.Constants
import System.Loam.Retorts.Internal.Descriptions
import System.Loam.Retorts.Internal.IoeRetorts

foreign import capi "libLoam/c/ob-retorts.h ob_error_string_literal"
    c_error_string_literal :: Int64 -> IO C.ConstCString

foreign import capi unsafe "libLoam/c/ob-retorts.h ob_retort_to_errno"
    c_retort_to_errno :: Int64 -> CInt

-- | Information about a particular 'Retort'.  Can be retrieved
-- with 'getRetortInfo'.
data RetortInfo = RetortInfo
  { riName :: T.Text
    -- ^ Name of retort, such as @POOL_NO_SUCH_POOL@
  , riDesc :: T.Text
    -- ^ Human-readable description of the retort
  , riType :: Maybe PlasmaExceptionType
    -- ^ Optionally, the 'PlasmaExceptionType' that should be
    -- associated with the retort
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
    else T.decodeUtf8Lenient <$> B.packCString (C.unConstPtr cs)

getRetortString :: Retort -> IO T.Text
getRetortString r = do
  txt <- getRetortStringFromC r
  if T.null txt
    then return $ T.pack $ "Retort " ++ show (unRetort r)
    else return txt

-- | Given a 'Retort', returns a 'RetortInfo' that describes the
-- 'Retort'.  For unknown returns, returns a 'RetortInfo' where the
-- 'riName' contains the numeric value of the retort, and 'riDesc'
-- is the empty string, and 'riType' is 'Nothing'.
getRetortInfo :: Retort -> IO RetortInfo
getRetortInfo r =
  case r `M.lookup` retortMap of
    Just ri -> return ri
    Nothing -> mkRetortInfo <$> getRetortString r

mkRetortInfo :: T.Text -> RetortInfo
mkRetortInfo txt = def { riName = txt }

{-# INLINE isSuccess #-}
-- | Returns 'True' if the retort is a success code; in other words,
-- if it is greater than or equal to zero.
isSuccess :: Retort -> Bool
isSuccess (Retort r) = r >= 0

{-# INLINE isFailure #-}
-- | Returns 'True' if the retort is a failure code; in other words,
-- if it is less than zero.
isFailure :: Retort -> Bool
isFailure (Retort r) = r < 0

{-# INLINABLE retortToErrno #-}
-- | Tests whether the 'Retort' encapsulates an @errno@ value from
-- the C library.  If it is, returns the 'Errno'.  If not, returns
-- 'Nothing'.
retortToErrno :: Retort -> Maybe Errno
retortToErrno (Retort r) =
  let eno = c_retort_to_errno r
  in if eno > 0
     then Just (Errno eno)
     else Nothing

-- | Convert a 'Retort' into a 'PlasmaException'.
retortToPlasmaException
  :: PlasmaExceptionType
  -- ^ Default exception type; used if 'riType' is 'Nothing'.
  -- If unsure, just pass in 'EtOther'.
  -> Maybe String
  -- ^ Optionally, additional information about the error, such as
  -- the function it occurred in.
  -> Retort
  -- ^ The 'Retort' to be converted to a 'PlasmaException'.
  -> Maybe ErrLocation
  -- ^ Optionally, the file or pool associated with the error.
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

-- | Given a 'Retort', throws an exception if the retort is a
-- failure code (i. e. negative numerical value).  Returns normally
-- if the retort is a success code.
--
-- If the retort encapsulates an @errno@ from the standard C
-- library, throws an 'IOException'.  For any other failure
-- retort, throws a 'PlasmaException'.
throwRetort
  :: HasCallStack
  => PlasmaExceptionType
  -- ^ Default exception type; used if 'riType' is 'Nothing'.
  -- If unsure, just pass in 'EtOther'.
  -> Maybe String
  -- ^ Optionally, additional information about the error, such as
  -- the function it occurred in.
  -> Retort
  -- ^ The 'Retort' to be converted to a 'PlasmaException'.
  -> Maybe ErrLocation
  -- ^ Optionally, the file or pool associated with the error.
  -> IO ()
  -- ^ Returns @()@ if retort is a success code.
throwRetort et addn r erl = withFrozenCallStack $ do
  throwRetort' et addn r erl
  return ()

-- | Same as 'throwRetort', exception if the retort is a success
-- code, it returns the retort instead of returning @()@.
throwRetort'
  :: HasCallStack
  => PlasmaExceptionType
  -- ^ Default exception type; used if 'riType' is 'Nothing'.
  -- If unsure, just pass in 'EtOther'.
  -> Maybe String
  -- ^ Optionally, additional information about the error, such as
  -- the function it occurred in.
  -> Retort
  -- ^ The 'Retort' to be converted to a 'PlasmaException'.
  -> Maybe ErrLocation
  -- ^ Optionally, the file or pool associated with the error.
  -> IO Retort
  -- ^ Returns the retort if it is a success code.
throwRetort' et addn r erl
  | isSuccess r = return r
  | otherwise   = withFrozenCallStack $ do
      case (retortToIoet r, retortToErrno r) of
        (Just ioet, _) -> throwIoetHelper      addn ioet erl
        (_,  Just eno) -> throwErrnoHelper     addn eno  erl
        _              -> throwRetortHelper et addn r    erl

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

throwIoetHelper :: HasCallStack
                => Maybe String        -- additional information/loc
                -> IOErrorType
                -> Maybe ErrLocation
                -> IO a
throwIoetHelper addn ioet erl = do
  let loc   = case addn of
                Just loc' -> loc'
                Nothing   -> locFromStack $ getCallStack callStack
      fname = case erl of
                Nothing                     -> Nothing
                Just (ErrLocation DsNone _) -> Nothing
                Just erl'                -> Just $ displayErrLocation erl'
      ioe   = mkIOError ioet loc Nothing fname
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
