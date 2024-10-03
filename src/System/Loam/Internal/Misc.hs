{-|
Module      : System.Loam.Internal.Misc
Description : Miscellaneous utility functions
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Internal.Misc
  ( natToDashedHex
  , ptrToIntegral
  , fPtrToIntegral
  , fmtPtr
  , fmtForeignPtr
  , fmtForeignObj
    --
  , conciseSrcLoc
  , nonEmptyName
  ) where

import Data.Bits
import Data.IORef
import Data.List
import qualified Data.Text                as T
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import GHC.Stack
import Numeric.Natural
import System.FilePath (takeFileName)
import System.IO.Unsafe
import Text.Printf

import Data.Slaw.Internal

natToDashedHex :: Natural -> String
natToDashedHex =
  intercalate "-" . map (printf "%04x") . reverse . nat2w16s

nat2w16s :: Natural -> [Word16]
nat2w16s n = fromIntegral n : nxt
  where n'  = n `shiftR` 16
        nxt = if n' > 0
              then nat2w16s n'
              else []

{-# INLINE ptrToIntegral #-}
ptrToIntegral :: Integral a => Ptr b -> a
ptrToIntegral = fromIntegral . ptrToWordPtr

{-# INLINE fPtrToIntegral #-}
fPtrToIntegral :: Integral a => ForeignPtr b -> a
fPtrToIntegral = fromIntegral . ptrToWordPtr . unsafeForeignPtrToPtr

fmtPtr :: Ptr a -> String
fmtPtr = natToDashedHex . ptrToIntegral

fmtForeignPtr :: ForeignPtr a -> String
fmtForeignPtr = fmtPtr . unsafeForeignPtrToPtr

fmtForeignObj
  :: String       -- type name
  -> T.Text       -- object name
  -> [String]     -- other information (optional)
  -> ForeignPtr a -- address of foreign object
  -> String
fmtForeignObj typeName objName info fPtr = intercalate " " parts
  where
    parts    = [typePart, namePart] ++ info ++ [ptrPart]
    typePart = "{" ++ typeName ++ ":"
    namePart = showEscapedStr $ T.unpack objName
    ptrPart  = "<" ++ fmtForeignPtr fPtr ++ ">}"

conciseSrcLoc :: CallStack -> Maybe String
conciseSrcLoc cs =
  case getCallStack cs of
    ((_, srcLoc):_) ->
      let fn = takeFileName $ srcLocFile srcLoc
          ln = srcLocStartLine srcLoc
      in Just $ fn ++ ":" ++ show ln
    _               -> Nothing

{-# NOINLINE counter #-}
counter :: IORef Integer
counter = unsafePerformIO $ newIORef 1

incCounter :: Integer -> (Integer, Integer)
incCounter !x = (x + 1, x)

nonEmptyName :: T.Text -> T.Text -> CallStack -> IO T.Text
nonEmptyName typeName objName cs
  | not (T.null objName) = return objName
  | otherwise = do
      let loc = maybe "" (" @ " ++) $ conciseSrcLoc cs
      n <- atomicModifyIORef' counter incCounter
      return $ mconcat [ "<"
                       , typeName
                       , " "
                       , T.pack (show n)
                       , T.pack loc
                       , ">"
                       ]
