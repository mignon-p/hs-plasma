{-|
Module      : System.Plasma.Pool.Internal.Hoses
Description : Map from raw pool_hose pointer to Haskell Hose
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.Hoses
  ( HosesRef
  , Hoses        -- opaque
  , ErrInfo(..)
  , newHoses
  , addToHoses
  , removeFromHoses
  , clearHoses
  , findInHoses
  , listHoses
  ) where

import Control.Exception
import Data.Int
import qualified Data.IntMap.Strict       as IM
import Data.IORef
import Data.List
import qualified Data.Text                as T
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Stack
import Numeric.Natural

import Data.Slaw
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Plasma.Pool.Internal.PoolHose
import System.Plasma.Pool.Internal.PoolName

foreign import capi unsafe "ze-hs-hose.h ze_hs_get_hose"
    c_get_hose :: Ptr FgnHose -> Ptr Int64 -> IO (Ptr FgnRawHose)

type HoseMap = IM.IntMap (Natural, Hose)

data Hoses = Hoses
  { hHoses   :: !HoseMap
  , hCounter :: !Natural
  }

type HosesRef = IORef Hoses

data ErrInfo = ErrInfo
  { errFunc  :: String
  , errStack :: CallStack
  , errGang  :: T.Text
  }

newHoses :: IO HosesRef
newHoses = newIORef $ Hoses IM.empty 0

addToHoses :: ErrInfo -> HosesRef -> Hose -> IO ()
addToHoses ei ref hose = do
  key    <- hoseToRawHoseAddr ei hose
  hoses  <- readIORef ref
  let im   = hHoses   hoses
      ctr  = hCounter hoses
      im'  = IM.insert key (ctr, hose) im
      ctr' = ctr + 1
      msg  = "Hose was already a member"
  hoses' <- if key `IM.member` im
            then throwIO $ eiToPE msg (Just hose) ei
            else return $ Hoses { hHoses = im', hCounter = ctr' }
  evaluate hoses'
  writeIORef ref hoses'

removeFromHoses :: ErrInfo -> HosesRef -> Hose -> IO ()
removeFromHoses ei ref hose = do
  key    <- hoseToRawHoseAddr ei hose
  hoses  <- readIORef ref
  let im  = hHoses hoses
      msg = "Could not find hose"
  hoses' <- if key `IM.member` im
            then return $ hoses { hHoses = key `IM.delete` im }
            else throwIO $ eiToPE msg (Just hose) ei
  evaluate hoses'
  writeIORef ref hoses'

clearHoses :: HosesRef -> IO ()
clearHoses ref = do
  hoses <- readIORef ref
  let hoses' = hoses { hHoses = IM.empty }
  evaluate hoses'
  writeIORef ref hoses'

findInHoses :: ErrInfo -> HosesRef -> Ptr FgnRawHose -> IO Hose
findInHoses ei ref ptr = do
  let (IntPtr key) = ptrToIntPtr ptr
      msg = "Could not find Hose for pool_hose " ++ fmtPtr ptr
  hoses <- readIORef ref
  case key `IM.lookup` hHoses hoses of
    Just (_, hose) -> return hose
    Nothing        -> throwIO $ eiToPE msg Nothing ei

listHoses :: HosesRef -> IO [Hose]
listHoses ref = do
  hoses <- readIORef ref
  let pairs = map snd $ IM.toList $ hHoses hoses
  return $ map snd $ sortOn fst pairs

eiToPE :: String -> Maybe Hose -> ErrInfo -> PlasmaException
eiToPE msg mHose ei =
  PlasmaException { peType      = EtPools
                  , peRetort    = Nothing
                  , peMessage   = msg'
                  , peCallstack = Just $ errStack ei
                  , peLocation  = fmap erlFromHose mHose
                  }
  where msg'  = concat [ errFunc ei
                       , ": "
                       , "gang "
                       , show (errGang ei)
                       , pname
                       , ": "
                       , msg
                       ]
        pname = case mHose of
                  Just hose -> ", hose " ++ show (hoseName hose)
                  Nothing   -> ""

hoseToRawHoseAddr :: ErrInfo -> Hose -> IO Int
hoseToRawHoseAddr ei hose = withForeignPtr (hosePtr hose) $ \ptr -> do
  let addn = Just $ errFunc ei
      erl  = Just $ erlFromPoolName $ hosePool hose
      cs   = errStack ei
  rawPtr <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    c_get_hose ptr tortPtr
  let (IntPtr key) = ptrToIntPtr rawPtr
  return key
