{-|
Module      : System.Loam.Rand
Description : Functions from ob-rand.h
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Loam.Rand
  ( trulyRandom
  ) where

import Control.Exception
import qualified Data.ByteString          as B
import Data.Default.Class
-- import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
import Data.Int
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Stack

-- import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Retorts

foreign import capi "libLoam/c/ob-rand.h ob_truly_random"
    c_truly_random :: Ptr () -> CSize -> IO Int64

trulyRandom :: HasCallStack => Int -> IO B.ByteString
trulyRandom nBytes
  | nBytes < 0 = do
      let msg = "trulyRandom: nBytes " ++ show nBytes ++ " < 0"
      throwIO $ def { peType      = EtInvalidArgument
                    , peMessage   = msg
                    , peCallstack = Just callStack
                    }
  | nBytes == 0 = return B.empty
  | otherwise = withFrozenCallStack $ do
      allocaBytes nBytes $ \ptr -> do
        tort <- Retort <$> c_truly_random ptr (fromIntegral nBytes)
        throwRetort EtOther (Just "trulyRandom") tort Nothing
        B.packCStringLen (castPtr ptr, nBytes)
