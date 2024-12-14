{-|
Module      : System.Plasma.Pool.Internal.PoolContext
Description : Context used when creating a new pool hose
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.PoolContext
  ( Context(..)
  , newContext
  , getContextOptions
  ) where

import Control.DeepSeq
import Control.Exception
-- import Control.Monad
-- import qualified Data.ByteString          as B
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.String
import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
-- import Data.Word
-- import Foreign.C.String
-- import Foreign.C.Types
import Foreign.ForeignPtr
-- import Foreign.Marshal.Alloc
import Foreign.Ptr
-- import Foreign.Storable
import GHC.Stack
import System.IO.Unsafe

import Data.Slaw
import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Misc
import System.Loam.Internal.Marshal
import System.Loam.Retorts
import System.Loam.Retorts.Constants

kContext :: IsString a => a
kContext = "Context"

-- | Holds “context” information that might be needed for pool
-- operations, such as credentials.
data Context = Context
  { ctxName :: !T.Text
  , ctxPtr  :: !(ForeignPtr FgnCtx)
  } deriving (Eq, Ord)

instance NFData Context where
  rnf x = ctxName x `deepseq` (ctxPtr x `seq` ())

instance Hashable Context where
  hash                ctx = hashInt        $ fPtrToIntegral (ctxPtr ctx)
  salt `hashWithSalt` ctx = salt `hash2xInt` fPtrToIntegral (ctxPtr ctx)

instance Show Context where
  show ctx = fmtForeignObj kContext (ctxName ctx) [] (ctxPtr ctx)

instance Default Context where
  def = emptyCtx

instance Nameable Context where
  typeName _ = "Context"

foreign import capi safe "ze-hs-ctx.h ze_hs_new_context"
    c_new_context :: C.ConstPtr FgnSlaw -> Ptr Int64 -> IO (Ptr FgnCtx)

foreign import capi unsafe "ze-hs-ctx.h &ze_hs_free_context"
    c_free_context :: FunPtr (Ptr FgnCtx -> IO ())

foreign import capi safe "ze-hs-ctx.h ze_hs_ctx_get_options"
    c_ctx_get_options :: Ptr FgnCtx -> Ptr SlawLen -> IO (Ptr FgnSlaw)

{-# NOINLINE emptyCtx #-}
emptyCtx :: Context
emptyCtx = unsafePerformIO $ newContext "(default context)" emptyMap
  where emptyMap = SlawMap []

-- | Creates a new 'Context'.
newContext
  :: (HasCallStack, ToSlaw a)
  -- | Name of this Context (only used in 'Show' instance).
  => T.Text
  -- | Context options (usually a 'System.Plasma.Pool.ContextOptions').
  -> a
  -> IO Context
newContext name0 opts = do
  initialize
  let cs   = callStack
      addn = Just "newContext"
  name <- nonEmptyName kContext name0 cs
  ptr  <- withSlaw (š opts) $ \slawPtr -> do
    withReturnedRetortCS EtPools addn Nothing cs $ \tortPtr -> do
      c_new_context slawPtr tortPtr
  fptr   <- newForeignPtr c_free_context ptr
  return $ Context { ctxName = name
                   , ctxPtr  = fptr
                   }

-- FIXME: this is kinda ugly...
noMem :: PlasmaException
noMem = unsafePerformIO $ do
  let addn = Just "getContextOptions"
  retortToPlasmaException EtPools addn OB_NO_MEM Nothing

-- | Returns the options that were used when creating the 'Context'.
getContextOptions
  :: (HasCallStack, FromSlaw a)
  => Context
  -> IO a
getContextOptions ctx = do
  withForeignPtr (ctxPtr ctx) $ \ptr -> do
    mSlaw <- withReturnedSlaw def $ c_ctx_get_options ptr
    case fmap ŝee mSlaw ?> Left noMem of
      Left  exc -> throwIO $ exc { peCallstack = Just callStack }
      Right x   -> return x
