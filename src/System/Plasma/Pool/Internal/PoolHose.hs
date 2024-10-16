{-|
Module      : System.Plasma.Pool.Internal.PoolHose
Description : Hose type
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module System.Plasma.Pool.Internal.PoolHose
  ( Hose(..)
  , RetProtein(..)
  , PoolTimestamp
  , newHose
  , withdraw
  , getHoseContext
  , cloneHose
  , deposit
    --
  , nthProtein
  , next
  , awaitNext
  , curr
  , prev
  , probeFrwd
  , awaitProbeFrwd
  , probeBack
    --
  , newestIndex
  , oldestIndex
  , currIndex
    --
  , rewind
  , toLast
  , runout
  , frwdBy
  , backBy
  , seekTo
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
import qualified Data.Text.Encoding       as T
-- import Data.Word
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
-- import System.IO.Unsafe

import Data.Slaw
-- import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
-- import System.Loam.Retorts.Constants
import System.Loam.Time
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolTimeout

-- | Number of seconds since the UNIX epoch (January 1, 1970)
type PoolTimestamp = LoamTime

foreign import capi safe "ze-hs-hose.h ze_hs_make_hose"
    c_make_hose :: Ptr FgnRawHose -> StablePtr Context -> C.ConstCString -> Ptr Int64 -> IO (Ptr FgnHose)

foreign import capi unsafe "ze-hs-hose.h &ze_hs_finalize_hose"
    c_finalize_hose :: FunPtr (Ptr FgnHose -> IO ())

foreign import capi safe "ze-hs-hose.h ze_hs_withdraw"
    c_withdraw :: Ptr FgnHose -> IO Int64

foreign import capi unsafe "ze-hs-hose.h ze_hs_get_context"
    c_get_context :: Ptr FgnHose -> IO (StablePtr Context)

foreign import capi safe "ze-hs-hose.h ze_hs_hose_clone"
    c_hose_clone :: Ptr FgnHose -> Ptr Int64 -> IO (Ptr FgnRawHose)

foreign import capi safe "ze-hs-hose.h ze_hs_deposit"
    c_deposit :: Ptr FgnHose -> C.ConstPtr FgnSlaw -> Ptr Int64 -> Ptr Double -> IO Int64

foreign import capi safe "ze-hs-hose.h ze_hs_nth_protein"
    c_nth_protein :: Ptr FgnHose -> Int64 -> Ptr PoolTimestamp -> Ptr Int64 -> Ptr SlawLen -> IO (Ptr FgnSlaw)

foreign import capi safe "ze-hs-hose.h ze_hs_protein_op"
    c_protein_op
      :: CChar              -- op
      -> Ptr FgnHose        -- zHose
      -> C.ConstPtr FgnSlaw -- search
      -> Double             -- timeout
      -> Ptr PoolTimestamp  -- ts_out
      -> Ptr PoolIndex      -- idx_out
      -> Ptr Int64          -- tort_out
      -> Ptr SlawLen        -- len_out
      -> IO (Ptr FgnSlaw)

foreign import capi safe "ze-hs-hose.h ze_hs_get_index"
    c_get_index
      :: CChar              -- op
      -> Ptr FgnHose        -- zHose
      -> Ptr Int64          -- tort_out
      -> IO PoolIndex

foreign import capi safe "ze-hs-hose.h ze_hs_seek_op"
    c_seek_op
      :: CChar              -- op
      -> Ptr FgnHose        -- zHose
      -> PoolIndex          -- idx
      -> IO Int64           -- retort

kHose :: IsString a => a
kHose = "Hose"

data Hose = Hose
  { hoseName :: !T.Text
  , hosePool :: !PoolName
  , hosePtr  :: !(ForeignPtr FgnHose)
  } deriving (Eq, Ord)

instance NFData Hose where
  rnf x = hoseName x `deepseq` hosePool x `deepseq` (hosePtr x `seq` ())

instance Hashable Hose where
  hash                hose = hashInt $ fPtrToIntegral (hosePtr hose)
  salt `hashWithSalt` hose =
    salt `hash2xInt` fPtrToIntegral (hosePtr hose)

instance Show Hose where
  show hose = fmtForeignObj kHose (hoseName hose) [info] (hosePtr hose)
    where
      info = "pool=" ++ show (hosePool hose)

instance Nameable Hose where
  typeName _ = "Hose"

data RetProtein = RetProtein
  { rpProtein   ::                Slaw
  , rpIndex     :: {-# UNPACK #-} !PoolIndex
  , rpTimestamp :: {-# UNPACK #-} !PoolTimestamp
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable RetProtein where
  typeName _ = "RetProtein"

newHose
  :: String         -- ^ name of API function
  -> CallStack
  -> T.Text         -- ^ name of this Hose
  -> PoolName       -- ^ name of pool
  -> Context        -- ^ context used when creating Hose
  -> Ptr FgnRawHose -- ^ actual pointer to the hose
  -> IO Hose
newHose loc cs name0 pool ctx hPtr = do
  initialize
  let erl  = Just $ erlFromPoolName pool
      addn = Just loc
  name    <- nonEmptyName kHose name0 cs
  stabPtr <- newStablePtr ctx
  zHose   <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    C.useAsConstCString (T.encodeUtf8 name) $ \namePtr -> do
      c_make_hose hPtr stabPtr namePtr tortPtr
  fptr    <- newForeignPtr c_finalize_hose zHose
  return $ Hose { hoseName = name
                , hosePool = pool
                , hosePtr  = fptr
                }

erlFromHose :: Hose -> ErrLocation
erlFromHose = erlFromPoolName . hosePool

erlFromHoseIdx :: Hose -> PoolIndex -> ErrLocation
erlFromHoseIdx h idx = erlFromPoolIdx (hosePool h) idx

withdraw :: HasCallStack => Hose -> IO ()
withdraw hose = withForeignPtr (hosePtr hose) $ \ptr -> do
  let erl  = erlFromHose hose
      addn = Just "withdraw"
  tort <- c_withdraw ptr
  throwRetortCS EtPools addn (Retort tort) (Just erl) callStack

getHoseContext :: Hose -> IO Context
getHoseContext h = withForeignPtr (hosePtr h) $ \hPtr -> do
  c_get_context hPtr >>= deRefStablePtr

cloneHose
  :: HasCallStack
  => T.Text       -- ^ name of new Hose
  -> Hose         -- ^ hose to clone
  -> IO Hose
cloneHose name orig = withForeignPtr (hosePtr orig) $ \origPtr -> do
  let loc  = "cloneHose"
      addn = Just loc
      pool = hosePool orig
      erl  = Just $ erlFromPoolName pool
      cs   = callStack
  spCtx <- c_get_context origPtr
  ctx   <- deRefStablePtr spCtx
  new   <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    c_hose_clone origPtr tortPtr
  newHose loc cs name pool ctx new

deposit
  :: (HasCallStack, ToSlaw a)
  => Hose
  -> a
  -> IO (PoolIndex, PoolTimestamp)
deposit h opts = withForeignPtr (hosePtr h) $ \hPtr -> do
  let addn = Just "deposit"
      erl  = erlFromHose h
  withSlaw (š opts) $ \slawPtr -> do
    alloca $ \idxPtr -> do
      alloca $ \timePtr -> do
        poke idxPtr  minBound
        poke timePtr (-1)
        tort <- Retort <$> c_deposit hPtr slawPtr idxPtr timePtr
        throwRetortCS EtPools addn tort (Just erl) callStack
        idx  <- peek idxPtr
        ts   <- peek timePtr
        return (idx, ts)

nthProtein
  :: HasCallStack
  => Hose
  -> PoolIndex
  -> IO RetProtein
nthProtein h idx = withForeignPtr (hosePtr h) $ \hPtr -> do
  let addn = Just "deposit"
      erl  = erlFromHoseIdx h idx
      cs   = callStack
  (p, ts) <- withRet1 (-1) $ \tsPtr -> do
    withReturnedSlaw' erl $ \lenPtr -> do
      withReturnedRetortCS EtPools addn (Just erl) cs $ \tortPtr -> do
        c_nth_protein hPtr idx tsPtr tortPtr lenPtr
  return $ RetProtein { rpProtein   = p
                      , rpIndex     = idx
                      , rpTimestamp = ts
                      }

proteinOp
  :: CallStack
  -> String      -- loc
  -> Char        -- op
  -> Hose
  -> Maybe Slaw  -- search
  -> PoolTimeout -- timeout
  -> IO RetProtein
proteinOp cs loc op h srch timeout = do
  let addn = Just loc
      erl  = erlFromHose h
      c    = toCChar op
  tmout <- case encodePoolTimeout timeout of
    Right t  -> return t
    Left msg ->
      throwIO $ def { peType      = EtPools
                    , peMessage   = loc ++ ": " ++ msg
                    , peCallstack = Just cs
                    , peLocation  = Just erl
                    }
  withForeignPtr (hosePtr h) $ \hPtr -> do
    withMaybeSlaw srch $ \srchPtr -> do
      ((p, idx), ts) <- withRet1 (-1) $ \tsPtr -> do
        withReturnedSlawIdx erl $ \idxPtr lenPtr -> do
          withReturnedRetortCS EtPools addn (Just erl) cs $ \tortPtr -> do
            c_protein_op c hPtr srchPtr tmout tsPtr idxPtr tortPtr lenPtr
      return $ RetProtein p idx ts

next
  :: HasCallStack
  => Hose
  -> IO RetProtein
next h = proteinOp callStack "next" 'n' h Nothing def

awaitNext
  :: HasCallStack
  => Hose
  -> PoolTimeout -- timeout
  -> IO RetProtein
awaitNext h = proteinOp callStack "awaitNext" 'a' h Nothing

curr
  :: HasCallStack
  => Hose
  -> IO RetProtein
curr h = proteinOp callStack "curr" 'c' h Nothing def

prev
  :: HasCallStack
  => Hose
  -> IO RetProtein
prev h = proteinOp callStack "prev" 'p' h Nothing def

probeFrwd
  :: HasCallStack
  => Hose
  -> Slaw -- search
  -> IO RetProtein
probeFrwd h srch = proteinOp callStack "probeFrwd" 'f' h (Just srch) def

awaitProbeFrwd
  :: HasCallStack
  => Hose
  -> Slaw        -- search
  -> PoolTimeout -- timeout
  -> IO RetProtein
awaitProbeFrwd h srch =
  proteinOp callStack "awaitProbeFrwd" 'w' h (Just srch)

probeBack
  :: HasCallStack
  => Hose
  -> Slaw -- search
  -> IO RetProtein
probeBack h srch = proteinOp callStack "probeBack" 'b' h (Just srch) def

indexOp
  :: CallStack
  -> String     -- loc
  -> Char       -- op
  -> Hose
  -> IO PoolIndex
indexOp cs loc op h = do
  let addn = Just loc
      erl  = erlFromHose h
      c    = toCChar op
  withForeignPtr (hosePtr h) $ \hPtr -> do
    withReturnedRetortCS EtPools addn (Just erl) cs $ \tortPtr -> do
      c_get_index c hPtr tortPtr

newestIndex
  :: HasCallStack
  => Hose
  -> IO PoolIndex
newestIndex = indexOp callStack "newestIndex" 'n'

oldestIndex
  :: HasCallStack
  => Hose
  -> IO PoolIndex
oldestIndex = indexOp callStack "oldestIndex" 'o'

currIndex
  :: HasCallStack
  => Hose
  -> IO PoolIndex
currIndex = indexOp callStack "currIndex" 'i'

seekOp
  :: CallStack
  -> String     -- loc
  -> Char       -- op
  -> Hose
  -> PoolIndex
  -> IO ()
seekOp cs loc op h idx = do
  let addn = Just loc
      erl  = erlFromHose h
      c    = toCChar op
  withForeignPtr (hosePtr h) $ \hPtr -> do
    tort <- c_seek_op c hPtr idx
    throwRetortCS EtPools addn (Retort tort) (Just erl) cs

rewind
  :: HasCallStack
  => Hose
  -> IO ()
rewind h = seekOp callStack "rewind" 'w' h minBound

toLast
  :: HasCallStack
  => Hose
  -> IO ()
toLast h = seekOp callStack "toLast" 'l' h minBound

runout
  :: HasCallStack
  => Hose
  -> IO ()
runout h = seekOp callStack "runout" 'o' h minBound

frwdBy
  :: HasCallStack
  => Hose
  -> PoolIndex -- offset
  -> IO ()
frwdBy = seekOp callStack "frwdBy" 'f'

backBy
  :: HasCallStack
  => Hose
  -> PoolIndex -- offset
  -> IO ()
backBy = seekOp callStack "backBy" 'b'

seekTo
  :: HasCallStack
  => Hose
  -> PoolIndex
  -> IO ()
seekTo = seekOp callStack "seekTo" 's'
