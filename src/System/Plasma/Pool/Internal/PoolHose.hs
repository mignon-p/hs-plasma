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
  , TimeComparison(..)
  , newHose
  , withdraw
  , getHoseContext
  , cloneHose
  , deposit
  , getInfo
  , advanceOldest
  , changeOptions
  , enableWakeup
  , wakeUp
    -- Reading proteins
  , nthProtein
  , next
  , awaitNext
  , curr
  , prev
  , probeFrwd
  , awaitProbeFrwd
  , probeBack
  , fetch
  , fetch'
    -- Getting position
  , newestIndex
  , oldestIndex
  , currIndex
    -- Setting position
  , rewind
  , toLast
  , runout
  , frwdBy
  , backBy
  , seekTo
  , seekToTime
  , seekByTime
    -- utility
  , erlFromHose
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
-- import qualified Data.ByteString          as B
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import Data.Ord
import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
-- import Data.Word
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
-- import Foreign.Marshal.Error
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
-- import System.IO.Unsafe

import Data.Slaw
import Data.Slaw.Util
import System.Loam.Hash
import qualified System.Loam.Internal.ConstPtr as C
import System.Loam.Internal.FgnTypes
import System.Loam.Internal.Initialize
import System.Loam.Internal.Marshal
import System.Loam.Internal.Misc
import System.Loam.Retorts
import System.Loam.Retorts.Constants
-- import System.Loam.Time
import System.Plasma.Pool.Internal.FetchOp
import System.Plasma.Pool.Internal.PoolContext
import System.Plasma.Pool.Internal.PoolName
import System.Plasma.Pool.Internal.PoolTimeout

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

foreign import capi safe "ze-hs-hose.h ze_hs_fetch"
    c_fetch
      :: Ptr FgnHose        -- zHose
      -> CBool              -- clamp
      -> Ptr Int64          -- ops
      -> CSize              -- nops
      -> Ptr Int64          -- oldest_idx_out
      -> Ptr Int64          -- newest_idx_out
      -> IO ()

foreign import capi safe "ze-hs-hose.h ze_hs_get_info"
    c_get_info
      :: Ptr FgnHose        -- zHose
      -> Int64              -- hops
      -> Ptr Int64          -- tort_out
      -> Ptr SlawLen        -- len_out
      -> IO (Ptr FgnSlaw)

foreign import capi safe "ze-hs-hose.h ze_hs_seek_time_op"
    c_seek_time_op
      :: CChar              -- op         ('t' or 'b')
      -> Ptr FgnHose        -- zHose
      -> PoolTimestamp      -- timestamp
      -> Char               -- timeCmp    (one of: ≈ ≤ ≥)
      -> IO Int64           -- retort

foreign import capi safe "ze-hs-hose.h ze_hs_change_options"
    c_change_options
      :: Ptr FgnHose        -- zHose
      -> C.ConstPtr FgnSlaw -- options
      -> IO Int64           -- retort

foreign import capi safe "ze-hs-hose.h ze_hs_wakeup_op"
    c_wakeup_op
      :: CChar              -- op         ('e' or 'w')
      -> Ptr FgnHose        -- zHose
      -> IO Int64           -- retort

kHose :: IsString a => a
kHose = "Hose"

-- | A Hose is a connection to a pool.  In that sense, it is much
-- like a file handle.  It can only be used from one thread at
-- a time.
data Hose = Hose
  { hoseName :: !T.Text   -- ^ Name specified when hose was created.
  , hosePool :: !PoolName -- ^ The pool the hose is connected to.
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

-- | This is the type returned by most of the functions that read
-- a protein from a pool, such as 'nthProtein', 'awaitNext',
-- 'probeFrwd', etc.
--
-- It contains the actual protein read, along with the index
-- and timestamp of that protein within the pool.
data RetProtein = RetProtein
  { -- | The protein read from the pool.
    rpProtein   ::                Slaw
    -- | The index at which the protein was located in the pool.
  , rpIndex     :: {-# UNPACK #-} !PoolIndex
    -- | The time at which the protein was originally written
    -- to the pool.
  , rpTimestamp :: {-# UNPACK #-} !PoolTimestamp
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Nameable RetProtein where
  typeName _ = "RetProtein"

-- | When seeking by time (i.e., positioning the pool's index pointing to
-- a protein whose timestamp is close to a given timestamp), there
-- are three possible strategies.
data TimeComparison =
    -- | Closest value less or equal to the desired one
    ClosestLower
    -- | Closest value, either above or below
  | Closest
    -- | Closest value greater or equal to the desired one
  | ClosestHigher
  deriving (Eq, Ord, Show, Read, Bounded, Enum,
             Generic, NFData, Hashable)

instance Nameable TimeComparison where
  typeName _ = "TimeComparison"

instance Default TimeComparison where
  def = Closest

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

-- | Closes a 'Hose'.  Any further operations on the hose will fail.
--
-- Although hoses will be withdrawn automatically when they are
-- garbage collected, it is much better to call 'withdraw'
-- explicitly when you are done with a hose.
--
-- You can't withdraw a hose if it is still a member of a
-- 'System.Plasma.Pool.Gang'.  You need to remove it from the
-- gang first, or just call 'System.Plasma.Pool.withdrawAll' if
-- you want to withdraw all the hoses in the gang.
withdraw :: HasCallStack => Hose -> IO ()
withdraw hose = withForeignPtr (hosePtr hose) $ \ptr -> do
  let erl  = erlFromHose hose
      addn = Just "withdraw"
  tort <- c_withdraw ptr
  throwRetortCS_ EtPools addn (Retort tort) (Just erl) callStack

-- | The 'Context' that was specified when the hose was created.
getHoseContext :: Hose -> IO Context
getHoseContext h = withForeignPtr (hosePtr h) $ \hPtr -> do
  c_get_context hPtr >>= deRefStablePtr

-- | Create a new connection exactly like the original.  The index of
-- the new 'Hose' will be set to the same point as the original.
cloneHose
  :: HasCallStack
  => T.Text       -- ^ Name of new Hose.
  -> Hose         -- ^ Hose to clone.
  -> IO Hose
cloneHose name orig = withForeignPtr (hosePtr orig) $ \origPtr -> do
  let loc  = "cloneHose"
      addn = Just loc
      pool = hosePool orig
      erl  = Just $ erlFromPoolName pool
      cs   = callStack
  spCtx <- c_get_context origPtr
  ctx   <- deRefStablePtr spCtx
  newH  <- withReturnedRetortCS EtPools addn erl cs $ \tortPtr -> do
    c_hose_clone origPtr tortPtr
  newHose loc cs name pool ctx newH

-- | Deposit a protein into this pool.  Returns the index and
-- timestamp that were assigned to the protein when it was
-- deposited.
--
-- Only proteins can be deposited in a pool; therefore the argument
-- should be a 'SlawProtein' or a 'Protein'.
deposit
  :: (HasCallStack, ToSlaw a)
  => Hose -- ^ Hose to deposit protein into.
  -> a    -- ^ The protein to deposit.
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
        throwRetortCS_ EtPools addn tort (Just erl) callStack
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
  -> IO Retort
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
rewind h = void $ seekOp callStack "rewind" 'w' h minBound

toLast
  :: HasCallStack
  => Hose
  -> IO ()
toLast h = void $ seekOp callStack "toLast" 'l' h minBound

runout
  :: HasCallStack
  => Hose
  -> IO ()
runout h = void $ seekOp callStack "runout" 'o' h minBound

frwdBy
  :: HasCallStack
  => Hose
  -> PoolIndex -- offset
  -> IO ()
frwdBy h idx = void $ seekOp callStack "frwdBy" 'f' h idx

backBy
  :: HasCallStack
  => Hose
  -> PoolIndex -- offset
  -> IO ()
backBy h idx = void $ seekOp callStack "backBy" 'b' h idx

seekTo
  :: HasCallStack
  => Hose
  -> PoolIndex
  -> IO ()
seekTo h idx = void $ seekOp callStack "seekTo" 's' h idx

-- | Sets the “oldest” index of a pool, essentially erasing any
-- proteins prior to that index.  Returns 'True' if at least one
-- protein was erased.  Returns 'False' if the given index is older
-- than the current oldest index.
--
-- Throws a 'PlasmaException' of 'POOL_NO_SUCH_PROTEIN' if the given
-- index is newer than the newest index.
advanceOldest
  :: HasCallStack
  => Hose
  -> PoolIndex -- ^ lowest index that should be preserved
  -> IO Bool   -- ^ Were any proteins erased?
advanceOldest h idx = do
  tort <- seekOp callStack "advanceOldest" 'a' h idx
  -- At this point, tort should be either OB_OK (one or more
  -- proteins erased) or OB_NOTHING_TO_DO (no proteins erased).
  return $ tort == OB_OK

-- | Throws an exception if a significant error (any 'Retort'
-- other than 'POOL_NO_SUCH_PROTEIN') occurs.
-- If 'POOL_NO_SUCH_PROTEIN' occurs for a particular part of the
-- query, returns 'Nothing' for that part of the query.
fetch
  :: HasCallStack
  => Hose
  -> Bool
  -> [FetchOp]
  -> IO ([Maybe FetchResult], Maybe (PoolIndex, PoolIndex))
fetch h clmp fops = do
  let loc = "fetch"
      cs  = callStack
  ret <- fetchCS loc cs h clmp fops
  handleExc loc cs ret

-- | Instead of throwing exceptions, returns the exception which
-- occurred (as a 'Left') for each part of the query.
fetch'
  :: HasCallStack
  => Hose
  -> Bool
  -> [FetchOp]
  -> IO ( [Either PlasmaException FetchResult]
        , Either PlasmaException (PoolIndex, PoolIndex)
        )
fetch' = fetchCS "fetch'" callStack

fetchCS
  :: String
  -> CallStack
  -> Hose
  -> Bool
  -> [FetchOp]
  -> IO ( [Either PlasmaException FetchResult]
        , Either PlasmaException (PoolIndex, PoolIndex)
        )
fetchCS loc cs h clmp fops = withForeignPtr (hosePtr h) $ \hPtr -> do
  let nops   = length fops
      nElems = nops * fieldsPerFetchRecord
      b      = fromBool clmp
      pool   = hosePool h
  allocaArray nElems $ \opPtr -> do
    fillBytes opPtr 0 $ nElems * sizeOf (0 :: Int64)
    pokeFetchOps fops opPtr
    (_, oldest, newest) <- withRet2 (-1, -1) $ \oldPtr newPtr -> do
      c_fetch hPtr b opPtr (fromIntegral nops) oldPtr newPtr
    rs <- peekFetchResults loc cs pool opPtr nops
    let minIndex = minimumBy (comparing keyForIndex) [oldest, newest]
    indices <- if minIndex < 0
                  then Left <$> makeExc loc cs pool (Retort minIndex)
                  else return $ Right (oldest, newest)
    return (rs, indices)

makeExc
  :: String
  -> CallStack
  -> PoolName
  -> Retort
  -> IO PlasmaException
makeExc loc cs pool tort = do
  let erl = erlFromPoolName pool
  pe <- retortToPlasmaException EtPools (Just loc) tort (Just erl)
  return $ pe { peCallstack = Just cs }

handleExc
  :: String
  -> CallStack
  -> ( [Either PlasmaException FetchResult]
     , Either PlasmaException (PoolIndex, PoolIndex)
     )
  -> IO ([Maybe FetchResult], Maybe (PoolIndex, PoolIndex))
handleExc loc cs (ethResults, ethIdx) = do
  mbyIdx     <-       seriousException loc cs  ethIdx
  mbyResults <- mapM (seriousException loc cs) ethResults
  return (mbyResults, mbyIdx)

seriousException
  :: String
  -> CallStack
  -> Either PlasmaException a
  -> IO (Maybe a)
seriousException _   _  (Right x) = return $ Just x
seriousException loc cs (Left pe)
  | peRetort pe == Just POOL_NO_SUCH_PROTEIN = return Nothing
  | peRetort pe == Nothing                   = throwIO pe
  | otherwise = do
      let tort = peRetort    pe ?> OB_UNKNOWN_ERR
          erl  = peLocation  pe
      throwRetortCS_ EtPools (Just loc) tort erl cs
      return Nothing -- should never reach this

keyForIndex :: PoolIndex -> (Int, PoolIndex)
keyForIndex idx
  | idx == unRetort POOL_NO_SUCH_PROTEIN = (2, idx)
  | idx < 0                              = (1, idx)
  | otherwise                            = (3, idx)

-- | Returns a protein with information about a pool.
-- (Therefore, the most useful return types would be 'Slaw',
-- 'Protein', or 'PoolInfo'.)
--
-- The returned protein should always include an ingest @type@, which
-- is a string naming the pool type, and @terminal@, which is a
-- boolean which is true if this is a terminal pool type like @mmap@,
-- or false if this is a transport pool type like @tcp@.
--
-- If @hops@ is 'Just 0', means return information about this pool
-- hose.  If @hops@ is 'Just 1', means return information about the
-- pool beyond this hose (assuming this hose is a nonterminal type
-- like TCP).  And higher values of @hops@ mean go further down the
-- line, if multiple nonterminal types are chained together.  If
-- @hops@ is 'Nothing', means return information about the terminal
-- pool, no matter how far it is.
getInfo
  :: (HasCallStack, FromSlaw a)
  => Hose        -- ^ Hose to get information about.
  -> Maybe Int   -- ^ The number of hops.
  -> IO a
getInfo h mHops = withForeignPtr (hosePtr h) $ \hPtr -> do
  let cs   = callStack
      addn = Just "getInfo"
      hops = fromIntegral (mHops ?> -1)
      erl  = erlFromHose h
  p <- withReturnedSlaw' erl $ \lenPtr -> do
    withReturnedRetortCS EtPools addn (Just erl) cs $ \tortPtr -> do
      c_get_info hPtr hops tortPtr lenPtr
  case ŝee p of
    Left exc -> throwIO $ exc { peCallstack = Just cs }
    Right x  -> return x

timeCmpChar :: TimeComparison -> Char
timeCmpChar Closest       = '≈'
timeCmpChar ClosestLower  = '≤'
timeCmpChar ClosestHigher = '≥'

seekTimeOp
  :: CallStack
  -> String     -- loc
  -> Char       -- op
  -> Hose
  -> PoolTimestamp
  -> TimeComparison
  -> IO ()
seekTimeOp cs loc op h ts tc = do
  let addn = Just loc
      erl  = erlFromHose h
      c    = toCChar op
  withForeignPtr (hosePtr h) $ \hPtr -> do
    tort <- c_seek_time_op c hPtr ts (timeCmpChar tc)
    throwRetortCS_ EtPools addn (Retort tort) (Just erl) cs

seekToTime
  :: HasCallStack
  => Hose
  -> PoolTimestamp
  -> TimeComparison
  -> IO ()
seekToTime = seekTimeOp callStack "seekToTime" 't'

seekByTime
  :: HasCallStack
  => Hose
  -> PoolTimestamp
  -> TimeComparison
  -> IO ()
seekByTime = seekTimeOp callStack "seekByTime" 'b'

changeOptions
  :: (HasCallStack, ToSlaw a)
  => Hose
  -- | Options to change (usually a
  -- 'System.Plasma.Pool.PoolCreateOptions').
  -> a
  -> IO ()
changeOptions h opts = do
  let addn = Just "changeOptions"
      erl  = erlFromHose h
      cs   = callStack
  withForeignPtr (hosePtr h) $ \hPtr -> do
    withSlaw (š opts) $ \slawPtr -> do
      tort <- c_change_options hPtr slawPtr
      throwRetortCS_ EtPools addn (Retort tort) (Just erl) cs

wakeupOp
  :: CallStack
  -> String     -- loc
  -> Char       -- op
  -> Hose
  -> IO ()
wakeupOp cs loc op h = do
  let addn = Just loc
      erl  = erlFromHose h
      c    = toCChar op
  withForeignPtr (hosePtr h) $ \hPtr -> do
    tort <- c_wakeup_op c hPtr
    throwRetortCS_ EtPools addn (Retort tort) (Just erl) cs

enableWakeup
  :: HasCallStack
  => Hose
  -> IO ()
enableWakeup = wakeupOp callStack "enableWakeup" 'e'

wakeUp
  :: HasCallStack
  => Hose
  -> IO ()
wakeUp = wakeupOp callStack "wakeUp" 'w'
