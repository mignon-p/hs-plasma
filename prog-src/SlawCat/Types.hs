module SlawCat.Types
  ( IoType(..)
  , IoDir(..)
  , InitialPos(..)
  , IoEntity(..)
  , Visibility(..)
  , GlobalOpts(..)
  , InputEntity(..)
  , OutputEntity(..)
  , AnnEntity(..)
  , MyOpts(..)
  , MetaSlaw(..)
  , usageFailure
  , exceptionFailure
  , validationFailure
  , dfltFracDigs
  ) where

import Data.Default.Class
import Data.IORef
import qualified Data.Text                as T
import qualified Data.Text.Lazy.Builder   as R
import System.Exit
import System.IO

import Data.Slaw
import Data.Slaw.IO
-- import Data.Slaw.IO.Yaml
-- import Data.Slaw.Util
import System.Plasma.Pool

import SlawCat.Units

data IoType = IoBinary
            | IoYaml
            | IoSpew
            | IoPool
            | IoFile
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data IoDir = DirInput | DirOutput
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data InitialPos = PosRewind
                | PosToLast
                | PosRunout
                | PosSeekTo !PoolIndex
                deriving (Eq, Ord, Show)

data IoEntity = IoEntity
  { entName  :: String
  , entDir   :: !IoDir
  , entType  :: Maybe IoType
  , entOpts  :: !Slaw
  , entMeta  :: Maybe String
  , entQuiet :: !Bool
  , entPos   :: !InitialPos
  } deriving (Eq, Ord, Show)

instance Default IoEntity where
  def = IoEntity { entName  = "-"
                 , entDir   = DirOutput
                 , entType  = Nothing
                 , entOpts  = SlawMap []
                 , entMeta  = Nothing
                 , entQuiet = False
                 , entPos   = PosRewind
                 }

data Visibility = Brief | Normal | Full
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GlobalOpts = GlobalOpts
  { goptHelp     :: Maybe Visibility
  , goptVersion  :: !Bool
  , goptAwait    :: !Bool
  , goptQuiet    :: !Bool
  , goptCount    :: Maybe Integer
  , goptValidate :: [ValidationFlag]
  , goptFracDigs :: !Int
  , goptTimeout  :: Maybe Duration
  , goptWidth    :: Maybe Int
  } deriving (Eq, Ord, Show)

instance Default GlobalOpts where
  def = GlobalOpts { goptHelp     = Nothing
                   , goptVersion  = False
                   , goptAwait    = False
                   , goptQuiet    = False
                   , goptCount    = Nothing
                   , goptValidate = []
                   , goptFracDigs = dfltFracDigs
                   , goptTimeout  = Nothing
                   , goptWidth    = Nothing
                   }

data InputEntity = InPool     { ieHose   :: Hose            }
                 | InStream   { ieStream :: SlawInputStream }
                 deriving (Show, Eq)

data OutputEntity = OutPool   { oeHose   :: Hose            }
                  | OutFile   { oeHandle :: Handle
                              , oeClose  :: Bool
                              , oeCount  :: IORef Integer
                              , oeFlush  :: Bool
                              }
                  | OutStream { oeHandle :: Handle
                              , oeStream :: SlawOutputStream
                              }
                  deriving (Eq)

-- Can't derive Show because IORef doesn't implement Show.
instance Show OutputEntity where
  show (OutPool   {}) = "OutPool"
  show (OutFile   {}) = "OutFile"
  show (OutStream {}) = "OutStream"

data AnnEntity a = AnnEntity
  { aeEnt  :: IoEntity
  , aeType :: T.Text
  , aeIdx  :: Maybe PoolIndex
  , aeAnn  :: Maybe a
  } deriving (Show, Eq)

data MyOpts = MyOpts
  { moGlobal   :: !GlobalOpts
  , moInputs   :: [AnnEntity InputEntity]
  , moOutputs  :: [AnnEntity OutputEntity]
  , moWarnings :: [R.Builder]
  } deriving (Eq, Show)

data MetaSlaw = MetaSlaw
  { msSlaw      :: Slaw
  , msIndex     :: !PoolIndex
  , msTimestamp :: Maybe PoolTimestamp
  , msSource    :: (String, String)
  } deriving (Eq, Ord, Show)

usageFailure, exceptionFailure, validationFailure :: ExitCode
exceptionFailure  = ExitFailure 1
usageFailure      = ExitFailure 2
validationFailure = ExitFailure 3

dfltFracDigs :: Int
dfltFracDigs = 6
