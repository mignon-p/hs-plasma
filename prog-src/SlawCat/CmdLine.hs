{-# LANGUAGE ScopedTypeVariables        #-}

module SlawCat.CmdLine
  ( ScOpts(..)
  , scUsage
  , scGetOpts
  , scCheckDups
  , valNames
  ) where

import Data.Bifunctor
import Data.Char
import Data.Containers.ListUtils
import Data.Default.Class
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import Data.Slaw
import Data.Slaw.IO
import Data.Slaw.IO.Yaml
import Data.Slaw.Util
import System.Console.GetOpt
import Text.Read

import SlawCat.CmdLine.Pretty
import SlawCat.Types
import SlawCat.Units

data ScOpt = OptError    String
           -- global options
           | OptHelp     !Visibility
           | OptVersion
           | OptAwait
           | OptQuiet
           | OptCount    !Integer
           | OptValidate [ValidationFlag]
           | OptFracDigs !Int
           | OptTimeout  !Duration
           | OptWidth    !Int
           | OptDescrips [Slaw]
           | OptIngests  [Slaw]
           -- per-entity options
           | OptConfig   !IoType !Slaw
           | OptCfgOnly          !Slaw
           | OptEnt      !IoDir  String
           | OptEntMeta  String
           | OptEntQuiet
           | OptEntPos   !InitialPos
           deriving (Eq, Ord, Show)

data ScOpts = ScOpts
  { optGlobal   :: !GlobalOpts
  , optEnts     :: [IoEntity]
  , optAccum    :: !IoEntity
  , optErrors   :: [String]
  } deriving (Eq, Ord, Show)

instance Default ScOpts where
  def = ScOpts { optGlobal = def
               , optEnts   = []
               , optAccum  = def
               , optErrors = []
               }

updOpts :: ScOpts -> ScOpt -> ScOpts
-- global options
updOpts !o (OptHelp     v) = updGlob o $ \g -> g { goptHelp    = Just v }
updOpts !o OptVersion      = updGlob o $ \g -> g { goptVersion = True   }
updOpts !o OptAwait        = updGlob o $ \g -> g { goptAwait   = True   }
updOpts !o OptQuiet        = updGlob o $ \g -> g { goptQuiet   = True   }
updOpts !o (OptCount    c) = updGlob o $ \g -> g { goptCount   = Just c }
updOpts !o (OptValidate v) = updGlob o (updVal v)
updOpts !o (OptFracDigs d) = updGlob o $ \g -> g { goptFracDigs = d     }
updOpts !o (OptTimeout  t) = updGlob o $ \g -> g { goptAwait   = True
                                                 , goptTimeout = Just t
                                                 }
updOpts !o (OptWidth    w) = updGlob o $ \g -> g { goptWidth   = Just w }
updOpts !o (OptDescrips d) = updGlob o (updDescrips d)
updOpts !o (OptIngests  i) = updGlob o (updIngests  i)
-- per-entity options
updOpts !o (OptConfig t s) = updCfg  o (Just t) s
updOpts !o (OptCfgOnly  s) = updCfg  o Nothing  s
updOpts !o (OptEntMeta  k) = updEnt  o $ \e -> e { entMeta  = Just k }
updOpts !o OptEntQuiet     = updEnt  o $ \e -> e { entQuiet = True   }
updOpts !o (OptEntPos   p) = updEnt  o $ \e -> e { entPos   = p      }
updOpts !o (OptEnt    d n) = doEnt   o d n
-- errors
updOpts !o (OptError  msg) = o { optErrors = msg : optErrors o }

updGlob :: ScOpts -> (GlobalOpts -> GlobalOpts) -> ScOpts
updGlob o f = o { optGlobal = f (optGlobal o) }

updEnt :: ScOpts -> (IoEntity -> IoEntity) -> ScOpts
updEnt o f = o { optAccum = f (optAccum o) }

updVal :: [ValidationFlag] -> GlobalOpts -> GlobalOpts
updVal v g = g { goptValidate = goptValidate g `appendUniq` v }

updDescrips :: [Slaw] -> GlobalOpts -> GlobalOpts
updDescrips d g = g { goptDescrips = goptDescrips g ++ d }

updIngests :: [Slaw] -> GlobalOpts -> GlobalOpts
updIngests i g = g { goptIngests = goptIngests g `appendUniq` i }

appendUniq :: Ord a => [a] -> [a] -> [a]
appendUniq v1 v2 = nubOrd $ v1 ++ v2

updCfg :: ScOpts -> Maybe IoType -> Slaw -> ScOpts
updCfg o t s = updEnt o (uc1 t s)

uc1 :: Maybe IoType -> Slaw -> IoEntity -> IoEntity
uc1 !t !s !e = e { entType = entType e `combType`  t
                 , entOpts = entOpts e `prefRight` s
                 }

combType :: Maybe IoType -> Maybe IoType -> Maybe IoType
combType (Just t1) (Just t2) = Just (t1 `ct1` t2)
combType Nothing   t2        = t2
combType t1        Nothing   = t1

ct1 :: IoType -> IoType -> IoType
ct1 t1 IoFile
  | t1 `elem` [IoBinary, IoYaml, IoSpew] = t1
ct1 _  t2 = t2

doEnt :: ScOpts -> IoDir -> String -> ScOpts
doEnt o d n = o { optEnts  = ent : optEnts o
                , optAccum = def
                }
  where ent = (optAccum o) { entName = n
                           , entDir  = d
                           }

finishOpts :: ScOpts -> ScOpts
finishOpts o =
  o { optEnts   = reverse (optEnts   o)
    , optErrors = reverse (optErrors o)
    }

makeOpts :: [ScOpt] -> ScOpts
makeOpts = mo1 def

mo1 :: ScOpts -> [ScOpt] -> ScOpts
mo1 !o []         = finishOpts o
mo1 !o (opt:rest) = mo1 (updOpts o opt) rest

--

type OptD = OptDescr ScOpt
type ArgD = ArgDescr ScOpt
type UsgD = UsageDescrV Visibility ScOpt

data OptR = AX { shrt :: String
               , long :: String
               , optX :: ScOpt
               , desc :: String
               }
          | AN { shrt :: String
               , long :: String
               , optB :: Bool -> ScOpt
               , desc :: String
               }
          | AR { shrt :: String
               , long :: String
               , argD :: String
               , optS :: String -> ScOpt
               , desc :: String
               }
          | AD { shrt :: String
               , long :: String
               , argD :: String
               , dflt :: String
               , optS :: String -> ScOpt
               , desc :: String
               }
          | LN T.Text
          | VS Visibility

mkD :: OptR -> ArgD -> OptD
mkD r d = Option (shrt r) [long r] d (desc r)

expandR :: OptR -> [UsgD]
expandR (VS vis)  = [UsageVisibility vis]
expandR (LN ln)   = [UsageLine ln]
expandR r@(AN {}) = map UsageOpt [d1, d2]
  where d1     = mkD r $ NoArg $ optB r True
        d2     = Option "" [noLong] (NoArg $ optB r False) noDesc
        noLong = "no-"     ++ long r
        noDesc = "do not " ++ desc r
expandR r         = [UsageOpt $ mkD r $ mkA r]

mkA :: OptR -> ArgD
mkA r@(AX {}) = NoArg  (optX r)
mkA r@(AR {}) = ReqArg (annErr r . optS r)                      (argD r)
mkA r@(AD {}) = OptArg (annErr r . optS r . fromMaybe (dflt r)) (argD r)
mkA _         = error "internal error"

annErr :: OptR -> ScOpt -> ScOpt
annErr r (OptError msg) =
  let longName = long r
      name     = if null longName
                 then '-' : shrt r
                 else "--" ++ longName
  in OptError $ "in option '" ++ name ++ "', " ++ msg
annErr _ opt = opt

nx :: OptR
nx = AX { shrt = ""
        , long = ""
        , optX = OptError "internal error"
        , desc = ""
        }

nn :: OptR
nn = AN { shrt = ""
        , long = ""
        , optB = const (OptError "internal error")
        , desc = ""
        }

nr :: OptR
nr = AR { shrt = ""
        , long = ""
        , argD = "ARG"
        , optS = const (OptError "internal error")
        , desc = ""
        }

descrs :: [UsgD]
descrs = concatMap expandR
  [ VS Brief
  , LN "Usage: slawcat \t[global-options] [[opts] INFILE] \
       \... [[opts] -o OUTFILE] ..."
  , LN ""
  , LN "Help options:"
  , AX "h" "help"    (OptHelp Normal) "print short help message and exit"
  , AX "H" "full-help" (OptHelp Full) "print full help message and exit"
  , AX "V" "version"   OptVersion     "print version number and exit"
  , VS Full
  , AR { shrt = "w", long = "width", argD = "INTEGER"
       , optS = intArg OptWidth
       , desc = "number of columns to wrap help message to"
       }
  , VS Normal
  , LN ""
  , LN "Global options:"
  , AX "q" "quiet" OptQuiet "don't print stuff to stderr, unless error"
  , AX "a" "await" OptAwait "keep waiting for more proteins to arrive"
  , AR { shrt = "t", long = "timeout", argD = "DURATION"
       , optS = allowErr . second OptTimeout . parseDuration
       , desc = concat [ "total time to await (number followed by one "
                       , "of these units: "
                       , durUnitsStr
                       , ")"
                       ]
       }
  , AR { shrt = "c", long = "count", argD = "INTEGER"
       , optS = integerArg OptCount
       , desc = "maximum number of slawx to process"
       }
  , AR { shrt = "v", long = "validate", argD = "LETTERS"
       , optS = allowErr . parseValidate
       , desc = valHelp "check that slawx meet criteria"
       }
  , VS Full
  , AR { shrt = "d", long = "frac-digs", argD = "INTEGER"
       , optS = intArg OptFracDigs
       , desc = concat [ "number of fractional digits of seconds to "
                       , "display in timestamps (default="
                       , show dfltFracDigs
                       , ")"
                       ]
       }
  , AR { shrt = "D", long = "descrips", argD = "WORDS"
       , optS = OptDescrips . wordsArg
       , desc = concat [ "only process proteins that match the "
                       , "specified descrips "
                       , "(WORDS is split on whitespace)"
                       ]
       }
  , AR { shrt = "I", long = "ingests", argD = "WORDS"
       , optS = OptIngests . wordsArg
       , desc = concat [ "only process proteins that have the "
                       , "specified ingests "
                       , "(WORDS is split on whitespace)"
                       ]
       }
  , VS Normal
  , LN ""
  , LN "Per-file/pool options:"
  , AR { shrt = "o", long = "output", argD = "NAME"
       , optS = OptEnt DirOutput
       , desc = "output to a pool or file"
       }
  , AX "b" "binary" (cfgBin def) "write to binary slaw file"
  , VS Full
  , nx { long = "big-endian"
       , optX = cfgBin $ def { wboByteOrder = Just BoBigEndian }
       , desc = "big endian binary slaw file"
       }
  , nx { long = "little-endian"
       , optX = cfgBin $ def { wboByteOrder = Just BoLittleEndian }
       , desc = "little endian binary slaw file"
       }
  , VS Normal
  , AX { shrt = "y", long = "yaml", optX = cfgYaml def
       , desc = "write to YAML slaw file"
       }
  , VS Full
  , nn { long = "tag-numbers"
       , optB = \b -> cfgYaml (def { wyoTagNumbers = Just b })
       , desc = "tag numbers with type information (YAML)"
       }
  , nn { long = "directives"
       , optB = \b -> cfgYaml (def { wyoDirectives = Just b })
       , desc = "emit %YAML and %TAG directives (YAML)"
       }
  , nn { long = "ordered-maps"
       , optB = \b -> cfgYaml (def { wyoOrderedMaps = Just b })
       , desc = "use !!omap for maps (YAML)"
       }
  , nn { long = "comment"
       , optB = \b -> cfgYaml (def { wyoComment = Just b })
       , desc = "add comment at beginning of file (YAML)"
       }
  , nr { long = "max-array", argD = "INTEGER"
       , optS = intArg maxArray
       , desc = "truncate arrays longer than this (YAML)"
       }
  , VS Normal
  , AX { shrt = "s", long = "spew"
       , optX = cfgOther IoSpew
       , desc = "write to \"spew overview\" file"
       }
  , AX { shrt = "p", long = "pool"
       , optX = cfgOther IoPool
       , desc = "read from/write to pool"
       }
  , AX { shrt = "L", long = "to-last"
       , optX = OptEntPos PosToLast
       , desc = "seek to the last protein in input pool"
       }
  , AX { shrt = "R", long = "runout"
       , optX = OptEntPos PosRunout
       , desc = "seek to one past the last protein in input pool"
       }
  , AR { shrt = "i", long = "seek-to", argD = "INTEGER"
       , optS = intArg (OptEntPos . PosSeekTo)
       , desc = "seek to specified index in input pool"
       }
  , AX { shrt = "f", long = "file"
       , optX = cfgOther IoFile
       , desc = "read from file"
       }
  , VS Full
  , AN { shrt = "F", long = "flush"
       , optB = optFlush
       , desc = "flush output file after each slaw"
       }
  , AD { shrt = "m", long = "add-metadata", argD = "KEY", dflt = "origin"
       , optS = OptEntMeta
       , desc = "add metadata to output proteins, in KEY ingest"
       }
  , AX { shrt = "Q", long = "no-metadata-comment"
       , optX = OptEntQuiet
       , desc = "do not write comments with pool name/index/time"
       }
  , VS Normal
  ]

cfgBin :: WriteBinaryOptions -> ScOpt
cfgBin wbo = OptConfig IoBinary (toSlaw wbo)

cfgYaml :: WriteYamlOptions -> ScOpt
cfgYaml wyo = OptConfig IoYaml (toSlaw wyo)

cfgOther :: IoType -> ScOpt
cfgOther t = OptConfig t emptyMap

allowErr :: Either String ScOpt -> ScOpt
allowErr (Left msg) = OptError msg
allowErr (Right x)  = x

parseInteger :: String -> Either String Integer
parseInteger str =
  case readMaybe str of
    Just n  -> Right n
    Nothing -> Left $ "could not parse " ++ show str ++ " as integer"

integerArg :: (Integer -> ScOpt) -> String -> ScOpt
integerArg f = allowErr . second f . parseInteger

intArg
  :: (Bounded a, Integral a, Nameable a)
  => (a -> ScOpt)
  -> String
  -> ScOpt
intArg f = allowErr . second f . checkCastInt . parseInteger

checkCastInt
  :: forall a. (Bounded a, Integral a, Nameable a)
  => Either String Integer
  -> Either String a
checkCastInt (Left  msg) = Left msg
checkCastInt (Right n  ) =
  let mn = minBound :: a
      mx = maxBound :: a
  in if n <= toInteger mx && n >= toInteger mn
     then Right $ fromInteger n
     else Left $ concat [ show n
                        , " is out of bounds for "
                        , typeName mx
                        ]

parseValidate :: String -> Either String ScOpt
parseValidate cs = do
  flags <- mapM (pv1 cs) cs
  return $ OptValidate flags

pv1 :: String -> Char -> Either String ValidationFlag
pv1 cs c =
  case toLower c `lookup` valOpts of
    Just (v, _) -> Right v
    _           -> Left $ concat [ "unrecognized validation character "
                                 , show c
                                 , " in "
                                 , show cs
                                 ]

wordsArg :: String -> [Slaw]
wordsArg = map toSlaw . words

maxArray :: Int64 -> ScOpt
maxArray n = cfgYaml $ def { wyoMaxArrayElements = Just (toInteger n) }

emptyMap :: Slaw
emptyMap = SlawMap []

optFlush :: Bool -> ScOpt
optFlush b = OptCfgOnly $ SlawMap [("auto-flush", f b)]
  where f False = "never"
        f True  = "always"

valOpts :: [(Char, (ValidationFlag, String))]
valOpts =
  [ ('c', (VfCSlaw,  "supported by C-plasma"))
  , ('s', (VfUtf8,   "strings are valid UTF-8"))
  , ('p', (VfDesIng, "proteins have list for descrips, map for ingests"))
  , ('m', (VfUniqKeys, "maps do not contain duplicate keys"))
  ]

valNames :: [(ValidationFlag, T.Text)]
valNames = map (second vn1 . snd) valOpts
  where vn1 = T.pack . unwords . words

valHelp :: String -> String
valHelp str = unlines $ str : map vh1 valOpts

vh1 :: (Char, (ValidationFlag, String)) -> String
vh1 (c, (_, str)) = concat [ "  "
                           , show c
                           , " - \t"
                           , str
                           ]

--

scUsage :: Visibility -> Maybe Int -> LT.Text
scUsage v w = getUsageWithVisibility (<= v) $ usageOpts w ++ descrs

usageOpts :: Maybe Int -> [UsgD]
usageOpts w = [UsageWidth w, UsageIndent ind]
  where ind = getInd w

minInd, maxInd :: Int
minInd = 10
maxInd = 28

getInd :: Maybe Int -> Int
getInd Nothing = maxInd
getInd (Just w)
  | w >= w80  = maxInd
  | otherwise = ind `max` minInd
  where
    w80 = 78
    dec = (w80 - w) `div` 2
    ind = maxInd - dec

scGetOpts :: [String] -> ScOpts
scGetOpts args =
  let ao              = ReturnInOrder (OptEnt DirInput)
      (opts, _, errs) = getOpt ao (optsOnly descrs) args
      scOpts          = makeOpts opts
  in scOpts { optErrors = map noNL errs ++ optErrors scOpts }

noNL :: String -> String
noNL = dropWhileEnd isSpace

scCheckDups :: Either LT.Text LT.Text
scCheckDups = checkDups descrs
