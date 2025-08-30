module SlawCat.CmdLine.Pretty
  ( UsageDescrV(..)
  , UsageDescr
  , getUsage
  , getUsageWithVisibility
  , optsOnly
  , checkDups
  , checkDups'
  , makePod
  , makePodWithVisibility
  ) where

import Data.Char
import Data.Either
import Data.List
import qualified Data.Map.Strict            as M
import Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as R
import qualified Data.Text.Lazy.Builder.Int as R
import System.Console.GetOpt
import Text.Wrap

data UsageDescrV v a = UsageWidth      (Maybe Int)
                     | UsageIndent     Int
                     | UsageVisibility v
                     | UsageLine       T.Text
                     | UsageOpt        (OptDescr a)

type UsageDescr = UsageDescrV ()

data UsageOpts = UsageOpts
  { uoWidth   :: Maybe Int
  , uoIndent  :: T.Text
  , uoVisible :: !Bool
  } deriving (Eq, Ord, Show)

dfltOpts :: UsageOpts
dfltOpts = UsageOpts
  { uoWidth   = Nothing
  , uoIndent  = mkPad 25
  , uoVisible = True
  }

mkPad :: Int -> T.Text
mkPad = (`T.replicate` " ")

-- optsOnly

optsOnly :: [UsageDescrV v a] -> [OptDescr a]
optsOnly = mapMaybe oo1

oo1 :: UsageDescrV v a -> Maybe (OptDescr a)
oo1 (UsageOpt x) = Just x
oo1 _            = Nothing

-- getUsage

getUsage :: [UsageDescr a] -> LT.Text
getUsage = getUsageWithVisibility (const True)

getUsageWithVisibility
  :: (v -> Bool)
  -> [UsageDescrV v a]
  -> LT.Text
getUsageWithVisibility f =
  R.toLazyText . mconcat . map gu1 . flattenUsage f

gu1 :: (UsageDescr a, UsageOpts) -> R.Builder
gu1 (UsageLine   t, uo) = fmtLine uo t
gu1 (UsageOpt    o, uo) = fmtOpt  uo o
gu1 _                   = mempty

flattenUsage
  :: (v -> Bool)
  -> [UsageDescrV v a]
  -> [(UsageDescr a, UsageOpts)]
flattenUsage = fu1 dfltOpts

fu1 :: UsageOpts
    -> (v -> Bool)
    -> [UsageDescrV v a]
    -> [(UsageDescr a, UsageOpts)]
fu1 _   _ []                    = mempty
fu1 !uo f (UsageWidth   w:rest) = fu1 (uo { uoWidth  = w       }) f rest
fu1 !uo f (UsageIndent  i:rest) = fu1 (uo { uoIndent = mkPad i }) f rest
fu1 !uo f (UsageVisibility v:rest) = fu1 (uo { uoVisible = f v }) f rest
fu1 !uo f (_             :rest)
  | not (uoVisible uo)          =                     fu1 uo f rest
fu1 !uo f (UsageLine    t:rest) = (UsageLine t, uo) : fu1 uo f rest
fu1 !uo f (UsageOpt     o:rest) = (UsageOpt  o, uo) : fu1 uo f rest


nl :: R.Builder
nl = R.singleton '\n'

fmtLine :: UsageOpts -> T.Text -> R.Builder
fmtLine uo txt =
  let w    = uoWidth uo
      lns1 = T.lines txt
      lns2 = concatMap (wrapMaybe w 0)   lns1
      lns3 = if null lns2 then [""] else lns2 -- at least one line
      lns4 = map ((<> nl) . R.fromText)  lns3
  in mconcat lns4

wrapMaybe :: Maybe Int -> Int -> T.Text -> [T.Text]
wrapMaybe Nothing  _   txt = [rmTabs txt]
wrapMaybe (Just w) ind txt = wrapLine w ind txt

fmtOpt :: UsageOpts -> OptDescr a -> R.Builder
fmtOpt uo (Option shrt long arg desc) =
  fmtDescription uo (fmtOptName shrt long (fmtArg id arg)) desc

fmtArg
  :: (String -> String)
  -> ArgDescr a
  -> (String, String) -- (long-arg, short-arg)
fmtArg _ (NoArg  _    ) = ("",                   "")
fmtArg f (ReqArg _ arg) = ('=' : f arg,          ' ' : f arg)
fmtArg f (OptArg _ arg) = ("[=" ++ f arg ++ "]", "[" ++ f arg ++ "]")

fmtOptName :: String -> [String] -> (String, String) -> R.Builder
fmtOptName shrt []   (_, shrtArg) = "  "     <> fmtShort shrt shrtArg
fmtOptName []   long (longArg, _) = "      " <> fmtLong  long longArg
fmtOptName shrt long (longArg, _) =
  "  " <> fmtShort shrt "" <> ", " <> fmtLong long longArg

fmtShort :: [Char] -> String -> R.Builder
fmtShort cs arg = mconcat $ intersperse ", " $ map (fs1 arg) cs

fs1 :: String -> Char -> R.Builder
fs1 arg c = R.singleton '-' <> R.singleton c <> R.fromString arg

fmtLong :: [String] -> String -> R.Builder
fmtLong names arg = mconcat $ intersperse ", " $ map (fl1 arg) names

fl1 :: String -> String -> R.Builder
fl1 arg name = "--" <> R.fromString name <> R.fromString arg

fmtDescription :: UsageOpts -> R.Builder -> String -> R.Builder
fmtDescription uo nameBldr desc =
  let pad       = uoIndent uo
      indent    = T.length pad
      optNames  = LT.toStrict $ R.toLazyText nameBldr
      origLines = T.lines $ T.pack desc
      descLines = fmtDescLines (uoWidth uo) indent origLines
      (firstLine, firstPad) = insertNames optNames pad
      pads      = firstPad : repeat pad
      lns       = firstLine ++ zipWith (<>) pads descLines
  in mconcat $ map bldLine lns

fmtDescLines :: Maybe Int -> Int -> [T.Text] -> [T.Text]
fmtDescLines _        _      []  = [""] -- ensure at least one line
fmtDescLines Nothing  _      lns = map rmTabs lns
fmtDescLines (Just w) indent lns = concatMap (wrapLine ww 0) lns
  where ww = (w - indent) `max` 10

wrapLine :: Int -> Int -> T.Text -> [T.Text]
wrapLine ww ind1 txt1 =
  let (ind2, txt2) = findTab txt1
      ind          = fromMaybe ind1 ind2
      dflt         = defaultWrapSettings
      strat        = if ind > 0 then FillIndent ind else NoFill
      settings     = dflt { fillStrategy = strat
                          , fillScope    = FillAfterFirst
                          }
  in wrpTxtToLns settings (Just ww) txt2

bldLine :: T.Text -> R.Builder
bldLine ln = R.fromText ln' <> nl
  where ln' = T.dropWhileEnd isSpace ln

insertNames :: T.Text -> T.Text -> ([T.Text], T.Text)
insertNames names pad =
  let nameLen = T.length names
      padLen  = T.length pad
      nPad    = padLen - nameLen
  in if nPad > 0
     then ([],      names <> T.take nPad pad)
     else ([names], pad)

findTab :: T.Text -> (Maybe Int, T.Text)
findTab txt =
  let (t1, t2) = T.break (== '\t') txt
  in case T.null t2 of
       True  -> (Nothing,            txt)
       False -> (Just (T.length t1), T.filter (/= '\t') txt)

rmTabs :: T.Text -> T.Text
rmTabs = snd . findTab

-- checkDups

type ShortMap = M.Map Char   [T.Text]
-- type LongMap  = M.Map T.Text [T.Text]

checkDups :: [UsageDescrV v a] -> Either LT.Text LT.Text
checkDups = checkDups' . optsOnly

checkDups' :: [OptDescr a] -> Either LT.Text LT.Text
checkDups' od =
  let (sPairs, lPairs) = partitionEithers $ concatMap xtract od
      sMap             = M.fromListWith (++) sPairs
      lMap             = M.fromListWith (++) lPairs
      errShrt          = checkOpts sMap "-"  T.singleton
      errLong          = checkOpts lMap "--" id
      errs             = R.toLazyText $ errShrt <> errLong
  in case LT.null errs of
       False -> Left errs
       True  -> Right $ showUsedChars sMap

type OptEth = Either (Char, [T.Text]) (T.Text, [T.Text])

xtract :: OptDescr a -> [OptEth]
xtract (Option shrts longs _ desc) =
  let desc' = [desc2txt desc]
  in map (xShort desc') shrts ++ map (xLong desc') longs

desc2txt :: String -> T.Text
desc2txt = T.pack . takeWhile (/= '\n')

xShort :: [T.Text] -> Char -> OptEth
xShort desc c = Left (c, desc)

xLong :: [T.Text] -> String -> OptEth
xLong desc str = Right (T.pack str, desc)

checkOpts
  :: Ord a
  => M.Map a [T.Text]
  -> T.Text
  -> (a -> T.Text)
  -> R.Builder
checkOpts m pfx f = mconcat $ map (checkOpt pfx f) $ M.toList m

checkOpt :: T.Text -> (a -> T.Text) -> (a, [T.Text]) -> R.Builder
checkOpt pfx f (k, v)
  | length v < 2 = mempty
  | otherwise    =
      let ln1 = mconcat [ "duplicates for "
                        , R.fromText pfx
                        , R.fromText (f k)
                        , nl
                        ]
          lns = map descLine v
      in mconcat (ln1 : lns)

descLine :: T.Text -> R.Builder
descLine txt = "    " <> R.fromText txt <> nl

showUsedChars :: ShortMap -> LT.Text
showUsedChars sm =
  let pairs = map (suc1 sm) $ ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
      ln1   = map fst pairs
      ln2   = dropWhileEnd isSpace $ map snd pairs
  in LT.pack $ unlines [ln1, ln2]

suc1 :: ShortMap -> Char -> (Char, Char)
suc1 sm c
  | c `M.member` sm = (c, '^')
  | otherwise       = (c, ' ')

-- makePod

makePod :: [UsageDescr a] -> LT.Text
makePod = makePodWithVisibility (const True)

makePodWithVisibility
  :: (v -> Bool)
  -> [UsageDescrV v a]
  -> LT.Text
makePodWithVisibility f =
  squashBlank . R.toLazyText . mp1 . flattenUsage f

squashBlank :: LT.Text -> LT.Text
squashBlank = LT.unlines . peephole . sb1 True . LT.lines

sb1 :: Bool -> [LT.Text] -> [LT.Text]
sb1 _ [] = []
sb1 skipBlank (ln:rest)
  | isBlank && skipBlank =      sb1 True    rest
  | otherwise            = ln : sb1 isBlank rest
  where
    isBlank = LT.null ln

peephole :: [LT.Text] -> [LT.Text]
peephole []           = []
peephole lns@(ln:rest)
  | take5 == overBack = "" : peephole rest5
  | otherwise         = ln : peephole rest
  where
    (take5, rest5) = splitAt 5 lns

overBack :: [LT.Text]
overBack =
  [ ""
  , "=over 4"
  , ""
  , "=back"
  , ""
  ]

mp1 :: [(UsageDescr a, UsageOpts)] -> R.Builder
mp1 uds@((UsageLine t, uo):rest) =
  case "Usage: " `T.stripPrefix` t of
    Just sfx -> mp2 sfx       (uoWidth uo) rest
    Nothing  -> mp2 dfltUsage Nothing      uds
mp1 uds =       mp2 dfltUsage Nothing      uds

dfltUsage :: T.Text
dfltUsage = "progname options"

mp2 :: T.Text -> Maybe Int -> [(UsageDescr a, UsageOpts)] -> R.Builder
mp2 usg ww uds =
  let (name, args) = T.break isSpace usg
  in mconcat [ mkName        name
             , mkSynopsis    name ww $ T.strip args
             , mkDescription
             , mkOptions     uds
             , mkExamples
             , mkEnvironment
             , mkAuthor
             ]

mkCmd :: R.Builder -> R.Builder
mkCmd cmd = "\n=" <> cmd <> "\n\n"

mkHead :: Int -> T.Text -> R.Builder
mkHead lvl name =
  mkCmd $ "head" <> R.decimal lvl <> " " <> R.fromText name

mkOver :: Int -> R.Builder
mkOver ind = mkCmd $ "over " <> R.decimal ind

mkBack :: R.Builder
mkBack = mkCmd "back"

quoteTxt :: T.Text -> R.Builder
quoteTxt = R.fromString . quoteString . T.unpack

quoteText :: T.Text -> T.Text
quoteText = T.pack . quoteString . T.unpack

quoteString :: String -> String
quoteString = concatMap qs1

qs1 :: Char -> String
qs1 '<'  = "E<lt>"
qs1 '>'  = "E<gt>"
qs1 '\t' = ""
qs1 c    = [c]

quoteNbsp :: T.Text -> T.Text
quoteNbsp = T.unwords . map qn1 . T.words . nbspToNonChar

qn1 :: T.Text -> T.Text
qn1 txt
  | T.any (== nonChar) txt = "S<" <> nonCharToSpace txt <> ">"
  | otherwise              = txt

formatOptions :: T.Text -> T.Text
formatOptions txt =
  let grps  = T.groupBy f txt
      f x y = isArg x == isArg y
  in mconcat $ map formatGroup grps

formatGroup :: T.Text -> T.Text
formatGroup txt =
  case T.uncons txt of
    Just ('-', _)                -> "B<" <>           txt <> ">"
    Just (c,   _) | isAlphaNum c -> "I<" <> T.toUpper txt <> ">"
    _                            -> quoteText         txt

isArg :: Char -> Bool
isArg '-' = True
isArg c   = isAlphaNum c

mkName :: T.Text -> R.Builder
mkName prog = mconcat
  [ mkHead 1 "NAME"
  , quoteTxt prog
  , " - todo: summary\n"
  ]

mkSynopsis :: T.Text -> Maybe Int -> T.Text -> R.Builder
mkSynopsis prog ww args =
  let lns   = wrpTxtToLns defaultWrapSettings ww $ ms1 prog args
      bldrs = mkHead 1 "SYNOPSIS" : map ((<> nl) . R.fromText) lns
  in mconcat bldrs

ms1 :: T.Text -> T.Text -> T.Text
ms1 prog args = mconcat
  [ "B<"
  , quoteText prog
  , "> "
  , quoteNbsp $ formatOptions args
  ]

mkPlaceholder :: T.Text -> R.Builder -> R.Builder
mkPlaceholder name body = mconcat
  [ mkHead 1 name
  , body
  , nl
  ]

mkDescription :: R.Builder
mkDescription = mkPlaceholder "DESCRIPTION" "todo: description"

mkExamples :: R.Builder
mkExamples = mkPlaceholder "EXAMPLES" "todo: examples"

mkEnvironment :: R.Builder
mkEnvironment = mkPlaceholder "ENVIRONMENT" $ mconcat
  [ mkOver 4
  , mkCmd "item SOME_ENV_VAR"
  , "Describe this environment variable."
  , nl
  , mkBack
  ]

mkAuthor :: R.Builder
mkAuthor = mkPlaceholder "AUTHOR" $ quoteTxt txt
  where txt = "Firstname Lastname <email at example dot com>"

mkOptions :: [(UsageDescr a, UsageOpts)] -> R.Builder
mkOptions uds = mconcat
  [ mkHead 1 "OPTIONS"
  , mkOver 4
  , mconcat $ map mo1 uds
  , mkBack
  ]

mo1 :: (UsageDescr a, UsageOpts) -> R.Builder
mo1 (UsageLine t, uo) = moUL t uo
mo1 (UsageOpt  o, uo) = moUO o uo
mo1 _                 = mempty

moUL :: T.Text -> UsageOpts -> R.Builder
moUL ln _
  | T.null ln = mempty
  | otherwise = mconcat [ mkBack
                        , mkHead 2 $ quoteText $ ln'
                        , mkOver 4
                        ]
  where ln' = T.dropWhileEnd (== ':') ln

moUO :: OptDescr a -> UsageOpts -> R.Builder
moUO (Option shrt long ad desc) uo =
  let ww   = uoWidth uo
      opts = map (shrtOpt ad) shrt ++ map (longOpt ad) long
      lns  = map (podWrap ww) $ lines $ quoteString desc
      item = mconcat $ "item " : intersperse ", " opts
  in mconcat $ mkCmd item : lns

shrtOpt :: ArgDescr a -> Char -> R.Builder
shrtOpt ad shrt = podOpt ad ['-', shrt] snd

longOpt :: ArgDescr a -> String -> R.Builder
longOpt ad long = podOpt ad ("--" ++ long) fst

podOpt
  :: ArgDescr a
  -> String
  -> ((String, String) -> String)
  -> R.Builder
podOpt ad name f = mconcat
  [ "B<"
  , R.fromString $ quoteString name
  , ">"
  , R.fromString $ f $ fmtArg podItalic ad
  ]

podItalic :: String -> String
podItalic x = "I<" ++ quoteString x ++ ">"

podWrap :: Maybe Int -> String -> R.Builder
podWrap _ []             = nl
podWrap ww s@(c:_)
  | isSpace c            = R.fromString qs <> nl
  | otherwise            = nl <> mconcat lns2 <> nl
  where
    qs   = quoteString s
    ln   = T.pack qs
    lns1 = wrpTxtToLns defaultWrapSettings ww ln
    lns2 = map ((<> nl) . R.fromText) lns1

wrpTxtToLns :: WrapSettings -> Maybe Int -> T.Text -> [T.Text]
wrpTxtToLns ws ww = map nonCharToSpace . wttl1 ws ww . nbspToNonChar

nonChar, nbsp :: Char
nonChar = chr 0xFFFF
nbsp    = chr 0x00A0

nonCharToSpace :: T.Text -> T.Text
nonCharToSpace = T.map f
  where f c | c == nonChar = ' '
            | otherwise    = c

nbspToNonChar :: T.Text -> T.Text
nbspToNonChar = T.map f
  where f c | c == nbsp = nonChar
            | otherwise = c

wttl1 :: WrapSettings -> Maybe Int -> T.Text -> [T.Text]
wttl1 _  Nothing  ln = [ln]
wttl1 ws (Just w) ln
  | T.length ln <= w = [ln]
  | otherwise        = wrapTextToLines ws w ln
