module SlawCat.CmdLine.Pretty
  ( UsageDescrV(..)
  , UsageDescr
  , getUsage
  , getUsageWithVisibility
  , optsOnly
  , checkDups
  , checkDups'
  ) where

import Data.Char
import Data.Either
import Data.List
import qualified Data.Map.Strict          as M
import Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Builder   as R
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
getUsageWithVisibility f = R.toLazyText . gu1 f dfltOpts

gu1 :: (v -> Bool) -> UsageOpts -> [UsageDescrV v a] -> R.Builder
gu1 _ _ [] = mempty
gu1 f !uo (UsageWidth  w:rest) = gu1 f (uo { uoWidth  = w       }) rest
gu1 f !uo (UsageIndent i:rest) = gu1 f (uo { uoIndent = mkPad i }) rest
gu1 f !uo (UsageVisibility v:rest) = gu1 f (uo { uoVisible = f v }) rest
gu1 f !uo (_            :rest)
  | not (uoVisible uo)         =                 gu1 f uo rest
gu1 f !uo (UsageLine   t:rest) = fmtLine uo t <> gu1 f uo rest
gu1 f !uo (UsageOpt    o:rest) = fmtOpt  uo o <> gu1 f uo rest

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
  fmtDescription uo (fmtOptName shrt long (fmtArg arg)) desc

fmtArg :: ArgDescr a -> (String, String) -- (long-arg, short-arg)
fmtArg (NoArg  _    ) = ("",                 "")
fmtArg (ReqArg _ arg) = ('=' : arg,          ' ' : arg)
fmtArg (OptArg _ arg) = ("[=" ++ arg ++ "]", "[" ++ arg ++ "]")

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
  in wrapTextToLines settings ww txt2

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
