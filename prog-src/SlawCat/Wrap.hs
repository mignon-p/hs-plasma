module SlawCat.Wrap
  ( wrapMessage
  , wrapOnSep
  , wrapWithInd
  , getWidth
  , nl
  ) where

import qualified Data.Text                as T
import qualified Data.Text.Lazy.Builder   as R
import System.Console.Terminal.Size
import System.IO
import Text.Wrap

import Data.Slaw.Util

                            -- wrapMessage --

wrapMessage
  :: (TextClass a, TextClass b)
  => Maybe Int
  -> T.Text
  -> [a]
  -> b
wrapMessage ww pfx lns =
  let bld = mconcat $ map (wm1 pfx . toTxtBld) lns
  in fromTxtBld $ wm2 ww (T.length pfx) bld

wm1 :: T.Text -> R.Builder -> R.Builder
wm1 pfx bld = R.fromText pfx <> bld <> nl

wm2 :: Maybe Int -> Int -> R.Builder -> R.Builder
wm2 Nothing  _      bld = bld
wm2 (Just w) pfxLen bld =
  let lns1 = T.lines $ toText bld
      lns2 = concatMap (wrpTxtToLns ws w) lns1
      ws   = myWS { fillStrategy = adjInd w pfxLen }
  in lnsToBld lns2

                             -- wrapOnSep --

wrapOnSep
  :: (TextClass a, TextClass b)
  => Maybe Int
  -> T.Text
  -> a
  -> b
wrapOnSep Nothing  _   src = fromLazyText $ toLazyText src
wrapOnSep (Just w) sep src =
  let lns = T.lines $ toText src
      bld = mconcat $ map (ws1 w sep) lns
  in fromTxtBld bld

ws1 :: Int -> T.Text -> T.Text -> R.Builder
ws1 w sep ln =
  let (pfx, sfx) = sep `T.breakOn` ln
      fstrat     = if T.null sfx
                   then NoFill
                   else adjInd w $ T.length pfx + T.length sep
      ws         = myWS { fillStrategy = fstrat }
      lns        = wrpTxtToLns ws w ln
  in lnsToBld lns

                            -- wrapWithInd --

wrapWithInd
  :: (TextClass a, TextClass b)
  => Maybe Int
  -> Int
  -> a
  -> b
wrapWithInd Nothing  _   src = fromLazyText $ toLazyText src
wrapWithInd (Just w) ind src =
  let lns    = T.lines $ toText src
      ws     = myWS { fillStrategy = adjInd w ind }
      bld    = mconcat $ map (wi1 w ws) lns
  in fromTxtBld bld

wi1 :: Int -> WrapSettings -> T.Text -> R.Builder
wi1 w ws = lnsToBld . wrpTxtToLns ws w

                   -- shared by above wrap routines --

nl :: R.Builder
nl = R.singleton '\n'

myWS :: WrapSettings
myWS = defaultWrapSettings
  { breakLongWords = True
  , fillScope      = FillAfterFirst
  }

adjInd :: Int -> Int -> FillStrategy
adjInd w ind
  | w - ind < 15 = FillIndent $ ind `min` 2
  | otherwise    = FillIndent $ ind

lnsToBld :: [T.Text] -> R.Builder
lnsToBld = mconcat . map ((<> nl) . R.fromText)

-- This fixes the issue where wrapTextToLines ""
-- returns [] instead of [""].  (We want the latter.)
wrpTxtToLns :: WrapSettings -> Int -> T.Text -> [T.Text]
wrpTxtToLns ws w ln
  | T.length ln <= w = [ln]
  | otherwise        = wrapTextToLines ws w ln

                              -- getWidth --

getWidth :: Maybe Int -> Handle -> IO (Maybe Int)
getWidth (Just w) _ = return $ Just $ constrainWidth w
getWidth Nothing  h = do
  wind <- hSize h
  return $ fmap (constrainWidth . width) wind

constrainWidth :: Int -> Int
constrainWidth w
  | w <= 20   = 20
  | otherwise = w - 1
