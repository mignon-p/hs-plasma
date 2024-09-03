{-|
Module      : Data.Slaw.Extras.Internal.SpewParser
Description : Parse the addresses in the "overview" output
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Extras.Internal.SpewParser
  ( SpewLine(..)
  , parseSpewLine
  ) where

import Control.Applicative
import qualified Data.Attoparsec.Text     as A
import Data.Char
import qualified Data.Text                as T
import qualified Data.Text.Lazy.Builder   as R
import Data.Word

data SpewLine = SpewLine
  { slPrefix ::                R.Builder
  , slAddr   :: {-# UNPACK #-} !Word64
  , slSuffix ::                R.Builder
  } deriving (Eq, Ord, Show)

parseSpewLine :: T.Text -> Either T.Text SpewLine
parseSpewLine txt
  | "slaw" `T.isSuffixOf` preBracket && (not . T.null) postBracket =
      case parseSpewLine' postBracket of
        Nothing -> Left txt
        Just sl -> Right $ sl { slPrefix = preBuilder <> slPrefix sl }
  | otherwise = Left txt
  where
    (preBracket, postBracket) = T.span (/= '[') txt
    preBuilder                = R.fromText preBracket

parseSpewLine' :: T.Text -> Maybe SpewLine
parseSpewLine' txt =
  case A.parseOnly (spewLine <* A.endOfInput) txt of
    Left  _  -> Nothing
    Right sl -> Just sl

spewLine :: A.Parser SpewLine
spewLine = do
  c1 <- A.char '['
  t1 <- A.takeWhile1 isDigit
  t2 <- A.string "o."
  t3 <- A.option T.empty (A.string "0x" <|> A.string "0X")
  x  <- A.hexadecimal
  c2 <- A.char ']'
  t4 <- A.takeLazyText
  return $ SpewLine
    { slPrefix = mconcat [ R.singleton c1
                         , R.fromText  t1
                         , R.fromText  t2
                         , R.fromText  t3
                         ]
    , slAddr   = x
    , slSuffix = R.singleton c2 <> R.fromLazyText t4
    }
