{-|
Module      : LogTest
Description : Test System.Loam.Log
Copyright   : © Mignon Pelletier, 2025
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module LogTest
  ( testLog
  , parseLine
  ) where

-- import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.Text     as A
import Data.Char
import Data.List
import qualified Data.Text                as T
import System.Directory
import System.IO
import Test.Tasty.HUnit
import Text.Printf

import System.Loam.Log

import PlasmaTestUtil (tmpDir)

line1, line2 :: T.Text
line1 = "If you like piña coladas"
line2 = "And gettin' caught in the rain"

xtreme :: T.Text
xtreme = T.pack $ concat [ "An extr"
                         , replicate 500 'e'
                         , "mely l"
                         , replicate 500 'o'
                         , "ng line"
                         , replicate 100 '!'
                         ]

lns :: [T.Text]
lns = [ line1 <> "\n" <> line2
      , line1
      , line2
      , xtreme
      ]

pfx :: T.Text
pfx = "ESCAPE: "

myCodes :: [LogCode]
myCodes =
  [ 0
  , 37619
  , 0xDEFACED_BAD_FACADE
  ]

showFlags :: [LogFlag]
showFlags =
  [ FlgStackTrace
  , FlgShowTime
  , FlgShowWhereFull
  , FlgShowWhere
  , FlgShowCode
  , FlgShowCodeOrWhere
  , FlgShowPid
  , FlgShowProg
  , FlgShowTid
  , FlgShowTidNonmain
  ]

testLog :: [LogFlag] -> Assertion
testLog flags =
  bracket (testLogOpen flags) testLogClose (testLog1 flags)

testLogOpen :: [LogFlag] -> IO (FilePath, Handle)
testLogOpen flags = openBinaryTempFile tmpDir $ "test-" <> x <> "-.log"
  where x = intercalate "_" $ map flagAbbrev flags

testLogClose :: (FilePath, Handle) -> IO ()
testLogClose (fname, h) = do
  hClose h
  removeFile fname

flagAbbrev :: LogFlag -> String
flagAbbrev flag = lastCap (show flag) : show (fromEnum flag)

lastCap :: String -> Char
lastCap s =
  case dropWhile isAsciiLower (reverse s) of
    c:_ -> c
    _   -> 'x'

testLog1 :: [LogFlag] -> (FilePath, Handle) -> IO ()
testLog1 flags (fname, h) = do
  lvl <- newLogLevel "test"
  levelSetPrefix   lvl pfx
  levelModifyFlags lvl flags (showFlags \\ flags)
  levelSetDestFile lvl $ DestFilePath Overwrite fname
  writeLines lvl id
  writeLines lvl (<> "\n")
  hSeek h AbsoluteSeek 0

writeLines :: LogLevel -> (T.Text -> T.Text) -> IO ()
writeLines lvl f = writeLines1 lvl $ map f lns

writeLines1 :: LogLevel -> [T.Text] -> IO ()
writeLines1 lvl xs = forM_ myCodes $ \c -> forM_ xs (logCode lvl c)

parseLine :: [LogFlag] -> LogCode -> T.Text -> T.Text -> A.Parser ()
parseLine flags code prog msg = do
  A.string pfx
  when (FlgShowProg `elem` flags) $ do
    A.string prog
    skipString ": "
  when (FlgShowPid `elem` flags) $ do
    A.skipWhile isDigit
    skipString ": "
  when (FlgShowTime `elem` flags) $ do
    parseDateTime
  when (FlgShowWhere `elem` flags) $ do
    A.string "LogTest.hs:"
    A.skipWhile isDigit
    skipString ": "
  when (FlgShowCode `elem` flags && code /= 0) $ do
    let codeStr = printf "%08x" code
    A.char '<'
    A.string $ T.pack codeStr
    skipString "> "
  when (FlgShowTid `elem` flags) $ do
    A.string "[t"
    A.skipWhile isDigit
    skipString "] "
  skipString msg

skipString :: T.Text -> A.Parser ()
skipString = void . A.string

months :: [T.Text]
months = T.words "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"

parseDateTime :: A.Parser ()
parseDateTime = do
  A.choice $ map A.string months
  A.skipSpace
  parseOneOrTwoDigits -- day
  A.char ','
  A.skipSpace
  A.count 4 A.digit   -- year
  A.skipSpace
  parseOneOrTwoDigits -- hour
  A.char ':'
  A.count 2 A.digit   -- minute
  A.char ':'
  A.count 2 A.digit   -- second
  A.char '.'
  A.count 2 A.digit   -- hundredths of a second
  A.skipSpace

parseOneOrTwoDigits :: A.Parser ()
parseOneOrTwoDigits = do
  A.digit
  void $ A.option 'x' A.digit
