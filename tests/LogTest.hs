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
  ) where

-- import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.Text     as A
import Data.Char
import Data.List
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import System.Directory
import System.IO
import Test.Tasty.HUnit
import Text.Printf

import System.Loam.Log
import System.Loam.Util

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

expLines :: [T.Text]
expLines = [ "(1/2): " <> line1
           , "(2/2): " <> line2
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
  txt <- T.hGetContents h
  checkLines flags (T.lines txt)

writeLines :: LogLevel -> (T.Text -> T.Text) -> IO ()
writeLines lvl f = writeLines1 lvl $ map f lns

writeLines1 :: LogLevel -> [T.Text] -> IO ()
writeLines1 lvl xs = forM_ myCodes $ \c -> forM_ xs (logCode lvl c)

checkLines :: [LogFlag] -> [T.Text] -> IO ()
checkLines flags xs = do
  let p1 = zip [1..] xs
  p2 <- checkLines1 flags p1
  p3 <- checkLines1 flags p2
  0 @=? length p3

checkLines1 :: [LogFlag] -> [(Int, T.Text)] -> IO [(Int, T.Text)]
checkLines1 = checkLines2 myCodes

checkLines2
  :: [LogCode]
  -> [LogFlag]
  -> [(Int, T.Text)]
  -> IO [(Int, T.Text)]
checkLines2 []           _     pairs = return pairs
checkLines2 (code:codes) flags pairs = do
  rest <- checkLines3 flags code pairs
  checkLines2 codes flags rest

checkLines3
  :: [LogFlag]
  -> LogCode
  -> [(Int, T.Text)]
  -> IO [(Int, T.Text)]
checkLines3 flags code pairs = do
  let nExp     = length  expLines
      (p1, p2) = splitAt nExp pairs
      numP1    = length  p1
  nExp @=? numP1
  zipWithM_ (checkLine flags code) p1 expLines
  return p2

checkLine :: [LogFlag] -> LogCode -> (Int, T.Text) -> T.Text -> IO ()
checkLine flags code (lNum, ln) expected = do
  prog <- getProgName
  let pl  = parseLine flags code prog
      ctx = concat [ "line "
                   , show lNum
                   , ", code "
                   , printf "%08x" code
                   , ":"
                   ]
  case A.feed (A.parse pl ln) "" of
    A.Fail leftover _ msg -> assertFailure $ mkFailMsg ctx leftover msg
    A.Done _ actual       -> assertEqual ctx expected actual
    _                     -> assertFailure $ ctx ++ " Unknown."

mkFailMsg :: String -> T.Text -> String -> String
mkFailMsg ctx leftover msg =
  ctx ++ " Couldn't parse " ++ show leftover ++ ": " ++ msg

parseLine :: [LogFlag] -> LogCode -> T.Text -> A.Parser T.Text
parseLine flags code prog = do
  A.string pfx
  when (FlgShowProg `elem` flags) $ do
    A.string prog
    skipString ": "
  when (FlgShowPid `elem` flags) $ do
    A.skipWhile isDigit
    skipString ": "
  when (FlgShowTime `elem` flags) $ do
    parseDateTime
  when (FlgShowWhere        `elem` flags ||
        (FlgShowCodeOrWhere `elem` flags && code == 0)) $ do
    parseWhere
  when ((FlgShowCode        `elem` flags ||
         FlgShowCodeOrWhere `elem` flags) && code /= 0) $ do
    parseCode code
  when (FlgShowTid `elem` flags) $ do
    A.string "[t"
    A.skipWhile isDigit
    skipString "] "
  A.takeText

parseWhere :: A.Parser ()
parseWhere = do
  A.string "LogTest.hs:"
  A.skipWhile isDigit
  skipString ": "

parseCode :: LogCode -> A.Parser ()
parseCode code = do
  let codeStr = printf "%08x" code
  A.char '<'
  A.string $ T.pack codeStr
  skipString "> "

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
