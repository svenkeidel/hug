{-# LANGUAGE OverloadedStrings #-}

module DatabaseParser where

import           Data.Monoid ((<>))
import           Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Calendar (Day)



data DBElem
  = LE Text
  | UUID Text
  | HD Text
  | ME [Text]
  | LK (Maybe Text)
  | CO (Maybe Text)
  | DT (Maybe Text)
  deriving (Show)

data DBRecord = DBRecord
            { lexicalEntry :: Text
            , uuid :: Int
            , meaning :: [Text]
            , lesson :: Maybe Text
            , comment :: Maybe Text
            , date :: Maybe Day
            }


parseDatabase :: FilePath -> IO String
parseDatabase dbFile = do
   dbTxt <- T.readFile dbFile
   let result = case P.parse parseDB dbFile dbTxt of
                        Left err -> show err
                        Right val -> show val
   return result

-- base delimiters and sections, to cope with \n in between
-- the rules are as follows:
-- a "line" starts with a newline + "\"
-- a "section" separator is a newline followed by a line starter

parseSecSep = P.try (P.newline >> P.many1 P.newline)
parseElemSep = P.try (P.newline >> (P.lookAhead (P.char '\\')))
parseElemChar = do
  P.try (P.noneOf ['\n', ';'] )
  P.<|> 
  (P.try (P.newline >> P.lookAhead (P.noneOf ['\\', '\n']) ))

parseDB :: Parsec Text () [[DBElem]]
parseDB = P.sepEndBy1 parseRecord parseSecSep

parseRecord :: Parsec Text () [DBElem]
parseRecord = P.sepEndBy1 parseDBElem parseElemSep

parseDBElem :: Parsec Text () DBElem
parseDBElem = P.try parseLexicalElement
          P.<|> P.try parseUUID
          P.<|> P.try parseLesson
          P.<|> P.try parseComment
          P.<|> P.try parseMeaning
          P.<|> P.try parseDay
          P.<|> parseHeader

parseLexicalElement :: Parsec Text () DBElem
parseLexicalElement = do
  P.string "\\le" >> P.spaces 
  t <- P.many parseElemChar
  return (LE (T.pack t))

parseUUID :: Parsec Text () DBElem
parseUUID = do
  P.string "\\_no" >> P.spaces
  t <- P.many parseElemChar
  return (UUID (T.pack t))

parseMeaning :: Parsec Text () DBElem
parseMeaning = do
  P.string "\\me" >> P.spaces
  ts <- P.sepEndBy (P.many parseElemChar) (P.char ';')
  return (ME (map T.pack ts))

parseLesson :: Parsec Text () DBElem
parseLesson = do
  P.string "\\lk" >> P.spaces
  t <- P.many parseElemChar
  return (LK (if t == [] then Nothing else Just (T.pack t)))

parseComment :: Parsec Text () DBElem
parseComment = do
  P.string "\\co" >> P.spaces
  t <- P.many parseElemChar
  return (CO (if t == [] then Nothing else Just (T.pack t)))

parseDay :: Parsec Text () DBElem
parseDay = do
  P.string "\\dt" >> P.spaces
  t <- P.many parseElemChar
  return (DT (if t == [] then Nothing else Just (T.pack t)))

parseHeader :: Parsec Text () DBElem
parseHeader = do
  P.string "\\_sh" >> P.spaces
  t <- P.many parseElemChar
  return (HD (T.pack t))

