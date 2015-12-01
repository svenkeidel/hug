{-# LANGUAGE OverloadedStrings #-}

module DatabaseParser where

import           Data.Monoid ((<>))
import           Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Calendar (Day)


--
-- generic database parser, working for all database files
--

-- Elements of records

data DBElem
  = LE Text           -- Lexikon, Suff
  | UUID Text
  | HD Text           -- Header only
  | ME [Text]         -- Lexikon only
  | LK (Maybe Text)
  | CO (Maybe Text)
  | DT (Maybe Text)
  | BK [[Text]]       -- Segmentation
  | FL Text
  deriving (Show)

-- Records, simply a list of elements

type DBRecord = [DBElem]

-- parser

-- base delimiters and sections, to cope with \n in between
-- the rules are as follows:
-- a "line" starts with a newline + "\"
-- a "section" separator is a newline followed by a line starter

spaces = P.many (P.oneOf " \t")     -- own spaces without newline
parseSecSep = P.try (P.newline >> P.many1 P.newline)
parseElemSep = P.try (P.newline >> (P.lookAhead (P.char '\\')))
parseElemChar = do
  P.try (P.noneOf ['\n', ';'] )
  P.<|> 
  (P.try (P.newline >> P.lookAhead (P.noneOf ['\\', '\n']) ))

parseElemChar2 = do
  P.try (P.noneOf ['\n', ';','-'] )
  P.<|> 
  (P.try (P.newline >> P.lookAhead (P.noneOf ['\\', '\n']) ))

parseDB :: Parsec Text () [DBRecord]
parseDB = P.sepEndBy1 parseRecord parseSecSep

parseRecord :: Parsec Text () DBRecord
parseRecord = P.sepEndBy1 parseDBElem parseElemSep

parseDBElem :: Parsec Text () DBElem
parseDBElem = P.try parseLexicalElement
          P.<|> P.try parseUUID
          P.<|> P.try parseLesson
          P.<|> P.try parseComment
          P.<|> P.try parseMeaning
          P.<|> P.try parseDay
          P.<|> P.try parseBreak
          P.<|> P.try parseFL
          P.<|> parseHeader

parseLexicalElement :: Parsec Text () DBElem
parseLexicalElement = do
  P.string "\\le" >> spaces 
  t <- P.many parseElemChar
  return (LE (T.pack t))

parseFL :: Parsec Text () DBElem
parseFL = do
  P.string "\\fl" >> spaces 
  t <- P.many parseElemChar
  return (FL (T.pack t))

parseUUID :: Parsec Text () DBElem
parseUUID = do
  P.string "\\_no" >> spaces
  t <- P.many parseElemChar
  return (UUID (T.pack t))

parseMeaning :: Parsec Text () DBElem
parseMeaning = do
  P.string "\\me" >> spaces
  ts <- P.sepEndBy (P.many parseElemChar) (P.char ';' >> spaces)
  return (ME (map T.pack ts))

parseBreak :: Parsec Text () DBElem
parseBreak = do
  P.string "\\bk" >> spaces
  bks <- P.sepEndBy (do
      ts <- P.sepEndBy (P.many parseElemChar2) (P.char '-')
      return (map T.pack ts)
    ) (P.char ';' >> spaces)
  return (BK bks)

parseLesson :: Parsec Text () DBElem
parseLesson = do
  P.string "\\lk" >> spaces
  t <- P.many parseElemChar
  return (LK (if t == [] then Nothing else Just (T.pack t)))

parseComment :: Parsec Text () DBElem
parseComment = do
  P.string "\\co" >> spaces
  t <- P.many parseElemChar
  return (CO (if t == [] then Nothing else Just (T.pack t)))

parseDay :: Parsec Text () DBElem
parseDay = do
  P.string "\\dt" >> spaces
  t <- P.many parseElemChar
  return (DT (if t == [] then Nothing else Just (T.pack t)))

parseHeader :: Parsec Text () DBElem
parseHeader = do
  P.string "\\_sh" >> spaces
  t <- P.many parseElemChar
  return (HD (T.pack t))

--
-- parse Lexicon files
--

data LexEntry = LexEntry
            { leEntry :: Text
            , leUuid :: Int
            , leMeaning :: [Text]
            , leLesson :: Maybe Text
            , leComment :: Maybe Text
            , leDate :: Maybe Text
            }
            deriving (Show)

newLexEntry = LexEntry "" (-1) [] Nothing Nothing Nothing

updateLexEntry :: LexEntry -> DBElem -> LexEntry
updateLexEntry r elem = case elem of
  LE t -> r {leEntry = t}
  UUID t -> r {leUuid = read (T.unpack t)}
  ME t -> r {leMeaning = t}
  LK t -> r {leLesson = t}
  CO c -> r {leComment = c}
  DT d -> r {leDate = d}
  _ -> r

lexEntryFromDBRecord :: DBRecord -> LexEntry
lexEntryFromDBRecord rec = foldl updateLexEntry newLexEntry rec

parseLexDB :: Text -> [LexEntry]
parseLexDB dbTxt = let
  result = case P.parse parseDB "" dbTxt of
                        Left _ -> []
                        Right val -> val
  in (map lexEntryFromDBRecord result)

--
-- parse suffix files
--

data SuffixEntry = SuffixEntry
            { suEntry :: Text
            , suUuid :: Int
            , suMeaning :: [Text]
            , suComment :: Maybe Text
            }
            deriving (Show)

newSuffixEntry = SuffixEntry "" (-1) [] Nothing

updateSuffixEntry :: SuffixEntry -> DBElem -> SuffixEntry
updateSuffixEntry r elem = case elem of
  LE t -> r {suEntry = t}
  UUID t -> r {suUuid = read (T.unpack t)}
  ME t -> r {suMeaning = t}
  CO c -> r {suComment = c}
  _ -> r

sufEntryFromDBRecord :: DBRecord -> SuffixEntry
sufEntryFromDBRecord rec = foldl updateSuffixEntry newSuffixEntry rec

parseSufDB :: Text -> [SuffixEntry]
parseSufDB dbTxt = let
  result = case P.parse parseDB "" dbTxt of
                        Left _ -> []
                        Right val -> val
  in (map sufEntryFromDBRecord result)


--
-- parse segmentation files
--

data SegEntry = SegEntry
            { sgFL :: Text
            , sgUuid :: Int
            , sgMorphemeBreak :: [[Text]]
            }
            deriving (Show)

newSegEntry = SegEntry "" (-1) []

updateSegEntry :: SegEntry -> DBElem -> SegEntry
updateSegEntry r elem = case elem of
  FL t -> r {sgFL = t}
  UUID t -> r {sgUuid = read (T.unpack t)}
  BK v -> r {sgMorphemeBreak = v}
  _ -> r

segEntryFromDBRecord :: DBRecord -> SegEntry
segEntryFromDBRecord rec = foldl updateSegEntry newSegEntry rec

parseSegDB :: Text -> [SegEntry]
parseSegDB dbTxt = let
  result = case P.parse parseDB "" dbTxt of
                        Left _ -> []
                        Right val -> val
  in (map segEntryFromDBRecord result)


--
-- file based input
--

-- usage: 
--    parseDBFile parseLexDB "frz.u8"
--    parseDBFile parseSufDB "frzsf.u8"
--    parseDBFile parseSegDB "frzps.u8"

parseDBFile :: (Text -> a) -> FilePath -> IO a
parseDBFile parseF file = do
  dbTxt <- T.readFile file
  return $ parseF dbTxt

