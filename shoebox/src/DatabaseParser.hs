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

-- parseDatabase :: FilePath -> IO [DBElem]
-- parseDatabase dbFile = do
--   dbTxt <- T.readFile dbFile
--   let result = P.parse parseDBElems dbFile dbTxt
--   undefined

-- parseHelpers :: Parsec Text () [DBElem]
-- parseHelpers = P.many1 parseHelper

parseLexicalElement :: Parsec Text () DBElem
parseLexicalElement = parseHelper "le" LE

parseUUID :: Parsec Text () DBElem
parseUUID = parseHelper "_no" UUID

parseMeaning :: Parsec Text () DBElem
parseMeaning = do
  _ <- P.string "\\me"
  _ <- P.spaces
  --P.sepBy1 (P.many1 (P.satisfy (/= ';'))) (P.string "; ")
  lst <- P.sepEndBy1 (P.many1 (P.noneOf ";\n")) ((P.char ';' >> P.spaces >> return ())
                                   P.<|>
                                   (P.try P.endOfLine >> return ()))
  return $ ME (map T.pack lst)

parseLesson :: Parsec Text () DBElem
parseLesson = parseHelper "lk" (\t -> LK (if T.null t then Nothing else (Just t)))

parseComment :: Parsec Text () DBElem
parseComment = parseHelper "co" (\t -> CO (if T.null t then Nothing else (Just t)))

parseDay :: Parsec Text () DBElem
parseDay = parseHelper "dt" (\t -> DT (if T.null t then Nothing else (Just t)))

parseDBElem :: Parsec Text () DBElem
parseDBElem = P.try parseLexicalElement
          P.<|> P.try parseUUID
          P.<|> P.try parseLesson
          P.<|> P.try parseComment
          P.<|> P.try parseMeaning
          P.<|> P.try parseDay

parseHelper :: String -> (Text -> DBElem) -> Parsec Text () DBElem
parseHelper entry ctor = do
  _ <- P.string ("\\" ++ entry)
  _ <- P.many (P.oneOf " \t")
  ctor . T.pack <$> P.manyTill P.anyChar (P.endOfLine)
