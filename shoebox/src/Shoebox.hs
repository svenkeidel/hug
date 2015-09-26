{-# LANGUAGE OverloadedStrings #-}
module Shoebox where

import Data.Text (Text)
import qualified Data.Map as M
import Data.Maybe

-- types of databases
type ShoeParsingDB = M.Map Text [MorphemeBreak]  -- parsing
type ShoeLexiconDB = M.Map LexEl [Meaning]       -- base words, lexicon
type ShoeSuffixDB  = M.Map SuffixEl [Abbreviation] -- suffixes

type SuffixEl = Text
type Abbreviation = Text
type Meaning = Text
type LexEl  = Text  -- inside lexicon
type FullEl = Text -- full element
type Suffix = Text -- suffix

data GlossChoice = AbbreviationChoice [Abbreviation]
                 | MeaningChoice [Meaning]
  deriving (Show,Eq)

data Gloss = Abbreviation Abbreviation
           | Meaning Meaning
  deriving (Show,Eq)

newtype MorphemeBreak = MB [Morpheme]
  deriving (Show,Eq)

data Morpheme = MorphemeLex LexEl | MorphemeSuffix Suffix
  deriving (Show,Eq)

data InterlinearBlock = ILB MorphemeBreak [GlossChoice]
  deriving (Show,Eq)

type TextEl = Text

shoeLexiconDB :: ShoeLexiconDB
shoeLexiconDB = M.fromList
  [ ("maison",["house", "building"])
  ]

shoeSuffixDB :: ShoeSuffixDB
shoeSuffixDB = M.fromList
  [ ("s",["PL"])
  ]

shoeParsingDB :: ShoeParsingDB
shoeParsingDB = M.fromList
   [ ("maisons", [ MB [ MorphemeLex "maison"
                      , MorphemeSuffix "s"
                      ]
                 ]
     )
   , ("cadeaux", [ MB [ MorphemeLex "cadeau"
                      , MorphemeSuffix "s"
                      ]
                 ]
     )
   ]

type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoeParsingDB)

shoeDB :: ShoeDB
shoeDB = (shoeLexiconDB, shoeSuffixDB, shoeParsingDB)

glossTX :: TextEl -> ShoeParsingDB -> [MorphemeBreak]
glossTX textEl parsingDB =
  fromMaybe [MB [MorphemeLex textEl]]
    (M.lookup textEl parsingDB)

glossMB :: MorphemeBreak -> ShoeLexiconDB -> ShoeSuffixDB -> [GlossChoice]
glossMB (MB mbs) lexiconDB suffixDB = map go mbs
  where
    go (MorphemeLex l)    = MeaningChoice $      fromMaybe [] (M.lookup l lexiconDB)
    go (MorphemeSuffix s) = AbbreviationChoice $ fromMaybe [] (M.lookup s suffixDB)

gloss :: TextEl -> ShoeDB -> [InterlinearBlock]
gloss textEl (lexiconDB,suffixDB,parsingDB) = do
  morphemeBreak <- glossTX textEl parsingDB
  let glosses = glossMB morphemeBreak lexiconDB suffixDB
  return $ ILB morphemeBreak glosses
