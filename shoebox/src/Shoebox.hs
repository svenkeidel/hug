{-# LANGUAGE OverloadedStrings #-}
module Shoebox where

import Data.Text (Text)
import qualified Data.Map as M
import Data.Maybe

-- types of databases
type ShoeSegmentationDB = M.Map Text [MorphemeBreak]  -- segmentation
type ShoeLexiconDB = M.Map LexEl [Meaning]       -- base words, lexicon
type ShoeSuffixDB  = M.Map SuffixEl [Abbreviation] -- suffixes
type ShoePrefixDB  = M.Map PrefixEl [Abbreviation] -- prefixes

type SuffixEl = Text
type PrefixEl = Text
type Abbreviation = Text
type Meaning = Text
type LexEl  = Text  -- inside lexicon
type FullEl = Text -- full element
type Suffix = Text -- suffix
type Prefix = Text -- prefix

data GlossChoice = AbbreviationChoice [Abbreviation]
                 | MeaningChoice [Meaning]
  deriving (Show,Eq)

data Gloss = Abbreviation Abbreviation
           | Meaning Meaning
  deriving (Show,Eq)

newtype MorphemeBreak = MB [Morpheme]
  deriving (Show,Eq)

data Morpheme = MorphemeLex LexEl | MorphemeSuffix Suffix | MorphemePrefix Prefix
  deriving (Show,Eq)

data InterlinearBlock = ILB MorphemeBreak [GlossChoice]
  deriving (Show,Eq)

type TextEl = Text

shoeLexiconDB :: ShoeLexiconDB
shoeLexiconDB = M.fromList
  [ ("maison",["house", "building"])
   	, ("lorsque",["when"]
   	, ("avoir",["to_have"]
   	, ("six",["six"]
   	, ("an",["year"]
  ]

shoeSuffixDB :: ShoeSuffixDB
shoeSuffixDB = M.fromList
  [ ("s",["PL"])
  	, ("ais",["1sIPF"])
  ]

shoePrefixDB :: ShoePrefixDB
shoePrefixDB = M.fromList
  [ ("je",["1sPRON"])
  ]

shoeSegmentationDB :: ShoeSegmentationDB
shoeSegmentationDB = M.fromList
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
   , ("j'avais", [ MB [ MorphemePrefix "je"
	 										, MorphemeLex "avoir"
                      , MorphemeSuffix "ais"
                      ]
                 ]
     )
   ]

type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoePrefixDB, ShoeSegmentationDB)

shoeDB :: ShoeDB
shoeDB = (shoeLexiconDB, shoeSuffixDB, shoePrefixDB, shoeSegmentationDB)

breakTX :: TextEl -> ShoeSegmentationDB -> [MorphemeBreak]
breakTX textEl segmentationDB =
  fromMaybe [MB [MorphemeLex textEl]]
    (M.lookup textEl segmentationDB)

lookupMB :: MorphemeBreak -> ShoeLexiconDB -> ShoeSuffixDB -> ShoePrefixDB -> [GlossChoice]
lookupMB (MB mbs) lexiconDB suffixDB prefixDB = map go mbs
  where
    go (MorphemeLex l)    = MeaningChoice $      fromMaybe [] (M.lookup l lexiconDB)
    go (MorphemeSuffix s) = AbbreviationChoice $ fromMaybe [] (M.lookup s suffixDB)
    go (MorphemePrefix p) = AbbreviationChoice $ fromMaybe [] (M.lookup s prefixDB)

gloss :: TextEl -> ShoeDB -> [InterlinearBlock]
gloss textEl (lexiconDB,suffixDB,segmentationDB) = do
  morphemeBreak <- breakTX textEl segmentationDB
  let glosses = lookupMB morphemeBreak lexiconDB suffixDB prefixDB
  return $ ILB morphemeBreak glosses
