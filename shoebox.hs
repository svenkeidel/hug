{-# LANGUAGE OverloadedStrings #-}

module ShoeOne ()
where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Control.Applicative as CA

-- types of databases
type ShoeLexiconDB = M.Map T.Text T.Text    -- base words, lexicon
type ShoeSuffixDB = M.Map T.Text T.Text   	-- suffixes
type ShoeParsingDB = M.Map T.Text [T.Text]  -- parsing


shoeLexiconDB :: ShoeLexiconDB
shoeLexiconDB = M.fromList [
	("maison","house")
	]

shoeSuffixDB :: ShoeSuffixDB
shoeSuffixDB = M.fromList [
	("s","PL")
	]

shoeParsingDB :: ShoeParsingDB
shoeParsingDB = M.fromList [
	("maisons",["maison", "s"])
	]

type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoeParsingDB)
shoeDB = (shoeLexiconDB, shoeSuffixDB, shoeParsingDB)

helper :: Maybe T.Text -> Maybe [T.Text]
helper Nothing = Nothing
helper (Just val) = Just [val]

gloss :: T.Text -> ShoeDB -> Maybe [T.Text]
gloss w (ldb, sdb, pdb) = M.lookup w pdb CA.<|> helper (M.lookup w sdb) CA.<|> helper (M.lookup w ldb)
