{-# LANGUAGE OverloadedStrings #-}

module ShoeOne ()
where

import qualified Data.Text as T
import qualified Data.Map as M

shoeDB :: M.Map T.Text T.Text
shoeDB = M.fromList [
	("maison","house")
	]

type ShoeDB = M.Map T.Text T.Text

gloss :: T.Text -> ShoeDB -> Maybe T.Text
gloss w db = M.lookup w db

