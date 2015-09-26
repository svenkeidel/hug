{-# LANGUAGE OverloadedStrings #-}
module ShoeboxSpec(main, spec) where

import           Test.Hspec
import           Shoebox

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Shoebox" $ do

  it "can gloss a piece of text" $ do
    glossTX "maisons" shoeParsingDB `shouldBe` [ MB [MorphemeLex "maison", MorphemeSuffix "s"] ]
    glossTX "cadeaux" shoeParsingDB `shouldBe` [ MB [MorphemeLex "cadeau", MorphemeSuffix "s"] ]

  it "can lookup the meaning of a morpheme break" $
    glossMB (MB [MorphemeLex "maison", MorphemeSuffix "s"])
            shoeLexiconDB shoeSuffixDB
      `shouldBe` [ MeaningChoice ["house", "building"], AbbreviationChoice ["PL"] ]
