{-# LANGUAGE OverloadedStrings #-}
module ShoeboxSpec(main, spec) where

import           Test.Hspec
import           Shoebox

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Shoebox" $ do

  it "can break up a piece of text into morphemes" $ do
    glossTX "maisons" shoeParsingDB `shouldBe` [ MB [MorphemeLex "maison", MorphemeSuffix "s"] ]
    glossTX "cadeaux" shoeParsingDB `shouldBe` [ MB [MorphemeLex "cadeau", MorphemeSuffix "s"] ]

  it "can lookup the meanings of a morpheme break" $
    glossMB (MB [MorphemeLex "maison", MorphemeSuffix "s"])
            shoeLexiconDB shoeSuffixDB
      `shouldBe` [ MeaningChoice ["house", "building"], AbbreviationChoice ["PL"] ]
	
  it "can gloss a piece of text" $ do
		gloss "maisons" shoeDB
			`shouldBe` [ILB [TX "maisons", (MB [MorphemeLex "maison",MorphemeSuffix "s"]), (GL [Meaning "house",Abbreviation "PL"])]]

	it "can gloss a sentence" $ do
		intl "Lorsque j'avais six ans"
			`shouldBe`
			[
			[ILB [TX "Lorsque", (MB [MorphemeLex "lorsque"]), (GL [Meaning "when"])]],
			[ILB [TX "j'avais", (MB [MorphemePrefix "je", MorphemeLex "avoir",MorphemeSuffix "ais"]), (GL [Abbreviation "1sPRON", Meaning "to_have",Abbreviation "1sIPF"])]],
			[ILB [TX "six", (MB [MorphemeLex "six"]), (GL [Meaning "six"])]],
			[ILB [TX "ans", (MB [MorphemeLex "an",MorphemeSuffix "s"]), (GL [Meaning "year",Abbreviation "PL"])]]
			]

	it "can pretty print an interlinear block" $ do
		pp [ILB [TX "maisons", (MB [MorphemeLex "maison",MorphemeSuffix "s"]), (GL [Meaning "house",Abbreviation "PL"])]]
			`shouldBe`
			"maisons\nmaison-s\nhouse -PL\n"

	it "can strip off punctuation from a sentence" $ do
		removePunc enAbbrevs "This, however, is an «example» for removed punctuation-marks - ignoring in-word punctuation like in dates or abbreviations, e.g. 21.10.2015."
			`shouldBe`
			"This however is an example for removed punctuation-marks ignoring in-word punctuation like in dates or abbreviations e.g. 21.10.2015"



