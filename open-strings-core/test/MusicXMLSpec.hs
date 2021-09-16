{-# LANGUAGE TemplateHaskell #-}
module MusicXMLSpec (spec) where

import Control.Lens
import Data.FileEmbed
import MusicXML
import Text.XML
import Text.XML.Lens
import Test.Hspec
import Relude

spec :: Spec
spec =
  describe "read the right number of time steps" do
    it "prokofiev" do
      nTimeSteps prok `shouldBe` 63
    it "brahms" do
      nTimeSteps brahms `shouldBe` 35
    it "sibelius" do
      nTimeSteps sibelius `shouldBe` 50
    it "ysaye" do
      nTimeSteps ysaye `shouldBe` 15
    it "tartini" do
      nTimeSteps tartini `shouldBe` 430


nTimeSteps :: Document -> Int
nTimeSteps doc = length (readTimeSteps (doc ^.. root . timeStep))

readXML :: ByteString -> Document
readXML = parseLBS_ def . toLazy

prok, brahms, sibelius, ysaye, tartini :: Document
prok = readXML $(embedFile "data/Prokofiev_violin_concerto_No_2_excerpt.musicxml")
brahms = readXML $(embedFile "data/Brahms_violin_concerto.musicxml")
sibelius = readXML $(embedFile "data/Sibelius_violin_concerto_excerpt.musicxml")
ysaye = readXML $(embedFile "data/Ysaye ballade excerpt.musicxml")
tartini = readXML $(embedFile "data/tartini_devil_page_1.musicxml")
