module MusicXMLSpec (spec) where

import Control.Lens
import MusicXML
import Text.XML
import Text.XML.Lens
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "prokofiev" do
    it "reads the right number of notes" do
      nTimeSteps <$> prok `shouldReturn` 63

nTimeSteps :: Document -> Int
nTimeSteps doc = length (readTimeSteps (doc ^.. root . timeStep))

readXML :: FilePath -> IO Document
readXML = Text.XML.readFile def

prok, brahms, sibelius, ysaye, tartini :: IO Document
prok = readXML "/home/joseph/Documents/MuseScore3/Scores/Prokofiev_violin_concerto_No_2_excerpt.musicxml"
brahms = readXML "/home/joseph/Documents/MuseScore3/Scores/Brahms_violin_concerto.musicxml"
sibelius = readXML "/home/joseph/Documents/MuseScore3/Scores/Sibelius_violin_concerto_excerpt.musicxml"
ysaye = readXML "/home/joseph/Documents/MuseScore3/Scores/Ysaye ballade excerpt.txt"
tartini = readXML "/home/joseph/Documents/MuseScore3/Scores/tartini_devil_page_1.musicxml"
