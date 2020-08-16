module MusicXML
  ( separateFingerings,
    mergeFingerings,
    adjustMeasures,
    measureNumbers,
  )
where

import ClassyPrelude hiding (Element)
import Control.Category ((>>>))
import Control.Lens
import Data.Text.Lens
import Text.XML
import Text.XML.Lens

-- | Remove the
-- <technical><fingering>f</fingering><string>s</string></technical>
-- elements from the top level score element
separateFingerings :: Document -> (Document, [[(Int, Node)]])
separateFingerings score =
  ( score & root . deep (el "technical") . nodes %~ filter (not . isFingering),
    score ^.. root . deep (el "technical")
      <&> (view nodes >>> zip [0 ..] >>> filter (isFingering . snd))
  )

-- | Inverse of 'separateFingerings'
mergeFingerings :: Document -> [[(Int, Node)]] -> Document
mergeFingerings music fingerings =
  over
    (partsOf $ root . deep (el "technical"))
    (zipWith merge fingerings)
    music

merge :: [(Int, Node)] -> Element -> Element
merge fingerings = over nodes (go 0 fingerings)
  where
    go :: Int -> [(Int, a)] -> [a] -> [a]
    go _ [] rest = rest
    go _ fs [] = map snd fs
    go i fs'@((j, f) : fs) ts'@(t : ts)
      | i == j = f : go (i + 1) fs ts'
      | otherwise = t : go (i + 1) fs' ts

isFingering :: Node -> Bool
isFingering e = (e ^? _Element . name) `elem` [Just "fingering", Just "string"]

-- | Shift measure numbers to start at the given point
adjustMeasures :: Int -> Document -> Document
adjustMeasures beg = over (root . measureNumbers) (+ (beg - 1))

measureNumbers :: Traversal' Element Int
measureNumbers =
  deep (el "measure")
    . filtered (\measure -> measure ^? attr "implicit" /= Just "yes")
    . attr "number"
    . unpacked
    . _Show

--------------------------------------------------------------------------------
-- repl utils
--------------------------------------------------------------------------------

-- readXML :: FilePath -> IO Document
-- readXML = Text.XML.readFile def

-- prok = readXML "/home/joseph/Documents/MuseScore3/Scores/Prokofiev_violin_concerto_No_2_excerpt.musicxml"

-- brahms = readXML "/home/joseph/Documents/MuseScore3/Scores/Brahms_violin_concerto.musicxml"
