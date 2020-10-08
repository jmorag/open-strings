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
import Data.List.Zipper as Z
import Data.Text.Lens (unpacked)
import Fingering
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
adjustMeasures beg = iset (root . measureNumbers) (+ beg)

measureNumbers :: IndexedTraversal' Int Element Int
measureNumbers =
  indexing $
    deep (el "measure")
      . filtered (\measure -> measure ^? attr "implicit" /= Just "yes")
      . attr "number"
      . unpacked
      . _Show

isTimeStep :: Node -> Bool
isTimeStep e = (e ^? _Element . name) `elem` [Just "note", Just "forward", Just "backup"]

timeStep :: Traversal' Element Element
timeStep = deep (failing (el "note") (failing (el "backup") (el "forward")))

readTimeSteps :: Document -> Maybe [TimeStep]
readTimeSteps doc = Z.toList <$> foldM readTimeStep Z.empty (doc ^.. root . timeStep)

readTimeStep :: Zipper TimeStep -> Element -> Maybe (Zipper TimeStep)
readTimeStep zipper e = do
  let readInt = readMay @Text @Int
  -- everything except grace notes have durations
  duration <- e ^? deep (el "duration" . text) . to readInt . _Just
  case e ^? name of
    Just "note" ->
      case e ^? deep (el "rest") of
        Just _ -> Just $ pushN duration Rest zipper
        Nothing ->
          case xmlPitch e of
            Nothing -> Nothing
            Just pitch ->
              let newNote = Single $ N pitch (xmlConstraint e)
               in case e ^? deep (el "chord") of
                    Just _ -> Just $ modifyN duration (<> newNote) (leftN duration zipper)
                    Nothing -> case e ^? deep (el "voice") . text . to readInt . _Just of
                      Just n | n > 1 -> Just $ modifyN duration (newNote <>) zipper
                      _ -> Just $ pushN duration newNote zipper
    Just "backup" -> Just $ leftN duration zipper
    Just "forward" -> Just $ rightN duration zipper

--------------------------------------------------------------------------------
-- repl utils
--------------------------------------------------------------------------------

readXML :: FilePath -> IO Document
readXML = Text.XML.readFile def

prok = readXML "/home/joseph/Documents/MuseScore3/Scores/Prokofiev_violin_concerto_No_2_excerpt.musicxml"

brahms = readXML "/home/joseph/Documents/MuseScore3/Scores/Brahms_violin_concerto.musicxml"

sibelius = readXML "/home/joseph/Documents/MuseScore3/Scores/Sibelius_violin_concerto_excerpt_no_grace.musicxml"
