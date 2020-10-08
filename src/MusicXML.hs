module MusicXML
  ( separateFingerings,
    mergeFingerings,
    adjustMeasures,
    measureNumbers,
    readTimeSteps,
  )
where

import ClassyPrelude hiding (Element)
import Control.Category ((>>>))
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.ST
import Data.Text.Lens (unpacked)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
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

timeStep :: Traversal' Element Element
timeStep = deep (failing (el "note") (failing (el "backup") (el "forward")))

readTimeSteps :: Document -> V.Vector TimeStep
readTimeSteps doc = V.create do
  vec <- VM.replicate (totalDuration doc) Rest
  foldM_ (readTimeStep vec) 0 (doc ^.. root . timeStep)
  pure vec

readTimeStep :: VM.MVector s TimeStep -> Int -> Element -> ST s Int
readTimeStep vec t e =
  case e ^?! name of
    "note" -> do
      let t' = maybe t (const (t - duration)) (e ^? deep (el "chord"))
      forM_ [t' .. t' + duration - 1] $
        VM.modify
          vec
          (maybe Rest (\pitch -> Single (N pitch (xmlConstraint e))) (xmlPitch e) <>)
      pure (t' + duration)
    "backup" -> pure (t - duration)
    "forward" -> pure (t + duration)
    n -> error $ "Impossible timestep element " <> show n
  where
    duration = e ^?! dur

totalDuration :: Document -> Int
totalDuration = sumOf (root . timeStep . to fn)
  where
    fn e = case e ^?! name of
      "note" -> maybe (e ^?! dur) (const 0) (e ^? deep (el "chord"))
      "backup" -> negate (e ^?! dur)
      "forward" -> e ^?! dur
      n -> error $ "Impossible timestep element " <> show n

dur :: Fold Element Int
dur = deep (el "duration") . text . to readInt . _Just

readInt :: Text -> Maybe Int
readInt = readMay

--------------------------------------------------------------------------------
-- repl utils
--------------------------------------------------------------------------------

readXML :: FilePath -> IO Document
readXML = Text.XML.readFile def

prok, brahms, sibelius :: IO Document
prok = readXML "/home/joseph/Documents/MuseScore3/Scores/Prokofiev_violin_concerto_No_2_excerpt.musicxml"
brahms = readXML "/home/joseph/Documents/MuseScore3/Scores/Brahms_violin_concerto.musicxml"
sibelius = readXML "/home/joseph/Documents/MuseScore3/Scores/Sibelius_violin_concerto_excerpt_no_grace.musicxml"
