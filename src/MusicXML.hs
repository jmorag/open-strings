module MusicXML (
  adjustMeasures,
  measureNumbers,
  readTimeSteps,
  inferFingerings,
) where

import ClassyPrelude hiding (Element)
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.ST
import qualified Data.List.NonEmpty as NE
import Data.Text.Lens (unpacked)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Fingering
import Text.XML
import Text.XML.Lens

inferFingerings :: Document -> Weights -> Document
inferFingerings doc weights = over (partsOf' (root . timeStep)) go doc
  where
    go steps =
      let assignedSteps = infer weights $ coalesceTimeSteps (readTimeSteps steps)
          assignedNotes =
            ordNubBy (view (xmlRef . _1)) (==) (assignedSteps ^.. traversed . notes)
          fingers =
            map (\n -> (n ^. xmlRef . _1, n ^. fingerings')) assignedNotes
       in assignFingers (zip [0 ..] steps) fingers

assignFingers :: [(Int, Element)] -> [(Int, Fingering)] -> [Element]
assignFingers e [] = map snd e
assignFingers ((i, e) : es) ((j, f) : fs) =
  if i == j
    then setFingering f e : assignFingers es fs
    else e : assignFingers es ((j, f) : fs)
assignFingers [] _ = error "Length of fingerings exceeds length of note elements"

-- Unlawful lens to get immediate child of an xml element and
-- create a new one if the target doesn't exist
child :: Name -> Lens' Element Element
child nm = lens getter setter
  where
    getter e = fromMaybe (Element nm mempty []) $ e ^? plate . el nm
    setter e new = case e ^? plate . el nm of
      Just _ -> set (singular (plate . el nm)) new e
      Nothing -> over nodes ((_Element # new) :) e

setFingering :: Fingering -> Element -> Element
setFingering fingering noteEl =
  let e nm cs = Element nm mempty cs
      fingerNum = case fingering ^. finger of
        One -> "1"
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Open -> "0"
      fingerEl = e "fingering" [_Content # fingerNum]
      stringNum = case fingering ^. string of E -> "1"; A -> "2"; D -> "3"; G -> "4"
      stringEl = e "string" [_Content # stringNum]
   in noteEl & child "notations" . child "technical" . child "fingering" .~ fingerEl
        & child "notations" . child "technical" . child "string" .~ stringEl

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
timeStep = deep $ el "note" `failing` el "backup" `failing` el "forward"

readTimeSteps :: [Element] -> Vector (TimeStep Set)
readTimeSteps es = V.create do
  vec <- VM.replicate (totalDuration es) Rest
  foldM_ (readTimeStep vec) 0 (zip [0 ..] es)
  pure vec

readTimeStep :: VM.MVector s (TimeStep Set) -> Int -> XmlRef -> ST s Int
readTimeStep vec t ref =
  case e ^?! name of
    "note" -> do
      let t' = maybe t (const (t - xmlDuration)) (e ^? deep (el "chord"))
      forM_ [t' .. t' + xmlDuration - 1] $
        VM.modify vec (maybe Rest Single (mkNote ref) <>)
      pure (t' + xmlDuration)
    "backup" -> pure (t - xmlDuration)
    "forward" -> pure (t + xmlDuration)
    n -> error $ "Impossible timestep element " <> show n
  where
    e = deref ref
    xmlDuration = e ^?! dur

coalesceTimeSteps :: Vector (TimeStep f) -> [Step f]
coalesceTimeSteps = fmap (\ts -> Step (NE.head ts) (length ts)) . NE.group

totalDuration :: [Element] -> Int
totalDuration = sumOf (traversed . to fn)
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
