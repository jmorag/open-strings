module MusicXML (
  adjustMeasures,
  measureNumbers,
  readTimeSteps,
  inferFingerings,
  inferWeights,
  timeStep,
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

inferFingerings :: Document -> Weights Double -> (Double, Document)
inferFingerings doc weights =
  doc
    & partsOf' (root . timeStep . attributeIs "data-selected" "") %%~ go
  where
    go :: [Element] -> (Double, [Element])
    go steps =
      let (cost, assignedSteps) = inferFingerings' weights (readTimeSteps steps)
          assignedNotes =
            ordNubBy (view (xmlRef . _1)) (==) (assignedSteps ^.. traversed . notes)
          fingers =
            map (\n -> (n ^. xmlRef . _1, n ^. fingering)) assignedNotes
          steps' = assignFingers (zip [0 ..] steps) fingers
       in (cost, steps' & traversed . attribute "data-selected" .~ Nothing)

inferWeights :: Document -> Weights Double -> Either Text (Weights Double)
inferWeights doc initialWeights = go (doc ^.. root . timeStep)
  where
    go :: [Element] -> Either Text (Weights Double)
    go steps = case traverse getSingleAnnotation $ readTimeSteps steps of
      Nothing ->
        Left "Cannot deduce preferences. In order to infer weights, all fingers and strings must be specified."
      Just assignedSteps -> Right $ inferWeights' [assignedSteps] initialWeights

    getSingleAnnotation :: UnassignedStep -> Maybe (AssignedStep)
    getSingleAnnotation s = case allAssignments s of
      [ann] -> Just ann
      _ -> Nothing


assignFingers :: [(Int, Element)] -> [(Int, Fingering)] -> [Element]
assignFingers e [] = map snd e
assignFingers ((i, e) : es) ((j, f) : fs) =
  if i == j
    then setFingering f e : assignFingers es fs
    else e : assignFingers es ((j, f) : fs)
assignFingers [] _ = error "Length of fingerings exceeds length of note elements"

-- Unlawful lens to get immediate child of an xml element and
-- create a new one if the target doesn't exist
child :: Text -> Lens' Element Element
child nm = lens getter setter
  where
    getter e = fromMaybe (Element (Name nm Nothing Nothing) mempty []) $ e ^? plate . ell nm
    setter e new = case e ^? plate . ell nm of
      Just _ -> set (singular (plate . ell nm)) new e
      Nothing -> over nodes ((_Element # new) :) e

setFingering :: Fingering -> Element -> Element
setFingering fing noteEl =
  let e nm cs = Element nm mempty cs
      fingerNum = case fing ^. finger of
        One -> "1"
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Open -> "0"
      fingerEl = e "fingering" [_Content # fingerNum]
      stringNum = case fing ^. string of E -> "1"; A -> "2"; D -> "3"; G -> "4"
      stringEl = e "string" [_Content # stringNum]
   in noteEl & child "notations" . child "technical" . child "fingering" .~ fingerEl
        & child "notations" . child "technical" . child "string" .~ stringEl

timeStep :: Traversal' Element Element
timeStep = deep $ ell "note" `failing` ell "backup" `failing` ell "forward"

readTimeSteps :: [Element] -> [Step Set]
readTimeSteps es = addGraceNotes graceNotes noGrace
  where
    es' = zip [0 ..] es
    (len, graceNotes) = totalDuration es'
    noGrace = coalesceTimeSteps $ V.create do
      vec <- VM.replicate len Rest
      foldM_ (readTimeStep vec) 0 es'
      pure vec

readTimeStep :: VM.MVector s (TimeStep Set) -> Int -> XmlRef -> ST s Int
readTimeStep vec t ref =
  let t' = if chord e then t - dur e else t
   in case e ^?! name of
        "note" -> do
          -- skip grace notes in this phase
          unless (grace e) $
            forM_ [t' .. t' + dur e - 1] $ VM.modify vec (mkNote ref <>)
          pure (t' + dur e)
        "backup" -> pure (t' - dur e)
        "forward" -> pure (t' + dur e)
        n -> error $ "Impossible timestep element " <> show n
  where
    e = deref ref

coalesceTimeSteps :: Foldable t => t (TimeStep f) -> [Step f]
coalesceTimeSteps = fmap (\ts -> Step (NE.head ts) (length ts)) . NE.group

addGraceNotes :: [(TimeStep Set, Int)] -> [Step Set] -> [Step Set]
addGraceNotes = go 0 []
  where
    go _ acc [] regularNotes = reverse acc <> regularNotes
    go _ acc graceNotes [] = reverse acc <> (map (\(g, _) -> Step g 1) graceNotes)
    go t acc graceNotes@((g, t') : gs) regularNotes@(n : ns)
      -- Assumes that grace notes line up exactly before regular
      -- notes. This should of course be true but it could bear some
      -- stress testing.
      | t == t' = go t (Step g 1 : acc) gs regularNotes
      | otherwise = go (t + _duration n) (n : acc) graceNotes ns

totalDuration :: [XmlRef] -> (Int, [(TimeStep Set, Int)])
totalDuration = go 0 []
  where
    go t acc [] = (t, reverse acc)
    go t acc (ref : es) = case grace e of
      False -> go (t + calculateDuration e) acc es
      True -> case chord e of
        False -> go t ((mkNote ref, t) : acc) es
        True -> case acc of
          [] -> error "Impossible - chord element before non-chord element"
          ((n, t') : ns) -> go t ((mkNote ref <> n, t') : ns) es
      where
        e = deref ref

calculateDuration :: Element -> Int
calculateDuration e = case e ^?! name of
  "note" -> if chord e then 0 else dur e
  "backup" -> negate (dur e)
  "forward" -> dur e
  n -> error $ "Impossible timestep element " <> show n

chord, grace :: Element -> Bool
chord = has (deep (ell "chord"))
grace = has (deep (ell "grace"))

dur :: Element -> Int
dur e =
  if grace e
    then 0
    else
      fromMaybe (error "Duration element missing from non-grace note") $
        e ^? deep (ell "duration") . text . to readMay . _Just

-- | Shift measure numbers to start at the given point
adjustMeasures :: Int -> Document -> Document
adjustMeasures beg = iset (root . measureNumbers) (+ beg)

measureNumbers :: IndexedTraversal' Int Element Int
measureNumbers =
  indexing $
    deep (ell "measure")
      . filtered (\measure -> measure ^? attr "implicit" /= Just "yes")
      . attr "number"
      . unpacked
      . _Show
