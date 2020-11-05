module MusicXML
  ( adjustMeasures,
    measureNumbers,
    readTimeSteps,
    inferFingerings,
  )
where

import ClassyPrelude hiding (Element)
import Control.Comonad.Store
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.ST
import Data.Foldable (foldr1)
import qualified Data.List.NonEmpty as NE
import Data.Text.Lens (unpacked)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Fingering
import Text.XML
import Text.XML.Lens

inferFingerings :: Document -> Document
inferFingerings doc =
  case infer (coalesceTimeSteps (readTimeSteps doc)) ^.. traversed . assignedNotes of
    [] -> doc
    ns -> peeks id . view xmlRef $ foldr1 go ns
  where
    go :: AssignedNote -> AssignedNote -> AssignedNote
    go note = over xmlRef (seeks (setFingering (note ^. fingerings . _Wrapped)))

assignedNotes :: Fold AssignedStep AssignedNote
assignedNotes = folding \step -> case _notes step of
  Rest -> []
  Single n -> [n]
  DoubleStop n1 n2 -> [n1, n2]
  TripleStop n1 n2 n3 -> [n1, n2, n3]
  QuadrupleStop n1 n2 n3 n4 -> [n1, n2, n3, n4]

setFingering :: Fingering -> Element -> Element
setFingering fingering noteEl =
  let e nm children = _Element # Element nm mempty children
      get nm parent = parent ^? plate . el nm
      fingerNum = case fingering ^. finger of One -> "1"; Two -> "2"; Three -> "3"; Four -> "4"; Open -> "0"
      fingerEl = e "f" [_Content # fingerNum]
      stringNum = case fingering ^. string of E -> "1"; A -> "2"; D -> "3"; G -> "4"
      stringEl = e "string" [_Content # stringNum]
      notationsEl = case get "notations" noteEl of
        Nothing ->
          e "notations" [e "technical" [fingerEl, stringEl]]
            ^?! _Element
        Just notations ->
          case get "technical" notations of
            Nothing -> notations & nodes %~ ((:) ((e "technical" [fingerEl, stringEl])))
            Just technical ->
              case (get "fingering" technical, get "string" technical) of
                (Nothing, Nothing) ->
                  notations & plate . el "technical" . nodes %~ ([fingerEl, stringEl] ++)
                (Nothing, Just _) ->
                  notations & plate . el "technical" . nodes %~ ((:) fingerEl)
                    & deep (el "string") . text .~ stringNum
                (Just _, Nothing) ->
                  notations & plate . el "technical" . nodes %~ ((:) stringEl)
                    & deep (el "fingering") . text .~ fingerNum
                (Just _, Just _) ->
                  notations & deep (el "fingering") . text .~ fingerNum
                    & deep (el "string") . text .~ stringNum
   in noteEl & plate . (el "notations") .~ notationsEl

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

readTimeSteps :: Document -> Vector (TimeStep Set)
readTimeSteps doc = V.create do
  vec <- VM.replicate (totalDuration doc) Rest
  foldM_ (readTimeStep vec) 0 (holesOf (root . timeStep) doc)
  pure vec

readTimeStep :: VM.MVector s (TimeStep Set) -> Int -> XmlRef -> ST s Int
readTimeStep vec t ref =
  case e ^?! name of
    "note" -> do
      let t' = maybe t (const (t - duration)) (e ^? deep (el "chord"))
      forM_ [t' .. t' + duration - 1] $
        VM.modify vec (maybe Rest Single (mkNote ref) <>)
      pure (t' + duration)
    "backup" -> pure (t - duration)
    "forward" -> pure (t + duration)
    n -> error $ "Impossible timestep element " <> show n
  where
    e = pos ref
    duration = e ^?! dur

coalesceTimeSteps :: Vector (TimeStep f) -> [Step f]
coalesceTimeSteps = fmap (\ts -> Step (NE.head ts) (length ts)) . NE.group

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
