{-# LANGUAGE ApplicativeDo #-}

module Fingering where

import ClassyPrelude hiding (Element)
import Control.Lens
import Control.Lens.Internal.Context
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.XML.Lens

data Finger = Open | One | Two | Three | Four
  deriving (Show, Eq, Enum, Ord)

data VString = E | A | D | G
  deriving (Show, Eq, Enum, Ord)

-- Midi pitch
type Pitch = Int

data Fingering = F Finger VString
  deriving (Show, Eq)

data Constraint = Free | OnString VString | Finger Finger | Fingering Fingering
  deriving (Show, Eq)

type XmlRef = Pretext' (->) Element Document

data Note = N Pitch Constraint XmlRef

pitch :: Note -> Pitch
pitch (N p _ _) = p

instance Show Note where
  show (N p c _) =
    debugPitch p <> case c of
      Free -> ""
      OnString s -> showStr s
      Finger f -> showFinger f
      Fingering (F f s) -> showFinger f <> showStr s
    where
      showStr E = "-I"
      showStr A = "-II"
      showStr D = "-III"
      showStr G = "-IV"
      showFinger Open = "-0"
      showFinger One = "-1"
      showFinger Two = "-2"
      showFinger Three = "-3"
      showFinger Four = "-4"

-- | At each time step, there is either a rest, or 1, 2, 3, or 4 notes that must be
-- covered by the left hand
data TimeStep
  = Single Note
  | DoubleStop Note Note
  | TripleStop Note Note Note
  | QuadrupleStop Note Note Note Note
  | Rest
  deriving (Show)

instance Semigroup TimeStep where
  Single n1 <> Single n2 =
    let [n1', n2'] = sortOn pitch [n1, n2] in DoubleStop n1' n2'
  Single n1 <> DoubleStop n2 n3 =
    let [n1', n2', n3'] = sortOn pitch [n1, n2, n3]
     in TripleStop n1' n2' n3'
  Single n1 <> TripleStop n2 n3 n4 =
    let [n1', n2', n3', n4'] = sortOn pitch [n1, n2, n3, n4]
     in QuadrupleStop n1' n2' n3' n4'
  DoubleStop n1 n2 <> Single n3 =
    let [n1', n2', n3'] = sortOn pitch [n1, n2, n3]
     in TripleStop n1' n2' n3'
  TripleStop n1 n2 n3 <> Single n4 =
    let [n1', n2', n3', n4'] = sortOn pitch [n1, n2, n3, n4]
     in QuadrupleStop n1' n2' n3' n4'
  DoubleStop n1 n2 <> DoubleStop n3 n4 =
    let [n1', n2', n3', n4'] = sortOn pitch [n1, n2, n3, n4]
     in QuadrupleStop n1' n2' n3' n4'
  n <> Rest = n
  Rest <> n = n
  n1 <> n2 = error $ "Unsatisfiably large constraint - too many notes to cover at one time: " <> show n1 <> " | " <> show n2

instance Monoid TimeStep where
  mempty = Rest

-- | C4
middleC :: Pitch
middleC = 60

debugPitch :: Pitch -> String
debugPitch p =
  case p `mod` 12 of
    0 -> "C"
    1 -> "C#"
    2 -> "D"
    3 -> "D#"
    4 -> "E"
    5 -> "F"
    6 -> "F#"
    7 -> "G"
    8 -> "G#"
    9 -> "A"
    10 -> "A#"
    11 -> "B"
    _ -> error "impossible"
    <> show (p `div` 12 - 1)

-- | Get the MIDI pitch number from the xml note element
xmlPitch :: Element -> Maybe Pitch
xmlPitch note = do
  pitchEl <- note ^? deep (el "pitch")
  step <-
    pitchEl ^? deep (el "step") . text >>= \case
      "C" -> Just 12
      "D" -> Just 14
      "E" -> Just 16
      "F" -> Just 17
      "G" -> Just 19
      "A" -> Just 21
      "B" -> Just 23
      _ -> Nothing
  alter <- (pitchEl ^? deep (el "alter") . text >>= readMay) <|> pure 0
  octave <- pitchEl ^? deep (el "octave") . text >>= readMay
  pure $ step + alter + (12 * octave)

-- | Read a fingering constraint from a note element
xmlConstraint :: Element -> Constraint
xmlConstraint note =
  let f = case note ^? deep (el "fingering") . text of
        Just "0" -> Just Open
        Just "1" -> Just One
        Just "2" -> Just Two
        Just "3" -> Just Three
        Just "4" -> Just Four
        _ -> Nothing
      s = case note ^? deep (el "string") . text of
        Just "1" -> Just E
        Just "2" -> Just A
        Just "3" -> Just D
        Just "4" -> Just G
        _ -> Nothing
   in case (f, s) of
        (Nothing, Nothing) -> Free
        (Just f', Nothing) -> Finger f'
        (Nothing, Just s') -> OnString s'
        (Just f', Just s') -> Fingering (F f' s')

data Location
  = -- | mm from nut
    L VString Finger Int
  deriving (Eq, Show, Ord)

-- type Node = (Pitch, S.Set Location)

measurementSeries :: V.Vector Int
measurementSeries =
  V.fromList
    [0, 13, 30, 45, 61, 87, 101, 114, 129, 138, 147, 156, 168, 180, 190, 195, 202, 209, 213, 222, 228, 235, 241, 245, 251, 256, 260, 262, 268, 271]

-- Ranges of the pitches playable on each string
gPitches, dPitches, aPitches, ePitches :: (Pitch, Pitch)
gPitches = (55, 84)
dPitches = (62, 91)
aPitches = (69, 98)
ePitches = (76, 105)

allLocations :: Pitch -> [Location]
allLocations pitch = do
  (str, (low, _)) <- zip [E, A, D, G] [ePitches, aPitches, dPitches, gPitches]
  case measurementSeries V.!? (pitch - low) of
    Nothing -> []
    Just dist -> map (flip (L str) dist) case pitch - low of
      -- At the low and high ends of the string, we can't use certain fingers
      -- TODO: decide high end
      0 -> [Open]
      1 -> [One]
      2 -> [One, Two]
      3 -> [One, Two]
      4 -> [One, Two]
      5 -> [One, Two, Three]
      _ -> [One, Two, Three, Four]

validPlacement :: Constraint -> Location -> Bool
validPlacement constraint (L str finger _) = case constraint of
  Free -> True
  OnString s -> s == str
  Finger f -> f == finger
  Fingering (F f s) -> f == finger && s == str

makeNode :: Note -> (Pitch, S.Set Location)
makeNode (N pitch constraint _) = (pitch, S.fromList locs)
  where
    locs = filter (validPlacement constraint) (allLocations pitch)