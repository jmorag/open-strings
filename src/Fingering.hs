{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Fingering where

import ClassyPrelude hiding (Element)
import Control.Comonad.Store
import Control.Lens
import Control.Lens.Internal.Context
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.XML.Lens

data Finger = Open | One | Two | Three | Four
  deriving (Show, Eq, Enum, Ord)

data VString = E | A | D | G
  deriving (Show, Eq, Enum, Ord)

-- Midi pitch
type Pitch = Int

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

data Constraint = Free | OnString VString | Finger Finger | Specified Finger VString
  deriving (Show, Eq)

type XmlRef = Pretext' (->) Element Document

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
        (Just f', Just s') -> Specified f' s'

data Fingering = Fingering {_string :: VString, _finger :: Finger, _distance :: Int}
  deriving (Eq, Show, Ord)

$(makeLenses ''Fingering)

data Note f = Note {_xmlRef :: XmlRef, _fingerings :: f Fingering}

$(makeLenses ''Note)

type UnassignedNote = Note Set

type AssignedNote = Note Identity

instance Eq (Note f) where
  n == m = getNote n == getNote m

instance Show (Note f) where
  show note =
    debugPitch (pitch note) <> case (constraint note) of
      Free -> ""
      OnString s -> showStr s
      Finger f -> showFinger f
      Specified f s -> showFinger f <> showStr s
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

getNote :: Note f -> Element
getNote = pos . _xmlRef

pitch :: Note f -> Pitch
pitch =
  fromMaybe (error "Called pitch on pitchless xml element") . (xmlPitch . getNote)

constraint :: Note f -> Constraint
constraint = xmlConstraint . getNote

-- | At each time step, there is either a rest, or 1, 2, 3, or 4 notes that must be
-- covered by the left hand
data TimeStep f
  = Single (Note f)
  | DoubleStop (Note f) (Note f)
  | TripleStop (Note f) (Note f) (Note f)
  | QuadrupleStop (Note f) (Note f) (Note f) (Note f)
  | Rest
  deriving (Show, Eq)

double :: Note f -> Note f -> TimeStep f
double n1 n2 = let [n1', n2'] = sortOn pitch [n1, n2] in DoubleStop n1' n2'

triple :: Note f -> Note f -> Note f -> TimeStep f
triple n1 n2 n3 =
  let [n1', n2', n3'] = sortOn pitch [n1, n2, n3]
   in TripleStop n1' n2' n3'

quad :: Note f -> Note f -> Note f -> Note f -> TimeStep f
quad n1 n2 n3 n4 =
  let [n1', n2', n3', n4'] = sortOn pitch [n1, n2, n3, n4]
   in QuadrupleStop n1' n2' n3' n4'

instance Semigroup (TimeStep f) where
  Single n1 <> Single n2 = double n1 n2
  Single n1 <> DoubleStop n2 n3 = triple n1 n2 n3
  Single n1 <> TripleStop n2 n3 n4 = quad n1 n2 n3 n4
  DoubleStop n1 n2 <> Single n3 = triple n1 n2 n3
  TripleStop n1 n2 n3 <> Single n4 = quad n1 n2 n3 n4
  DoubleStop n1 n2 <> DoubleStop n3 n4 = quad n1 n2 n3 n4
  n <> Rest = n
  Rest <> n = n
  n1 <> n2 = error $ "Unsatisfiably large constraint - too many notes to cover at one time: " <> show n1 <> " | " <> show n2

instance Monoid (TimeStep f) where
  mempty = Rest

data Step f = Step {_notes :: TimeStep f, _duration :: Int}
  deriving (Show, Eq)

$(makeLenses ''Step)

type UnassignedStep = Step Set

type AssignedStep = Step Identity

coalesceTimeSteps :: Vector (TimeStep f) -> [Step f]
coalesceTimeSteps = fmap (\ts -> Step (NE.head ts) (length ts)) . NE.group

-- | C4
middleC :: Pitch
middleC = 60

measurementSeries :: Vector Int
measurementSeries =
  V.fromList
    -- Assuming e string
    [ 0, -- e
      13, -- f
      30, -- f#
      45, -- g
      61, -- g#
      87, -- a
      101, -- a#
      114, -- b
      129, -- c
      138, -- c#
      147, -- d
      156, -- d#
      168, -- e
      180, -- f
      190, -- f#
      195, -- g
      202, -- g#
      209, -- a
      213, -- a#
      222, -- b
      228, -- c
      235, -- c#
      241, -- d
      245, -- d#
      251, -- e
      256, -- f
      260, -- f#
      262, -- g
      268, -- g#
      271 -- a
    ]

-- Ranges of the pitches playable on each string
gPitches, dPitches, aPitches, ePitches :: (Pitch, Pitch)
gPitches = (55, 84)
dPitches = (62, 91)
aPitches = (69, 98)
ePitches = (76, 105)

allFingerings :: Pitch -> [Fingering]
allFingerings p = do
  (str, (low, _high)) <- zip [E, A, D, G] [ePitches, aPitches, dPitches, gPitches]
  case measurementSeries V.!? (p - low) of
    Nothing -> []
    Just dist -> map (flip (Fingering str) dist) case p - low of
      -- At the low and high ends of the string, we can't use certain fingers
      -- TODO: decide high end
      0 -> [Open]
      1 -> [One]
      2 -> [One, Two]
      3 -> [One, Two]
      4 -> [One, Two]
      5 -> [One, Two, Three]
      _ -> [One, Two, Three, Four]

validPlacement :: Fingering -> Constraint -> Bool
validPlacement Fingering {..} = \case
  Free -> True
  OnString s -> s == _string
  Finger f -> f == _finger
  Specified f s -> f == _finger && s == _string

mkNote :: XmlRef -> Maybe UnassignedNote
mkNote ref =
  xmlPitch (pos ref) <&> \p ->
    let fs =
          S.fromList $
            filter (flip validPlacement (xmlConstraint (pos ref))) (allFingerings p)
     in Note ref fs

-- The cartesian product of all the possibleFingerings for a given timestep
allAssignments :: Step Set -> [Step Identity]
allAssignments (Step ns dur) = map (flip Step dur) (go ns)
  where
    go :: TimeStep Set -> [TimeStep Identity]
    go = \case
      Rest -> [Rest]
      Single (Note x fs) -> fmap (\f -> Single (Note x (Identity f))) (S.toList fs)
      DoubleStop (Note x1 fs1) (Note x2 fs2) -> do
        f1 <- S.toList fs1
        f2 <- S.toList fs2
        pure $ DoubleStop (Note x1 (Identity f1)) (Note x2 (Identity f2))
      TripleStop (Note x1 fs1) (Note x2 fs2) (Note x3 fs3) -> do
        f1 <- S.toList fs1
        f2 <- S.toList fs2
        f3 <- S.toList fs3
        pure $
          TripleStop
            (Note x1 (Identity f1))
            (Note x2 (Identity f2))
            (Note x3 (Identity f3))
      QuadrupleStop (Note x1 fs1) (Note x2 fs2) (Note x3 fs3) (Note x4 fs4) -> do
        f1 <- S.toList fs1
        f2 <- S.toList fs2
        f3 <- S.toList fs3
        f4 <- S.toList fs4
        pure $
          QuadrupleStop
            (Note x1 (Identity f1))
            (Note x2 (Identity f2))
            (Note x3 (Identity f3))
            (Note x4 (Identity f4))

data Penalty1 = P1
  { _p1Name :: Text,
    _p1Cost :: AssignedStep -> Double,
    _p1Weight :: Double
  }

$(makeLenses ''Penalty1)

data Penalty2 = P2
  { _p2Name :: Text,
    _p2Cost :: AssignedStep -> AssignedStep -> Double,
    _p2Weight :: Double
  }

$(makeLenses ''Penalty2)

-- | maximum floating point representable
infinity, high, medium, low :: Double
infinity = 1.8e308
high = 1e6
medium = 1e3
low = 10

trill :: Penalty1
trill = P1 "trill" cost high
  where
    cost step = case step ^. notes of
      Rest -> 0
      Single (Note x (Identity f)) ->
        case pos x ^? deep (failing (el "trill-mark") (el "wavy-line")) of
          Just _ -> case f ^. finger of
            Open -> high
            One -> 0
            Two -> 0
            Three -> if step ^. duration >= 4 then infinity else high
            Four -> infinity
          Nothing -> 0
      DoubleStop n1 n2 ->
        cost (set notes (Single n1) step) + cost (set notes (Single n2) step)
      -- there should never be trills on triple/quadruple stops...
      _ -> 0

doubleStopAdjacent :: Penalty1
doubleStopAdjacent = P1 "double stops on adjacent strings" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2 ->
        let f1 = n1 ^. fingerings . _Wrapped
            f2 = n2 ^. fingerings . _Wrapped
            adjacent = f2 ^. string . from enum - f1 ^. string . from enum == 1
         in if adjacent then 0 else infinity
      _ -> 0

staticThird :: Penalty1
staticThird = P1 "static third" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) `elem` [3, 4] ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (Three, One) -> 0
                (Four, Two) -> 0
                (_, Open) -> 0
                (Open, _) -> 0
                (Four, One)
                  | f1 ^. distance <= 101 && f2 ^. distance <= 45 -> low
                  | otherwise -> medium
                _ -> infinity
      _ -> 0

staticSixth :: Penalty1
staticSixth = P1 "static sixth" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) `elem` [8, 9] ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Two) -> 0
                (Two, Three) -> 0
                (Three, Four) -> 0
                (One, Three) -> low
                (Two, Four) -> low
                (_, Open) -> low
                (Open, _) -> low
                _ -> infinity
      _ -> 0

staticOctave :: Penalty1
staticOctave = P1 "static octave" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) == 12 ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Four) -> 0
                (Open, _) -> 0
                -- TODO make these more interesting
                (One, Three) -> if f2 ^. distance >= 147 then 0 else low
                (Two, Four) -> if f2 ^. distance >= 147 then 0 else low
                _ -> infinity
      _ -> 0

staticTenth :: Penalty1
staticTenth = P1 "static tenth (or any interval greater than an octave)" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) > 12 ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Four) -> 0
                (Open, _) -> 0
                _ -> infinity
      _ -> 0

staticUnison :: Penalty1
staticUnison = P1 "static unison" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | pitch n2 == pitch n1 ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in -- Make sure that f1 refers to the lower string
              case (min f1 f2 ^. finger, max f1 f2 ^. finger) of
                (Four, One) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticSecond :: Penalty1
staticSecond = P1 "static second" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) `elem` [1, 2] ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (Four, One) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticFourth :: Penalty1
staticFourth = P1 "static fourth" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) `elem` [5, 6] ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (Two, One) -> 0
                (Three, Two) -> 0
                (Four, Three) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticFifth :: Penalty1
staticFifth = P1 "static fifth" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) == 7 ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (One, One) -> 0
                (Two, Two) -> 0
                (Three, Three) -> 0
                (Four, Four) -> low
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticSeventh :: Penalty1
staticSeventh = P1 "static seventh" cost high
  where
    cost step = case step ^. notes of
      DoubleStop n1 n2
        | (pitch n2 - pitch n1) `elem` [10, 11] ->
          let f1 = n1 ^. fingerings . _Wrapped
              f2 = n2 ^. fingerings . _Wrapped
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Three) -> 0
                (Two, Four) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0
