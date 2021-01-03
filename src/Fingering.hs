{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Fingering (
  XmlRef,
  deref,
  infer,
  Fingering (..),
  fingerings',
  xmlRef,
  finger,
  string,
  Finger (..),
  VString (..),
  TimeStep (..),
  Step (..),
  Note (..),
  AssignedNote,
  mkNote,
  notes,
  allAssignments,
) where

import ClassyPrelude hiding (Element, second)
import Control.Lens
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V
import Graph.ShortestPath
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

-- TODO figure out how to make Pretext' (Indexed Int) Element Document work
type XmlRef = (Int, Element)

deref :: XmlRef -> Element
deref = snd

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
  deriving (Eq, Ord)

instance Show Fingering where
  show (Fingering {..}) = showFinger _finger <> showStr _string
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

makeLenses ''Fingering

data Note f = Note {_xmlRef :: XmlRef, _fingerings :: f Fingering}

makeLenses ''Note

fingerings' :: Lens' AssignedNote Fingering
fingerings' = fingerings . _Wrapped

type UnassignedNote = Note Set

type AssignedNote = Note Identity

instance Eq (Note f) where
  n == m = getNote n == getNote m

deriving instance Ord (Note Identity)

instance Foldable f => Show (Note f) where
  show note =
    debugPitch (pitch note) <> "[" <> F.concatMap show (_fingerings note) <> "]"

getNote :: Note f -> Element
getNote = deref . _xmlRef

pitch :: Note f -> Pitch
pitch =
  fromMaybe (error "Called pitch on pitchless xml element") . (xmlPitch . getNote)

-- | At each time step, there is either a rest, or 1, 2, 3, or 4 notes that must be
-- covered by the left hand
data TimeStep f
  = Single (Note f)
  | DoubleStop (Note f) (Note f)
  | TripleStop (Note f) (Note f) (Note f)
  | QuadrupleStop (Note f) (Note f) (Note f) (Note f)
  | Rest
  deriving (Show, Eq)

deriving instance Ord (TimeStep Identity)

makePrisms ''TimeStep

notes :: Fold (Step f) (Note f)
notes = folding \step -> case _timestep step of
  Rest -> []
  Single n -> [n]
  DoubleStop n1 n2 -> [n1, n2]
  TripleStop n1 n2 n3 -> [n1, n2, n3]
  QuadrupleStop n1 n2 n3 n4 -> [n1, n2, n3, n4]

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

instance (Foldable f) => Semigroup (TimeStep f) where
  Single n1 <> Single n2 = double n1 n2
  Single n1 <> DoubleStop n2 n3 = triple n1 n2 n3
  Single n1 <> TripleStop n2 n3 n4 = quad n1 n2 n3 n4
  DoubleStop n1 n2 <> Single n3 = triple n1 n2 n3
  TripleStop n1 n2 n3 <> Single n4 = quad n1 n2 n3 n4
  DoubleStop n1 n2 <> DoubleStop n3 n4 = quad n1 n2 n3 n4
  n <> Rest = n
  Rest <> n = n
  n1 <> n2 =
    error $
      "Unsatisfiably large constraint - too many notes to cover at one time: "
        <> show n1
        <> " | "
        <> show n2

instance (Foldable f) => Monoid (TimeStep f) where
  mempty = Rest

data Step f = Step {_timestep :: TimeStep f, _duration :: Int}
  deriving (Show, Eq)

deriving instance Ord (Step Identity)

makeLenses ''Step

type UnassignedStep = Step Set

type AssignedStep = Step Identity

-- | C4
middleC :: Pitch
middleC = 60

measurementSeries :: Vector Int
measurementSeries =
  V.fromList
    -- Assuming e string
    [ 0 -- e
    , 13 -- f
    , 30 -- f#
    , 45 -- g
    , 61 -- g#
    , 87 -- a
    , 101 -- a#
    , 114 -- b
    , 129 -- c
    , 138 -- c#
    , 147 -- d
    , 156 -- d#
    , 168 -- e
    , 180 -- f
    , 190 -- f#
    , 195 -- g
    , 202 -- g#
    , 209 -- a
    , 213 -- a#
    , 222 -- b
    , 228 -- c
    , 235 -- c#
    , 241 -- d
    , 245 -- d#
    , 251 -- e
    , 256 -- f
    , 260 -- f#
    , 262 -- g
    , 268 -- g#
    , 271 -- a
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
      4 -> [One, Two, Three]
      5 -> [One, Two, Three]
      _ -> [One, Two, Three, Four]

validPlacement :: Fingering -> Constraint -> Bool
validPlacement Fingering {..} constraint = case constraint of
  Free -> True
  OnString s -> s == _string
  Finger f -> f == _finger
  Specified f s -> f == _finger && s == _string

data Penalty a = P
  { _pName :: Text
  , _pCost :: a -> Double
  , _pWeight :: Double
  }

makeLenses ''Penalty

type Penalty1 = Penalty AssignedStep

type Penalty2 = Penalty (AssignedStep, AssignedStep)

mkNote :: XmlRef -> Maybe UnassignedNote
mkNote ref =
  xmlPitch (deref ref) <&> \p ->
    let fs =
          S.fromList $
            filter
              (flip validPlacement (xmlConstraint (deref ref)))
              (allFingerings p)
     in Note ref fs

-- The cartesian product of all the possibleFingerings for a given timestep
allAssignments :: UnassignedStep -> [AssignedStep]
allAssignments (Step ns dur) = map (flip Step dur) (go ns)
  where
    staticCost step =
      sumOf
        (traversed . to (\p -> ((p ^. pCost) step) * p ^. pWeight))
        p1s
    note x f = Note x (Identity f)
    ret step = step <$ guard (staticCost (Step step dur) < infinity)
    go :: TimeStep Set -> [TimeStep Identity]
    go = \case
      Rest -> [Rest]
      Single (Note x fs) -> fmap (\f -> Single (note x f)) (S.toList fs)
      DoubleStop (Note x1 fs1) (Note x2 fs2) -> do
        f1 <- S.toList fs1
        f2 <- S.toList fs2
        ret $ DoubleStop (note x1 f1) (note x2 f2)
      TripleStop (Note x1 fs1) (Note x2 fs2) (Note x3 fs3) -> do
        f1 <- S.toList fs1
        f2 <- S.toList fs2
        f3 <- S.toList fs3
        ret $ TripleStop (note x1 f1) (note x2 f2) (note x3 f3)
      QuadrupleStop (Note x1 fs1) (Note x2 fs2) (Note x3 fs3) (Note x4 fs4) -> do
        f1 <- S.toList fs1
        f2 <- S.toList fs2
        f3 <- S.toList fs3
        f4 <- S.toList fs4
        ret $ QuadrupleStop (note x1 f1) (note x2 f2) (note x3 f3) (note x4 f4)

--------------------------------------------------------------------------------
-- Intervals
--------------------------------------------------------------------------------
_p1
  , _p4
  , _p5
  , _p8
  , _m2
  , _M2
  , _m3
  , _M3
  , _a4
  , _m6
  , _M6
  , _m7
  , _M7
  , _m9
  , second
  , third
  , sixth
  , seventh ::
    Note f -> Note f -> Bool
halfSteps :: Note f -> Note f -> Int
halfSteps p1 p2 = abs (pitch p2 - pitch p1)
_p1 p1 p2 = halfSteps p1 p2 == 0
_p4 p1 p2 = halfSteps p1 p2 == 5
_p5 p1 p2 = halfSteps p1 p2 == 7
_p8 p1 p2 = halfSteps p1 p2 == 12
_m2 p1 p2 = halfSteps p1 p2 == 1
_M2 p1 p2 = halfSteps p1 p2 == 2
_m3 p1 p2 = halfSteps p1 p2 == 3
_M3 p1 p2 = halfSteps p1 p2 == 4
_a4 p1 p2 = halfSteps p1 p2 == 6
_m6 p1 p2 = halfSteps p1 p2 == 8
_M6 p1 p2 = halfSteps p1 p2 == 9
_m7 p1 p2 = halfSteps p1 p2 == 10
_M7 p1 p2 = halfSteps p1 p2 == 11
-- minor 9th or higher
_m9 p1 p2 = halfSteps p1 p2 > 12
second p1 p2 = _m2 p1 p2 || _M2 p1 p2
third p1 p2 = _m3 p1 p2 || _M3 p1 p2
sixth p1 p2 = _m6 p1 p2 || _M6 p1 p2
seventh p1 p2 = _m7 p1 p2 || _M7 p1 p2

-- TODO investigate some kind of TH macro to autodiscover these like hedgehog's discover
p1s :: [Penalty1]
p1s =
  [ trill
  , chordAdjacent
  , staticUnison
  , staticSecond
  , staticThird
  , staticFourth
  , staticFifth
  , staticSixth
  , staticSeventh
  , staticOctave
  , staticTenth
  , staticTripleStop
  , staticQuadrupleStop
  ]

p2s :: [Penalty2]
p2s = [oneFingerHalfStep]

infer :: [UnassignedStep] -> [AssignedStep]
infer steps = case sequence $ steps ^.. (traversed . to allAssignments . to NE.nonEmpty) of
  Nothing -> error "Could not assign fingering"
  Just steps' -> case shortestPath steps' singleCost transitionCost of
    (c, path) -> traceShow c path
    where
      singleCost s =
        let cost p = (p ^. pCost) s * p ^. pWeight
         in sumOf (traversed . to cost) p1s
      transitionCost s1 s2 =
        let cost p = (p ^. pCost) (s1, s2) * p ^. pWeight
         in sumOf (traversed . to cost) p2s

-- | maximum floating point representable
high, medium, low :: Double
high = 1e6
medium = 1e3
low = 10

trill :: Penalty1
trill = P "trill" cost high
  where
    cost step = case step ^. timestep of
      Rest -> 0
      Single (Note x (Identity f)) ->
        case deref x ^? deep (failing (el "trill-mark") (el "wavy-line")) of
          Just _ -> case f ^. finger of
            Open -> high
            One -> 0
            Two -> 0
            Three -> if step ^. duration >= 4 then infinity else high
            Four -> infinity
          Nothing -> 0
      DoubleStop n1 n2 ->
        cost (set timestep (Single n1) step) + cost (set timestep (Single n2) step)
      -- there should never be trills on triple/quadruple stops...
      _ -> 0

--------------------------------------------------------------------------------
-- Double Stops
--------------------------------------------------------------------------------
chordAdjacent :: Penalty1
chordAdjacent = P "chords on adjacent strings" cost high
  where
    cost step =
      case sort $ step ^.. notes . fingerings' . string of
        [] -> 0 -- rest
        [_] -> 0
        [s1, s2] -> if fromEnum s2 - fromEnum s1 == 1 then 0 else infinity
        [G, D, A] -> 0
        [D, A, E] -> 0
        [G, D, A, E] -> 0
        _ -> infinity

staticThird :: Penalty1
staticThird = P "static third" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | third n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (Three, One) -> 0
                (Four, Two) -> 0
                (_, Open) -> 0
                (Open, _) -> 0
                (Four, One)
                  | f1 ^. distance <= 101 && f2 ^. distance <= 45 -> low
                  | otherwise -> medium
                (Four, Three)
                  | f2 ^. distance - f1 ^. distance <= 40 -> low
                  | otherwise -> high
                (Three, Two)
                  | f2 ^. distance - f1 ^. distance <= 35 -> low
                  | otherwise -> high
                (Two, One)
                  | f2 ^. distance - f1 ^. distance <= 30 -> low
                  | otherwise -> high
                _ -> infinity
      _ -> 0

staticSixth :: Penalty1
staticSixth = P "static sixth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | sixth n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
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
staticOctave = P "static octave" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _p8 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Four) -> 0
                (Open, _) -> 0
                -- TODO make these more interesting
                (One, Three) -> if f2 ^. distance >= 147 then 0 else low
                (Two, Four) -> if f2 ^. distance >= 147 then 0 else low
                _ -> infinity
      _ -> 0

staticTenth :: Penalty1
staticTenth = P "static tenth (or any interval greater than an octave)" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _m9 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Four) -> 0
                (Open, _) -> 0
                _ -> infinity
      _ -> 0

staticUnison :: Penalty1
staticUnison = P "static unison" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _p1 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in -- Make sure that f1 refers to the lower string
              case (min f1 f2 ^. finger, max f1 f2 ^. finger) of
                (Four, One) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticSecond :: Penalty1
staticSecond = P "static second" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | second n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (Four, One) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticFourth :: Penalty1
staticFourth = P "static fourth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _p4 n1 n2 || _a4 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (Two, One) -> 0
                (Three, Two) -> 0
                (Four, Three) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

staticFifth :: Penalty1
staticFifth = P "static fifth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _p5 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
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
staticSeventh = P "static seventh" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | seventh n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Three) -> 0
                (Two, Four) -> 0
                (Open, _) -> 0
                (_, Open) -> 0
                _ -> infinity
      _ -> 0

--------------------------------------------------------------------------------
-- Triple/Quadruple Stops
--------------------------------------------------------------------------------

staticTripleStop :: Penalty1
staticTripleStop = P "static triple stop" cost high
  where
    cost step =
      case sortOn (view (fingerings' . string)) $
        step ^.. timestep . _TripleStop . each of
        ns@[n1, n2, n3] ->
          cost (set timestep (DoubleStop n1 n2) step)
            + cost (set timestep (DoubleStop n2 n3) step)
            + let [f1, _, f3] = ns ^.. traversed . fingerings' . finger
               in if f1 == f3 && f1 /= Open
                    then infinity
                    else 0
        _ -> 0

staticQuadrupleStop :: Penalty1
staticQuadrupleStop = P "static quadruple stop" cost high
  where
    cost step =
      case sortOn (view (fingerings' . string)) $
        step ^.. timestep . _QuadrupleStop . each of
        ns@[n1, n2, n3, n4] ->
          cost (set timestep (DoubleStop n1 n2) step)
            + cost (set timestep (DoubleStop n2 n3) step)
            + cost (set timestep (DoubleStop n3 n4) step)
            + let [f1, f2, f3, f4] = ns ^.. traversed . fingerings' . finger
               in if any
                    (\(x, y) -> x == y && x /= Open)
                    [(f1, f3), (f2, f4), (f1, f4)]
                    then infinity
                    else 0
        _ -> 0

--------------------------------------------------------------------------------
-- Penalty2s
--------------------------------------------------------------------------------
oneFingerHalfStep :: Penalty2
oneFingerHalfStep = P "one finger half step shift" cost low
  where
    cost steps = case steps ^.. both . timestep . _Single . fingerings' of
      [Fingering f1 s1 _, Fingering f2 s2 _] | (f1, s1) == (f2, s2) -> 1
      _ -> 0
