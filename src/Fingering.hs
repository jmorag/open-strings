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
  low,
  medium,
  high,
  Weights,
) where

import ClassyPrelude hiding (Element, second)
import Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V
import Graph.ShortestPath
import Text.XML.Lens

data Finger = Open | One | Two | Three | Four
  deriving (Show, Eq, Enum, Ord)

-- we want lower strings to come before higher strings as that's the
-- order we think about fingerings for chords, despite the convention
-- of E = I, A = II, etc.
data VString = G | D | A | E
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

-- TODO: fixme, should be idealized string split into halfsteps, not
-- some ridiculous tape measure I did on my fingerboard
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

data Position = Half | First | Second | Third | Fourth | Fifth | Sixth | Seventh | EighthAndUp
  deriving (Show, Eq, Ord, Enum, Bounded)

position :: Fingering -> [Position]
position (Fingering _ Open _) = [Half .. EighthAndUp]
position (Fingering _ One 13) = [Half, First]
position (Fingering _ One 30) = [First]
position (Fingering _ One 45) = [Second]
position (Fingering _ One 61) = [Second, Third]
position (Fingering _ One 87) = [Third]
position (Fingering _ One 101) = [Third, Fourth]
position (Fingering _ One 114) = [Fourth]
position (Fingering _ One 129) = [Fourth, Fifth]
position (Fingering _ One 138) = [Fifth]
position (Fingering _ One 147) = [Sixth]
position (Fingering _ One 156) = [Seventh]
position (Fingering _ One 168) = [EighthAndUp]
position (Fingering _ One 180) = [EighthAndUp]
position (Fingering _ One 190) = [EighthAndUp]
position (Fingering _ One 195) = [EighthAndUp]
position (Fingering _ One 202) = [EighthAndUp]
position (Fingering _ One 209) = [EighthAndUp]
position (Fingering _ One 213) = [EighthAndUp]
position (Fingering _ One 222) = [EighthAndUp]
position (Fingering _ One 228) = [EighthAndUp]
position (Fingering _ One 235) = [EighthAndUp]
position (Fingering _ One 241) = [EighthAndUp]
position (Fingering _ One 245) = [EighthAndUp]
position (Fingering _ One 251) = [EighthAndUp]
position (Fingering _ One 256) = [EighthAndUp]
position (Fingering _ One 260) = [EighthAndUp]
position (Fingering _ One 262) = [EighthAndUp]
position (Fingering _ One 268) = [EighthAndUp]
position (Fingering _ One 271) = [EighthAndUp]
position (Fingering _ Two 30) = [Half]
position (Fingering _ Two 45) = [First]
position (Fingering _ Two 61) = [First]
position (Fingering _ Two 87) = [Second]
position (Fingering _ Two 101) = [Third]
position (Fingering _ Two 114) = [Third]
position (Fingering _ Two 129) = [Fourth]
position (Fingering _ Two 138) = [Fourth]
position (Fingering _ Two 147) = [Fifth]
position (Fingering _ Two 156) = [Fifth]
position (Fingering _ Two 168) = [Sixth]
position (Fingering _ Two 180) = [Seventh]
position (Fingering _ Two 190) = [Seventh]
position (Fingering _ Two 195) = [EighthAndUp]
position (Fingering _ Two 202) = [EighthAndUp]
position (Fingering _ Two 209) = [EighthAndUp]
position (Fingering _ Two 213) = [EighthAndUp]
position (Fingering _ Two 222) = [EighthAndUp]
position (Fingering _ Two 228) = [EighthAndUp]
position (Fingering _ Two 235) = [EighthAndUp]
position (Fingering _ Two 241) = [EighthAndUp]
position (Fingering _ Two 245) = [EighthAndUp]
position (Fingering _ Two 251) = [EighthAndUp]
position (Fingering _ Two 256) = [EighthAndUp]
position (Fingering _ Two 260) = [EighthAndUp]
position (Fingering _ Two 262) = [EighthAndUp]
position (Fingering _ Two 268) = [EighthAndUp]
position (Fingering _ Two 271) = [EighthAndUp]
position (Fingering _ Three 45) = [Half]
position (Fingering _ Three 61) = [First]
position (Fingering _ Three 87) = [First]
position (Fingering _ Three 101) = [First, Second]
position (Fingering _ Three 114) = [Second]
position (Fingering _ Three 129) = [Second, Third]
position (Fingering _ Three 138) = [Third, Fourth]
position (Fingering _ Three 147) = [Fourth]
position (Fingering _ Three 156) = [Fourth, Fifth]
position (Fingering _ Three 168) = [Fifth]
position (Fingering _ Three 180) = [Fifth]
position (Fingering _ Three 190) = [Sixth]
position (Fingering _ Three 195) = [Seventh]
position (Fingering _ Three 202) = [Seventh]
position (Fingering _ Three 209) = [EighthAndUp]
position (Fingering _ Three 213) = [EighthAndUp]
position (Fingering _ Three 222) = [EighthAndUp]
position (Fingering _ Three 228) = [EighthAndUp]
position (Fingering _ Three 235) = [EighthAndUp]
position (Fingering _ Three 241) = [EighthAndUp]
position (Fingering _ Three 245) = [EighthAndUp]
position (Fingering _ Three 251) = [EighthAndUp]
position (Fingering _ Three 256) = [EighthAndUp]
position (Fingering _ Three 260) = [EighthAndUp]
position (Fingering _ Three 262) = [EighthAndUp]
position (Fingering _ Three 268) = [EighthAndUp]
position (Fingering _ Three 271) = [EighthAndUp]
position (Fingering _ Four 61) = [Half]
position (Fingering _ Four 87) = [Half]
position (Fingering _ Four 101) = [First]
position (Fingering _ Four 114) = [First]
position (Fingering _ Four 129) = [Second]
position (Fingering _ Four 138) = [Second]
position (Fingering _ Four 147) = [Third]
position (Fingering _ Four 156) = [Third, Fourth]
position (Fingering _ Four 168) = [Fourth]
position (Fingering _ Four 180) = [Fourth, Fifth]
position (Fingering _ Four 190) = [Fifth]
position (Fingering _ Four 195) = [Sixth]
position (Fingering _ Four 202) = [Sixth]
position (Fingering _ Four 209) = [Seventh]
position (Fingering _ Four 213) = [Seventh]
position (Fingering _ Four 222) = [EighthAndUp]
position (Fingering _ Four 228) = [EighthAndUp]
position (Fingering _ Four 235) = [EighthAndUp]
position (Fingering _ Four 241) = [EighthAndUp]
position (Fingering _ Four 245) = [EighthAndUp]
position (Fingering _ Four 251) = [EighthAndUp]
position (Fingering _ Four 256) = [EighthAndUp]
position (Fingering _ Four 260) = [EighthAndUp]
position (Fingering _ Four 262) = [EighthAndUp]
position (Fingering _ Four 268) = [EighthAndUp]
position (Fingering _ Four 271) = [EighthAndUp]
position _ = []

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

applyP1s :: Weights -> [Penalty1] -> AssignedStep -> Double
applyP1s weights ps step = sum $ map cost ps
  where
    cost p =
      let weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
       in (p ^. pCost) step * weight

applyP2s :: Weights -> [Penalty2] -> AssignedStep -> AssignedStep -> Double
applyP2s weights ps step1 step2 = sum $ map cost ps
  where
    cost p =
      let weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
       in (p ^. pCost) (step1, step2) * weight

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
allAssignments :: Weights -> UnassignedStep -> [AssignedStep]
allAssignments weights (Step ns dur) = map (flip Step dur) (go ns)
  where
    staticCost = applyP1s weights p1s
    note x f = Note x (Identity f)
    ret step =
      let cost = staticCost (Step step dur)
       in if cost < infinity then pure step else mzero

    go :: TimeStep Set -> [TimeStep Identity]
    go unassigned = case possible of
      [] -> error (show unassigned)
      _ -> possible
      where
        possible = case unassigned of
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

p1s :: [Penalty1]
p1s =
  singles
    <> doubleStops
    <> [ trill
       , chordAdjacent
       , staticTripleStop
       , staticQuadrupleStop
       ]

singles, doubleStops :: [Penalty1]
singles = [highPosition, mediumPosition, fourthFinger, openString]
doubleStops =
  [ staticUnison
  , staticSecond
  , staticThird
  , staticFourth
  , staticFifth
  , staticMinorSixth
  , staticMajorSixth
  , staticSeventh
  , staticOctave
  , staticTenth
  ]

p2s :: [Penalty2]
p2s = [oneFingerHalfStep, samePosition, sameString]

type Weights = Map Text Double
infer :: Weights -> [UnassignedStep] -> [AssignedStep]
infer weights steps =
  case sequence $ steps ^.. (traversed . to (allAssignments weights) . to NE.nonEmpty) of
    Nothing -> error "Could not assign fingering"
    Just steps' -> case shortestPath steps' (applyP1s weights p1s) (applyP2s weights p2s) of
      (c, path) -> traceShow c path

high, medium, low :: Double
high = 10
medium = 5
low = 1

binarize :: Bool -> Double
binarize b = if b then 1 else 0

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

highPosition :: Penalty1
highPosition = P "high position" cost medium
  where
    cost step =
      binarize $ anyOf (notes . fingerings' . to position . traversed) (>= EighthAndUp) step

mediumPosition :: Penalty1
mediumPosition = P "medium position" cost low
  where
    cost step =
      binarize $ anyOf (notes . fingerings' . to position . traversed) (>= Fourth) step

fourthFinger :: Penalty1
fourthFinger = P "fourth finger" cost 0
  where
    cost step = binarize $ step ^? timestep . _Single . fingerings' . finger == Just Four

openString :: Penalty1
openString = P "open string" cost 0
  where
    cost step = binarize $ anyOf (notes . fingerings' . finger) (== Open) step

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
                  | allOf (both . to position . traversed) (<= First) (f1, f2) -> low
                  | otherwise -> medium
                -- TODO: Make these more comprehensive
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

staticMinorSixth :: Penalty1
staticMinorSixth = P "static minor sixth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _m6 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (Two, Two) -> 0
                (Two, Three) -> 0
                (Three, Four) -> 0
                (_, Open) -> low
                (Open, _) -> low
                _ -> infinity
      _ -> 0

staticMajorSixth :: Penalty1
staticMajorSixth = P "static major sixth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _m6 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (Two, Two) -> 0
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
                (Two, Four) -> 0
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
                (Four, Two) -> 0
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
                (Four, Two) -> 0
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
                (Two, Two) -> 0
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
                (Four, Four) -> 0
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
                (Two, Three) -> 0
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
          -- TODO: fixme pass weights to all penalty functions
          applyP1s mempty doubleStops (set timestep (DoubleStop n1 n2) step)
            + applyP1s mempty doubleStops (set timestep (DoubleStop n2 n3) step)
            + let [f1, _, f3] = ns ^.. traversed . fingerings' . finger
               in if f1 == f3 && f1 /= Open then infinity else 0
        _ -> 0

staticQuadrupleStop :: Penalty1
staticQuadrupleStop = P "static quadruple stop" cost high
  where
    cost step =
      case sortOn (view (fingerings' . string)) $
        step ^.. timestep . _QuadrupleStop . each of
        ns@[n1, n2, n3, n4] ->
          -- TODO: fixme pass weights to all penalty functions
          applyP1s mempty doubleStops (set timestep (DoubleStop n1 n2) step)
            + applyP1s mempty doubleStops (set timestep (DoubleStop n2 n3) step)
            + applyP1s mempty doubleStops (set timestep (DoubleStop n3 n4) step)
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
oneFingerHalfStep = P "one finger half step shift" cost (- low)
  where
    cost (Step (Single n1) _, Step (Single n2) _)
      | and
          [ _m2 n1 n2
          , n1 ^. fingerings' . finger == n2 ^. fingerings' . finger
          , n1 ^. fingerings' . string == n2 ^. fingerings' . string
          ] =
        1
    -- TODO: double stops
    cost _ = 0

samePosition :: Penalty2
samePosition = P "same position" cost (- high)
  where
    cost steps =
      let positions = steps ^.. both . notes . fingerings' . to position
       in binarize $ not (null (F.foldr1 L.intersect positions))

sameString :: Penalty2
sameString = P "same string" cost (- high)
  where
    cost (x, y) = case (x ^. timestep, y ^. timestep) of
      (Single n1, Single n2) -> binarize $ n1 ^. s == n2 ^. s
      (DoubleStop n11 n12, DoubleStop n21 n22) ->
        binarize $ n11 ^. s == n21 ^. s && n12 ^. s == n22 ^. s
      (Single n1, DoubleStop n21 n22) -> binarize $ (n1 ^. s) `elem` [n21 ^. s, n22 ^. s]
      (DoubleStop n11 n12, Single n2) -> binarize $ (n2 ^. s) `elem` [n11 ^. s, n12 ^. s]
      _ -> 0
    s = fingerings' . string
