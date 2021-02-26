{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Fingering (
  XmlRef,
  deref,
  inferFingerings',
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
  AssignedStep,
  UnassignedStep,
  mkNote,
  notes,
  allAssignments,
  low,
  medium,
  high,
  Weights,
  inferWeights',
) where

import ClassyPrelude hiding (Element, second)
import Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V
import Graph.ShortestPath
import Numeric.AD (stochasticGradientDescent)
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
  pitchEl <- note ^? deep (ell "pitch")
  step <-
    pitchEl ^? deep (ell "step") . text >>= \case
      "C" -> Just 12
      "D" -> Just 14
      "E" -> Just 16
      "F" -> Just 17
      "G" -> Just 19
      "A" -> Just 21
      "B" -> Just 23
      _ -> Nothing
  alter <- (pitchEl ^? deep (ell "alter") . text >>= readMay) <|> pure 0
  octave <- pitchEl ^? deep (ell "octave") . text >>= readMay
  pure $ step + alter + (12 * octave)

-- | Read a fingering constraint from a note element
xmlConstraint :: Element -> Constraint
xmlConstraint note =
  let f = case note ^? deep (ell "fingering") . text of
        Just "0" -> Just Open
        Just "1" -> Just One
        Just "2" -> Just Two
        Just "3" -> Just Three
        Just "4" -> Just Four
        _ -> Nothing
      s = case note ^? deep (ell "string") . text of
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

data Fingering = Fingering {_string :: VString, _finger :: Finger, _distance :: Double}
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

-- | Calculate the (infinite) series of Hz values for a chromatic
-- scale starting at baseFreq.
halfStepFreqs :: Floating t => t -> [t]
halfStepFreqs baseFreq = baseFreq : halfStepFreqs (baseFreq * 2 ** (1 / 12))

-- | Calculate the position on an idealized string with given base
-- length and frequency of a finger required to play a note of
-- frequency freq. Note that real strings don't behave this nicely,
-- but the differences in practice shouldn't matter too much when
-- picking fingerings
halfStepPosition :: Fractional a => a -> a -> a -> a
halfStepPosition baseLen baseFreq freq = baseLen * (1 - (baseFreq / freq))

-- According to
-- https://www.fretlessfingerguides.com/measure_violin_scale_string_length.html
-- and confirmed by shar, pirastro, an ideal full size violin string
-- has a vibrating length of 330mm.
measurementSeries :: Vector Double
measurementSeries =
  -- There's no point to calculating this at runtime
  -- halfStepFreqs 440 & map (halfStepPosition 330 440) & take 30
  V.fromList
    [ 0 -- e
    , 18.521476815041137 -- f
    , 36.00342301368804 -- f#
    , 52.50418296627425 -- g
    , 68.0788264252471 -- g#
    , 82.77933231534762 -- a
    , 96.65476220843938 -- a#
    , 109.7514240619444 -- b
    , 122.113026767346 -- c
    , 133.78082602455106 -- c#
    , 144.79376202895352 -- d
    , 155.18858943071635 -- d#
    , 165.00000000000009 -- e
    , 174.26073840752065 -- f
    , 183.0017115068441 -- f#
    , 191.25209148313715 -- g
    , 199.0394132126236 -- g#
    , 206.38966615767384 -- a
    , 213.3273811042197 -- a#
    , 219.8757120309722 -- b
    , 226.056513383673 -- c
    , 231.89041301227553 -- c#
    , 237.3968810144768 -- d
    , 242.5942947153582 -- d#
    , 247.50000000000003 -- e
    , 252.13036920376032 -- f
    , 256.50085575342206 -- f#
    , 260.6260457415686 -- g
    , 264.5197066063118 -- g#
    , 268.19483307883695 -- a
    ]

-- Patterns to make matching on the measurement series easier
-- TODO: matching on doubles like this is probably crazy unsafe...
pattern E5, F5, Fs5, G5, Gs5, A5, As5, B5, C6, Cs6, D6, Ds6, E6, F6, Fs6, G6, Gs6, A6, As6, B6, C7, Cs7, D7, Ds7, E7, F7, Fs7, G7, Gs7, A7 :: Double
pattern E5 = 0
pattern F5 = 18.521476815041137
pattern Fs5 = 36.00342301368804
pattern G5 = 52.50418296627425
pattern Gs5 = 68.0788264252471
pattern A5 = 82.77933231534762
pattern As5 = 96.65476220843938
pattern B5 = 109.7514240619444
pattern C6 = 122.113026767346
pattern Cs6 = 133.78082602455106
pattern D6 = 144.79376202895352
pattern Ds6 = 155.18858943071635
pattern E6 = 165.00000000000009
pattern F6 = 174.26073840752065
pattern Fs6 = 183.0017115068441
pattern G6 = 191.25209148313715
pattern Gs6 = 199.0394132126236
pattern A6 = 206.38966615767384
pattern As6 = 213.3273811042197
pattern B6 = 219.8757120309722
pattern C7 = 226.056513383673
pattern Cs7 = 231.89041301227553
pattern D7 = 237.3968810144768
pattern Ds7 = 242.5942947153582
pattern E7 = 247.50000000000003
pattern F7 = 252.13036920376032
pattern Fs7 = 256.50085575342206
pattern G7 = 260.6260457415686
pattern Gs7 = 264.5197066063118
pattern A7 = 268.19483307883695
{-# COMPLETE E5, F5, F5, Fs5, G5, Gs5, A5, As5, B5, C6, Cs6, D6, Ds6, E6, F6, Fs6, G6, Gs6, A6, As6, B6, C7, Cs7, D7, Ds7, E7, F7, Fs7, G7, Gs7, A7 #-}

data Position = Half | First | Second | Third | Fourth | Fifth | Sixth | Seventh | EighthAndUp
  deriving (Show, Eq, Ord, Enum, Bounded)

position :: Fingering -> [Position]
position (Fingering _ Open E5) = [Half .. EighthAndUp]
position (Fingering _ One F5) = [Half, First]
position (Fingering _ One Fs5) = [First]
position (Fingering _ One G5) = [Second]
position (Fingering _ One Gs5) = [Second, Third]
position (Fingering _ One A5) = [Third]
position (Fingering _ One As5) = [Third, Fourth]
position (Fingering _ One B5) = [Fourth]
position (Fingering _ One C6) = [Fourth, Fifth]
position (Fingering _ One Cs6) = [Fifth]
position (Fingering _ One D6) = [Sixth]
position (Fingering _ One Ds6) = [Seventh]
position (Fingering _ One E6) = [EighthAndUp]
position (Fingering _ One F6) = [EighthAndUp]
position (Fingering _ One Fs6) = [EighthAndUp]
position (Fingering _ One G6) = [EighthAndUp]
position (Fingering _ One Gs6) = [EighthAndUp]
position (Fingering _ One A6) = [EighthAndUp]
position (Fingering _ One As6) = [EighthAndUp]
position (Fingering _ One B6) = [EighthAndUp]
position (Fingering _ One C7) = [EighthAndUp]
position (Fingering _ One Cs7) = [EighthAndUp]
position (Fingering _ One D7) = [EighthAndUp]
position (Fingering _ One Ds7) = [EighthAndUp]
position (Fingering _ One E7) = [EighthAndUp]
position (Fingering _ One F7) = [EighthAndUp]
position (Fingering _ One Fs7) = [EighthAndUp]
position (Fingering _ One G7) = [EighthAndUp]
position (Fingering _ One Gs7) = [EighthAndUp]
position (Fingering _ One A7) = [EighthAndUp]
position (Fingering _ Two Fs5) = [Half]
position (Fingering _ Two G5) = [First]
position (Fingering _ Two Gs5) = [First, Second]
position (Fingering _ Two A5) = [Second]
position (Fingering _ Two As5) = [Second, Third]
position (Fingering _ Two B5) = [Third]
position (Fingering _ Two C6) = [Fourth]
position (Fingering _ Two Cs6) = [Fourth]
position (Fingering _ Two D6) = [Fifth]
position (Fingering _ Two Ds6) = [Fifth]
position (Fingering _ Two E6) = [Sixth]
position (Fingering _ Two F6) = [Seventh]
position (Fingering _ Two Fs6) = [Seventh]
position (Fingering _ Two G6) = [EighthAndUp]
position (Fingering _ Two Gs6) = [EighthAndUp]
position (Fingering _ Two A6) = [EighthAndUp]
position (Fingering _ Two As6) = [EighthAndUp]
position (Fingering _ Two B6) = [EighthAndUp]
position (Fingering _ Two C7) = [EighthAndUp]
position (Fingering _ Two Cs7) = [EighthAndUp]
position (Fingering _ Two D7) = [EighthAndUp]
position (Fingering _ Two Ds7) = [EighthAndUp]
position (Fingering _ Two E7) = [EighthAndUp]
position (Fingering _ Two F7) = [EighthAndUp]
position (Fingering _ Two Fs7) = [EighthAndUp]
position (Fingering _ Two G7) = [EighthAndUp]
position (Fingering _ Two Gs7) = [EighthAndUp]
position (Fingering _ Two A7) = [EighthAndUp]
position (Fingering _ Three G5) = [Half]
position (Fingering _ Three Gs5) = [First]
position (Fingering _ Three A5) = [First]
position (Fingering _ Three As5) = [First, Second]
position (Fingering _ Three B5) = [Second]
position (Fingering _ Three C6) = [Second, Third]
position (Fingering _ Three Cs6) = [Third, Fourth]
position (Fingering _ Three D6) = [Fourth]
position (Fingering _ Three Ds6) = [Fourth, Fifth]
position (Fingering _ Three E6) = [Fifth]
position (Fingering _ Three F6) = [Fifth]
position (Fingering _ Three Fs6) = [Sixth]
position (Fingering _ Three G6) = [Seventh]
position (Fingering _ Three Gs6) = [Seventh]
position (Fingering _ Three A6) = [EighthAndUp]
position (Fingering _ Three As6) = [EighthAndUp]
position (Fingering _ Three B6) = [EighthAndUp]
position (Fingering _ Three C7) = [EighthAndUp]
position (Fingering _ Three Cs7) = [EighthAndUp]
position (Fingering _ Three D7) = [EighthAndUp]
position (Fingering _ Three Ds7) = [EighthAndUp]
position (Fingering _ Three E7) = [EighthAndUp]
position (Fingering _ Three F7) = [EighthAndUp]
position (Fingering _ Three Fs7) = [EighthAndUp]
position (Fingering _ Three G7) = [EighthAndUp]
position (Fingering _ Three Gs7) = [EighthAndUp]
position (Fingering _ Three A7) = [EighthAndUp]
position (Fingering _ Four Gs5) = [Half]
position (Fingering _ Four A5) = [Half]
position (Fingering _ Four As5) = [First]
position (Fingering _ Four B5) = [First]
position (Fingering _ Four C6) = [Second]
position (Fingering _ Four Cs6) = [Second]
position (Fingering _ Four D6) = [Third]
position (Fingering _ Four Ds6) = [Third, Fourth]
position (Fingering _ Four E6) = [Fourth]
position (Fingering _ Four F6) = [Fourth, Fifth]
position (Fingering _ Four Fs6) = [Fifth]
position (Fingering _ Four G6) = [Sixth]
position (Fingering _ Four Gs6) = [Sixth]
position (Fingering _ Four A6) = [Seventh]
position (Fingering _ Four As6) = [Seventh]
position (Fingering _ Four B6) = [EighthAndUp]
position (Fingering _ Four C7) = [EighthAndUp]
position (Fingering _ Four Cs7) = [EighthAndUp]
position (Fingering _ Four D7) = [EighthAndUp]
position (Fingering _ Four Ds7) = [EighthAndUp]
position (Fingering _ Four E7) = [EighthAndUp]
position (Fingering _ Four F7) = [EighthAndUp]
position (Fingering _ Four Fs7) = [EighthAndUp]
position (Fingering _ Four G7) = [EighthAndUp]
position (Fingering _ Four Gs7) = [EighthAndUp]
position (Fingering _ Four A7) = [EighthAndUp]
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

data Penalty step a = P
  { _pName :: Text
  , _pCost :: step -> a
  , _pWeight :: a
  }

makeLenses ''Penalty

type Penalty1 = Penalty AssignedStep

type Penalty2 = Penalty (AssignedStep, AssignedStep)

applyP1s :: Num a => Weights a -> [Penalty1 a] -> AssignedStep -> a
applyP1s weights ps step = sum $ map cost ps
  where
    cost p =
      let weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
       in (p ^. pCost) step * weight

applyP2s :: Num a => Weights a -> [Penalty2 a] -> AssignedStep -> AssignedStep -> a
applyP2s weights ps step1 step2 = sum $ map cost ps
  where
    cost p =
      let weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
       in (p ^. pCost) (step1, step2) * weight

mkNote :: XmlRef -> TimeStep Set
mkNote ref =
  case xmlPitch (deref ref) of
    Just p ->
      let fs =
            S.fromList $
              filter
                (flip validPlacement (xmlConstraint (deref ref)))
                (allFingerings p)
       in Single $ Note ref fs
    Nothing -> Rest

-- The cartesian product of all the possibleFingerings for a given timestep
allAssignments :: UnassignedStep -> [AssignedStep]
allAssignments (Step ns dur) = map (flip Step dur) (go ns)
  where
    note x f = Note x (Identity f)

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
            pure $ DoubleStop (note x1 f1) (note x2 f2)
          TripleStop (Note x1 fs1) (Note x2 fs2) (Note x3 fs3) -> do
            f1 <- S.toList fs1
            f2 <- S.toList fs2
            f3 <- S.toList fs3
            pure $ TripleStop (note x1 f1) (note x2 f2) (note x3 f3)
          QuadrupleStop (Note x1 fs1) (Note x2 fs2) (Note x3 fs3) (Note x4 fs4) -> do
            f1 <- S.toList fs1
            f2 <- S.toList fs2
            f3 <- S.toList fs3
            f4 <- S.toList fs4
            pure $ QuadrupleStop (note x1 f1) (note x2 f2) (note x3 f3) (note x4 f4)

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

p1s :: Num a => [Penalty1 a]
p1s =
  singles
    <> doubleStops
    <> [ trill
       , chordAdjacent
       , staticTripleStop
       , staticQuadrupleStop
       ]

singles, doubleStops :: Num a => [Penalty1 a]
singles = [highPosition, mediumPosition, fourthFinger, openString]
doubleStops =
  [ staticUnison
  , staticSecond
  , staticThird
  , staticFourth
  , staticFifth
  , staticMinorSixth
  , staticMajorSixth
  , staticMinorSeventh
  , staticMajorSeventh
  , staticOctave
  , staticTenth
  ]

p2s :: Num a => [Penalty2 a]
p2s = [oneFingerHalfStep, samePosition, sameString]

type Weights a = Map Text a
inferFingerings' :: Weights Double -> [UnassignedStep] -> (Double, [AssignedStep])
inferFingerings' weights steps =
  case mkAssignments steps of
    Nothing -> error "Could not assign fingering"
    Just steps' -> shortestPath infinity steps' (applyP1s weights p1s) (applyP2s weights p2s)

mkAssignments :: [UnassignedStep] -> Maybe [NE.NonEmpty AssignedStep]
mkAssignments steps = traverse (NE.nonEmpty . allAssignments) steps

-- | Given a corpus of fingered passages and an initial weight configuration, run SGD
-- to find the weight configuration that minimizes the cost function
inferWeights' :: [[AssignedStep]] -> Weights Double -> Weights Double
inferWeights' fingeringCorpus =
  (L.!! 1000)
    . stochasticGradientDescent objective fingeringCorpus
  where
    objective assigned ws =
      pathCost (applyP1s ws p1s) (applyP2s ws p2s) assigned
        -- weight squared term to bias towards center
        + L.sum (fmap (\w -> w * w) ws)

infinity, high, medium, low :: Num a => a
infinity = 1000000000000000
high = 10
medium = 5
low = 1

binarize :: Num a => Bool -> a
binarize b = if b then 1 else 0

trill :: Num a => Penalty1 a
trill = P "trill" cost high
  where
    cost step = case step ^. timestep of
      Rest -> 0
      Single (Note x (Identity f)) ->
        case deref x ^? deep (failing (ell "trill-mark") (ell "wavy-line")) of
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

highPosition :: Num a => Penalty1 a
highPosition = P "high position" cost medium
  where
    cost step =
      binarize $ anyOf (notes . fingerings' . to position . traversed) (>= EighthAndUp) step

mediumPosition :: Num a => Penalty1 a
mediumPosition = P "medium position" cost low
  where
    cost step =
      binarize $ anyOf (notes . fingerings' . to position . traversed) (>= Fourth) step

fourthFinger :: Num a => Penalty1 a
fourthFinger = P "fourth finger" cost 0
  where
    cost step = binarize $ step ^? timestep . _Single . fingerings' . finger == Just Four

openString :: Num a => Penalty1 a
openString = P "open string" cost 0
  where
    cost step = binarize $ anyOf (notes . fingerings' . finger) (== Open) step

--------------------------------------------------------------------------------
-- Double Stops
--------------------------------------------------------------------------------
chordAdjacent :: Num a => Penalty1 a
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

staticThird :: Num a => Penalty1 a
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

staticMinorSixth :: Num a => Penalty1 a
staticMinorSixth = P "static minor sixth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _m6 n1 n2 ->
          let f1 = n1 ^. fingerings'
              f2 = n2 ^. fingerings'
           in case (f1 ^. finger, f2 ^. finger) of
                (One, Two) -> 0
                (Two, Three) -> 0
                (Three, Four) -> 0
                (_, Open) -> low
                (Open, _) -> low
                _ -> infinity
      _ -> 0

staticMajorSixth :: Num a => Penalty1 a
staticMajorSixth = P "static major sixth" cost high
  where
    cost step = case step ^. timestep of
      DoubleStop n1 n2
        | _m6 n1 n2 ->
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

staticOctave :: Num a => Penalty1 a
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
                (One, Three) -> if f2 ^. distance >= G7 then 0 else low
                (Two, Four) -> if f2 ^. distance >= G7 then 0 else low
                _ -> infinity
      _ -> 0

staticTenth :: Num a => Penalty1 a
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

staticUnison :: Num a => Penalty1 a
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

staticSecond :: Num a => Penalty1 a
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

staticFourth :: Num a => Penalty1 a
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

staticFifth :: Num a => Penalty1 a
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

staticMinorSeventh :: Num a => Penalty1 a
staticMinorSeventh = P "static minor seventh" cost high
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
                (One, Two) -> low
                (Two, Three) -> low
                (Three, Four) -> medium
                _ -> infinity
      _ -> 0

staticMajorSeventh :: Num a => Penalty1 a
staticMajorSeventh = P "static major seventh" cost high
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

staticTripleStop :: Num a => Penalty1 a
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

staticQuadrupleStop :: Num a => Penalty1 a
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
oneFingerHalfStep :: Num a => Penalty2 a
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

samePosition :: Num a => Penalty2 a
samePosition = P "same position" cost (- high)
  where
    cost steps =
      let positions = steps ^.. both . notes . fingerings' . to position
       in binarize $ not (null (F.foldr1 L.intersect positions))

sameString :: Num a => Penalty2 a
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
