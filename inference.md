# Thoughts on inference
## 10/31/2020

I've now reached the point where the thesis demands creativity. After gridding a passage, sheet music gets reduced to:

```haskell
data Finger = Open | One | Two | Three | Four
  deriving (Show, Eq, Enum, Ord)

data VString = E | A | D | G
  deriving (Show, Eq, Enum, Ord)

data Fingering = Fingering {string :: VString, finger :: Finger, distance :: Int}
  deriving (Eq, Show, Ord)

-- XmlRef is basically a pointer to that note inside the musicxml document it came from.
-- It contains all information about pitch, articulation, etc. necessary to make 
-- fingering decisions.
-- possibleFingerings are preselected based on impossible placements (putting the pinkie 
-- at the lowest point of the fingerboard) and constraints already entered by the user
data Note = Note {xmlRef :: XmlRef, possibleFingerings :: Set Fingering}

data TimeStep
  = Single Note
  | DoubleStop Note Note
  | TripleStop Note Note Note
  | QuadrupleStop Note Note Note Note
  | Rest
  deriving (Show, Eq)
  
data Step = Step {notes :: TimeStep, duration :: Int}
  deriving (Show, Eq)
  
type Passage = List Step
```

At this point, since I have relatively little data but very strong beliefs about the prior, I propose to proceed by manually constructing binary feature vectors from each group of two consecutive `Step`s and initializing weights to what I believe are appropriate penalties for each feature. Something like

```haskell
-- could also be called Transition or Penalty (naming is hard)
type Duration = Int -- how many timesteps (can also take xml tempo into account, although this isn't generally reliable)
type Distance = Int -- mm 
data Feature = 
    SamePosition
  | StringCrossing VString VString
  | Shift Finger Finger Distance
  | Trill Finger
  | ExcessiveFourthFinger Duration
  | ...
  deriving (Ord, Ix) -- the Ix typeclass constructs an isomorphism between this type and integers, so these can all be put into an array and we can do linear algebra reasonably
  
weights :: Feature -> Double
weights = \case
  SamePosition -> 0
  StringCrossing G E -> high
  StringCrossing G D -> low
  ..etc
  Shift Four Three _ -> high
  ..etc
  Trill Three -> high
  Trill Four -> infinity
  ..etc
  ExcessiveFourthFinger duration -> f duration -- some function of duration and possibly tempo
  ...
```

Then, given
```haskell
extractFeatures :: Step -> Step -> Fingering -> Fingering -> Set Feature
```

we can construct something isomorphic to a binary feature vector and minimize fingering assignment costs over this sequence. I think this eventually reduces to a tree search and is solvable with Dijkstra's algorithm or A*. This framework is also nice in that adjusting for violinists' preferences is as simple as tweaking the weights, which can be exposed to users as a GUI with a bunch of sliders or eventually learned if people submit enough fingerings.
