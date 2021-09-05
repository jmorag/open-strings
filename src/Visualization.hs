-- |
module Visualization (mkGraph) where

import ClassyPrelude hiding (Element, id)
import Control.Lens hiding ((.=))
import Control.Monad.State
import Data.Aeson
import Fingering
import MusicXML
import Text.XML
import Text.XML.Lens hiding (nodes)

data D3Graph = D3Graph {nodes :: [D3Node], links :: [D3Link]}
  deriving (Generic)
  deriving anyclass (ToJSON)

data Penalty = Penalty {name :: Text, cost :: Double, weight :: Double}
  deriving (Generic)
  deriving anyclass (ToJSON)

data D3Node = D3Node
  { label :: AssignedStep
  , penalties :: [Penalty]
  , -- , x :: Int
    id :: Int
  }
  deriving (Generic)

instance ToJSON D3Node where
  toJSON D3Node {label, penalties, id} =
    object ["label" .= show label, "penalties" .= penalties, "id" .= id]

data D3Link = D3Link
  { source :: Int
  , target :: Int
  , penalties :: [Penalty]
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

mkGraph :: Document -> Weights Double -> D3Graph
mkGraph doc weights =
  let steps = doc ^. partsOf' (root . timeStep)
      assignments =
        mkAssignments (readTimeSteps steps)
          & fromMaybe (error "Could not create graph")
      numberedAssignments = number assignments & map toList
      nodes = map (mkNode weights) (concat numberedAssignments)
      links = mkLinks weights numberedAssignments
   in D3Graph {nodes, links}

number :: (Traversable f, Traversable g) => f (g a) -> f (g (a, Int))
number =
  flip evalState 0 . mapM (mapM \x -> do i <- get; put (i + 1); pure (x, i))

mkNode :: Weights Double -> (AssignedStep, Int) -> D3Node
mkNode weights (step, id) =
  D3Node {label = step, penalties = map mkPenalty p1s, id}
  where
    mkPenalty p =
      Penalty
        { name = p ^. pName
        , cost = (p ^. pCost) step
        , weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
        }

mkLinks :: Weights Double -> [[(AssignedStep, Int)]] -> [D3Link]
mkLinks weights steps = case steps of
  (_ : ss) -> zipWith (mkLink weights) steps ss & concat
  [] -> []

mkLink :: Weights Double -> [(AssignedStep, Int)] -> [(AssignedStep, Int)] -> [D3Link]
mkLink weights ls rs = do
  (l, idl) <- ls
  (r, idr) <- rs
  let mkPenalty p =
        Penalty
          { name = p ^. pName
          , cost = (p ^. pCost) (l, r)
          , weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
          }
  pure $ D3Link {source = idl, target = idr, penalties = map mkPenalty p2s}
