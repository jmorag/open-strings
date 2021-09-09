-- |
module Visualization (mkGraph, renderGraph, VisualizeParams (..)) where

import ClassyPrelude hiding (Element, index)
import ClassyPrelude.Yesod (Html, shamlet)
import Control.Lens hiding (index, (.=))
import Control.Monad.State
import Data.Aeson
import Fingering
import MusicXML
import Text.XML
import Text.XML.Lens hiding (name, nodes)

data D3Graph = D3Graph
  { nodes :: [[D3Node]]
  , links :: [D3Link]
  }
  deriving (Generic)

instance ToJSON D3Graph where
  toJSON D3Graph {nodes, links} =
    object
      [ "nodes" .= concat nodes
      , "links" .= links
      ]

data Penalty = Penalty {name :: Text, cost :: Double, weight :: Double}
  deriving (Generic)
  deriving anyclass (ToJSON)

data D3Node = D3Node
  { label :: AssignedStep
  , penalties :: [Penalty]
  , fx :: Double
  , fy :: Double
  , index :: Int
  }
  deriving (Generic)

instance ToJSON D3Node where
  toJSON D3Node {label, penalties, fx, fy} =
    object
      [ "label" .= show (_timestep label)
      , "fx" .= fx
      , "fy" .= fy
      , "penalties" .= penalties
      ]

data D3Link = D3Link
  { source :: D3Node
  , target :: D3Node
  , penalties :: [Penalty]
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

number :: (Traversable f, Traversable g) => f (g a) -> f (g (a, Int))
number =
  flip evalState 0 . mapM (mapM \x -> do i <- get; put (i + 1); pure (x, i))

mkGraph :: Document -> VisualizeParams -> D3Graph
mkGraph doc VisualizeParams {..} =
  let steps = doc ^. partsOf' (root . timeStep . attributeIs "data-selected" "")
      height = 400
      assignments =
        mkAssignments (readTimeSteps steps)
          & fromMaybe (error "Could not create graph")
          & number
          & map toList
      nodes =
        zipWith
          ( \ass x ->
              zipWith
                ( \(as, index) y ->
                    mkNode
                      infer_weights
                      width
                      height
                      x
                      y
                      (length assignments)
                      (length ass)
                      index
                      as
                )
                ass
                [0 ..]
          )
          assignments
          [0 ..]
      links = mkLinks infer_weights nodes
   in D3Graph {nodes, links}

-- mkNode :: Weights Double -> Int -> Int -> AssignedStep -> D3Node
mkNode weights width height x y n_x n_y index step =
  D3Node
    { label = step
    , penalties = map mkPenalty p1s
    , fx = uniform x n_x width
    , fy = uniform y n_y height
    , index
    }
  where
    mkPenalty p =
      Penalty
        { name = p ^. pName
        , cost = (p ^. pCost) step
        , weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
        }

mkLinks :: Weights Double -> [[D3Node]] -> [D3Link]
mkLinks weights steps = case steps of
  (_ : ss) -> zipWith (mkLink weights) steps ss & concat
  [] -> []

mkLink :: Weights Double -> [D3Node] -> [D3Node] -> [D3Link]
mkLink weights ls rs = do
  l <- ls
  r <- rs
  let mkPenalty p =
        Penalty
          { name = p ^. pName
          , cost = (p ^. pCost) (label l, label r)
          , weight = fromMaybe (p ^. pWeight) (lookup (p ^. pName) weights)
          }
  pure $ D3Link {source = l, target = r, penalties = map mkPenalty p2s}

data VisualizeParams = VisualizeParams
  { infer_xml :: !LText
  , infer_weights :: !(Map Text Double)
  , width :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

uniform :: Int -> Int -> Int -> Double
uniform i n len = fi (i + 1) * (fi len / (fi n + 1))
  where
    fi = fromIntegral

renderGraph :: Document -> VisualizeParams -> Html
renderGraph musicxml ps@VisualizeParams {..} =
  let D3Graph {..} = mkGraph musicxml ps
      -- TODO calculate these from number of steps
      height = 400 :: Int
      radius = 7 :: Int
      showPenalties =
        map (\p -> name p <> ". Cost: " <> tshow (cost p) <> " Weight: " <> tshow (weight p))
          . filter (\p -> cost p /= 0)
      nodeLabel D3Node {..} =
        unlines $ tshow (_timestep label) : showPenalties penalties
   in [shamlet|
  <svg width=#{width} height=#{height}>
    <g fill="orange">
      $forall node <- concat nodes
        <circle r=#{radius} cx=#{fx node} cy=#{fy node}>
          <title>#{nodeLabel node}
    <g stroke="#999" opacity="0.6" stroke-width="2.5">
      $forall D3Link source target penalties <- links
        <line x1=#{fx source} x2=#{fx target} y1=#{fy source} y2=#{fy target}>
          <title>#{unlines (showPenalties penalties)}
    |]
