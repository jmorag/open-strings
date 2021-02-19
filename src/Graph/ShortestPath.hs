{-# LANGUAGE BangPatterns #-}

-- |
-- Module : Graph.ShortestPath
module Graph.ShortestPath where

import Control.Monad
import Control.Monad.ST
import Data.Function
import Data.List (minimumBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Prelude

-- Introduction to Algorithms, Chapter 6 (Cormen, Leiserson, Rivest, Stein)
-- https://edutechlearners.com/download/Introduction_to_algorithms-3rd%20Edition.pdf
--------------------------------------------------------------------------------
-- DAG-Shortest-Paths(G, w, s):
--   TOPSORT(G) # fingering graph is already in topologically sorted order
--   INITIALIZE-SINGLE-SOURCE(G)
--   for each vertex u, taken in topologically sorted order
--     for each vertex v in G.adj(u)
--       RELAX(u,v,w)
--------------------------------------------------------------------------------
-- INITIALIZE-SINGLE-SOURCE(G, s):
-- for each vertex v in G.V
--   v.d = ∞
--   v.π = NIL
-- s.d = 0
--------------------------------------------------------------------------------
-- RELAX(u, v, w):
-- if v.d > u.d + w(u,v)
--   v.d = u.d + w(u,v)
--   v.π = u

-- G ~ [NonEmpty state]
data GraphNode state a = Node
  { vertex :: state
  , dist :: a
  , staticCost :: a
  , previous :: Maybe (GraphNode state a)
  }
  deriving (Show, Eq, Ord)

getPath :: (Ord a, Num a) => Vector (NonEmpty (GraphNode state a)) -> (a, [state])
getPath vec =
  let end = minimumBy (compare `on` dist) (V.last vec)
   in (dist end, reverse $ vertex end : go (previous end))
  where
    go Nothing = []
    go (Just p) = vertex p : go (previous p)

initialize :: Num a => a -> [NonEmpty state] -> (state -> a) -> [NonEmpty (GraphNode state a)]
initialize infinity graph singleCost =
  map (fmap (\f -> Node f infinity (singleCost f) Nothing)) graph

shortestPath ::
  forall state a.
  (Ord state, Num a, Ord a) =>
  a ->
  [NonEmpty state] ->
  (state -> a) ->
  (state -> state -> a) ->
  (a, [state])
shortestPath _ [] _ _ = (0, [])
shortestPath infinity (fs : rest) singleCost transitionCost =
  fmap mkGraph fs & minimumBy (compare `on` fst)
  where
    mkGraph f =
      let graph =
            V.fromList $
              (Node f 0 (singleCost f) Nothing :| []) : initialize infinity rest singleCost
       in getPath $ V.modify go graph

    relax u v =
      -- add the static cost of the next node when calculating the new distance
      let dist' = dist u + transitionCost (vertex u) (vertex v) + staticCost v
       in if dist' < (dist v) then v {dist = dist', previous = Just u} else v

    go :: forall s. VM.MVector s (NonEmpty (GraphNode state a)) -> ST s ()
    go g = forM_ [1 .. VM.length g - 1] \i -> do
      mapM_ (\u -> VM.modify g (fmap (relax u)) i) =<< VM.read g (i - 1)

pathCost :: (Num a) => (state -> a) -> (state -> state -> a) -> [state] -> a
pathCost singleCost transitionCost = go
  where
    go = \case
      [] -> 0
      [f] -> singleCost f
      (f : f' : fs) -> singleCost f + transitionCost f f' + go (f' : fs)

nEdges :: [NonEmpty state] -> Int
nEdges (x : y : rest) = length x * length y + nEdges (y : rest)
nEdges _ = 0
