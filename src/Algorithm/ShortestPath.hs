{-# LANGUAGE BangPatterns #-}

-- |
-- Module : Algorithm.ShortestPath
module Algorithm.ShortestPath where

-- import Control.Lens
import Control.Monad.Memo
import Control.Monad.Memo.Class
import Data.Function
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector.Unboxed (Unbox)
import Prelude

-- TODO: Right now the memoization uses two Maps, one for single costs
-- and one for transition costs, so we incur a log lookup penalty. Not
-- horrible, but we are very invested in the speed of this procedure.
-- Optimally, we should create a bijection (V `union` E) <=> Int so we
-- can use a single unboxed vector to cache costs. The easiest way to
-- do that would be to decorate `state`s with an Int and then figure
-- out another bijection (Int, Int) <=> Int for state pairs. The
-- standard strided array index formulation is suboptimal because
-- [NonEmpty state] is ragged, so picking the largest NonEmpty state
-- would waste space.
shortestKPaths ::
  (Ord state, Unbox cost, Num cost, Ord cost) =>
  Int ->
  (state -> cost) ->
  (state -> state -> cost) ->
  [NonEmpty state] ->
  [(cost, [state])]
shortestKPaths k singleCost transitionCost graph =
  let paths = enumeratePaths graph
      doMemo m x = startEvalMemo (startEvalMemoT (m x))
      costs = doMemo (mapM (\p -> fmap (,p) (pathCost singleCost transitionCost p))) paths
   in take k $ sortBy (compare `on` fst) costs
{-# INLINEABLE shortestKPaths #-}

enumeratePaths :: [NonEmpty state] -> [[state]]
enumeratePaths (fs : next) = concatMap (\f -> map (f :) (enumeratePaths next)) fs
enumeratePaths [] = [[]]

-- enum' :: [NonEmpty state] -> [[(Int, state)]]
-- enum' = enumeratePaths . over (unsafePartsOf' (traversed . traversed)) (zip [0 ..])

-- data CostCache = C {
--   single :: Vector
-- }

pathCost ::
  ( Ord state
  , MonadTrans t
  , Unbox cost
  , Num cost
  , MonadCache state cost m
  , MonadCache (state, state) cost (t m)
  ) =>
  (state -> cost) ->
  (state -> state -> cost) ->
  [state] ->
  t m cost
pathCost singleCost transitionCost = go 0
  where
    go !acc = \case
      [] -> pure acc
      [state] -> (acc +) <$> memol1 (pure . singleCost) state
      (s1 : s2 : rest) -> do
        single <- memol1 (pure . singleCost) s1
        transition <- for2 memol0 (\x y -> pure (transitionCost x y)) s1 s2
        go (acc + single + transition) (s2 : rest)
{-# INLINEABLE pathCost #-}
