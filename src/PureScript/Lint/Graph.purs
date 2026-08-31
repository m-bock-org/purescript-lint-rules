module PureScript.Lint.Graph
  ( Graph
  , edgesOf
  , fromEdges
  , longestPath
  , successors
  , successorsOf
  ) where

import Prelude

import Data.Foldable (maximum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe) as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))

newtype Graph a = Graph (Map a (Set a))

fromEdges :: ∀ a. Ord a => Array (a /\ a) -> Graph a
fromEdges edges =
  let
    pairs = map (\(from /\ to) -> from /\ Set.singleton to) edges
  in
    Graph (Map.fromFoldableWith Set.union pairs)

successors :: ∀ a. Ord a => Graph a -> a -> Set a
successors (Graph edges) node = Maybe.fromMaybe Set.empty (Map.lookup node edges)

successorsOf :: ∀ a. Ord a => Graph a -> a -> Array a
successorsOf graph node = Set.toUnfoldable (successors graph node)

edgesOf :: ∀ a. Graph a -> Map a (Set a)
edgesOf (Graph edges) = edges

longestPath :: ∀ a. Ord a => Graph a -> a -> Int
longestPath graph = longestPathFrom graph Set.empty

longestPathFrom :: ∀ a. Ord a => Graph a -> Set a -> a -> Int
longestPathFrom graph visited node =
  let
    walked = Set.insert node visited
    hopTo callee =
      if Set.member callee walked then 0 else 1 + longestPathFrom graph walked callee
    callees = successorsOf graph node
  in
    Maybe.fromMaybe 0 (maximum (map hopTo callees))
