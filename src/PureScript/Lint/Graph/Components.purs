module PureScript.Lint.Graph.Components
  ( componentsOf
  , condense
  ) where

import Prelude

import Data.Array (concatMap, cons, foldl, snoc, take, uncons) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe) as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import PureScript.Lint.Graph (Graph, edgesOf, fromEdges, successorsOf)

type Visit a = { seen :: Set a, order :: Array a }

type Assignment a = { assigned :: Set a, components :: Array (Set a) }

componentsOf :: ∀ a. Ord a => Graph a -> Array (Set a)
componentsOf graph =
  let
    nodes = nodesOf graph

    incoming from = map (\to -> to /\ from) (successorsOf graph from)
    reversed = fromEdges (Array.concatMap incoming nodes)

    visited = Array.foldl (visitForward graph) { seen: Set.empty, order: [] } nodes

    grouped = Array.foldl
      ( \acc node ->
          if Set.member node acc.assigned then acc
          else assign acc (reachable reversed acc.assigned node)
      )
      { assigned: Set.empty, components: [] }
      visited.order
  in
    grouped.components

assign :: ∀ a. Ord a => Assignment a -> Set a -> Assignment a
assign acc component =
  { assigned: Set.union acc.assigned component
  , components: Array.snoc acc.components component
  }

visitForward :: ∀ a. Ord a => Graph a -> Visit a -> a -> Visit a
visitForward graph state node
  | Set.member node state.seen = state
  | otherwise =
      let
        marked = state { seen = Set.insert node state.seen }
        descended = Array.foldl (visitForward graph) marked (successorsOf graph node)
      in
        descended { order = Array.cons node descended.order }

reachable :: ∀ a. Ord a => Graph a -> Set a -> a -> Set a
reachable graph excluded start = walkFrom { graph, excluded } [ start ] Set.empty

type Walk a = { graph :: Graph a, excluded :: Set a }

walkFrom :: ∀ a. Ord a => Walk a -> Array a -> Set a -> Set a
walkFrom ctx frontier found = Maybe.maybe found
  ( \{ head, tail } ->
      if Set.member head found || Set.member head ctx.excluded then walkFrom ctx tail found
      else walkFrom ctx (tail <> successorsOf ctx.graph head) (Set.insert head found)
  )
  (Array.uncons frontier)

nodesOf :: ∀ a. Ord a => Graph a -> Array a
nodesOf graph =
  let
    edges = edgesOf graph
    sources = Set.fromFoldable (Map.keys edges)
    targets = Set.unions (Map.values edges)
  in
    Set.toUnfoldable (Set.union sources targets)

condense :: ∀ a. Ord a => Graph a -> { graph :: Graph a, representativeOf :: a -> a }
condense graph =
  let
    components = componentsOf graph

    representatives :: Map a a
    representatives = Map.fromFoldable do
      component <- components
      let members = Set.toUnfoldable component :: Array a
      leader <- Array.take 1 members
      map (\member -> member /\ leader) members

    representativeOf node = Maybe.fromMaybe node (Map.lookup node representatives)

    collapsed = do
      component <- components
      member <- Set.toUnfoldable component :: Array a
      successor <- successorsOf graph member
      let from = representativeOf member
      let to = representativeOf successor
      if from == to then [] else [ from /\ to ]
  in
    { graph: fromEdges collapsed, representativeOf }
