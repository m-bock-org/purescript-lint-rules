module PureScript.Lint.Rules.Nesting.MaxCallStackDepth (maxCallStackDepth) where

import Prelude

import Data.Array (mapMaybe) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import PureScript.CST.Types (Declaration(..), Ident(..), Module(..), ModuleBody(..), Name(..))
import PureScript.Lint.Graph (Graph, fromEdges, longestPath)
import PureScript.Lint.Graph.Components (condense)
import PureScript.Lint.Rule
  ( LintResult
  , ModuleLint
  , violation
  , violations
  )
import PureScript.Lint.Scope (BindingId(..), moduleReferences)

maxCallStackDepth :: Int -> ModuleLint
maxCallStackDepth maxHops =
  { name: "max-call-stack-depth"
  , description:
      "Flags a function whose longest local call chain exceeds the configured number of hops."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context cstModule -> violations (findings maxHops cstModule)
  }

findings :: Int -> Module Void -> Array String
findings maxHops cstModule@(Module { body: ModuleBody { decls } }) =
  let
    edges = map (\r -> r.from /\ r.to) (moduleReferences cstModule)
    condensed = condense (fromEdges edges)
  in
    Array.mapMaybe (checkHops maxHops condensed) (Array.mapMaybe topLevelName decls)

type Condensed = { graph :: Graph BindingId, representativeOf :: BindingId -> BindingId }

checkHops :: Int -> Condensed -> String -> Maybe String
checkHops maxHops condensed name =
  let
    n = longestPath condensed.graph (condensed.representativeOf (TopLevelBinding name))
    message = fold
      [ name, " is ", show n, " local call hops deep, over the max of ", show maxHops ]
  in
    if n > maxHops then Just message else Nothing

topLevelName :: Declaration Void -> Maybe String
topLevelName = case _ of
  DeclValue { name: Name { name: Ident n } } -> Just n
  _ -> Nothing
