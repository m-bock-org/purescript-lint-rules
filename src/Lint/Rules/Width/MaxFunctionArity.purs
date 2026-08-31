module Lint.Rules.Width.MaxFunctionArity (maxFunctionArity) where

import Prelude

import Data.Array (length) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import Lint.Rule (DeclarationLint, violations, withHint)

maxFunctionArity :: Int -> DeclarationLint
maxFunctionArity maxArity =
  { name: "max-function-arity"
  , description:
      "Flags a top-level function definition with more than the configured number of arguments."
  , goodExamples: [ "resize { width, height, quality } img = ..." ]
  , badExamples: [ "resize width height quality img = ..." ]
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity ->
            withHint
              "group related ones into a record, or a tuple where they have no good names"
              ( violations
                  [ fold
                      [ n
                      , " takes "
                      , show (Array.length binders)
                      , " arguments, over the max of "
                      , show maxArity
                      ]
                  ]
              )
      _ -> violations []
  }
