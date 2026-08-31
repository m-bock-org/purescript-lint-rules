module Lint.Rules.Size.MaxFunctionArity (maxFunctionArity) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (length) as Array
import Data.Foldable (fold)
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import Lint.Rule (DeclarationLint, violations, withHint)

maxFunctionArity :: DeclarationLint Int
maxFunctionArity =
  { name: "max-function-arity"
  , description:
      "Flags a top-level definition binding more parameters than the configured maximum. Counts the binders written in the equation, which need not match the arrows in its type."
  , examples: Just
      { config: 3
      , printConfig: \n -> Just ("max arity " <> show n)
      , good: [ "resize { width, height, quality } img = img" ]
      , bad: [ "resize width height quality img = img" ]
      }
  , rule: \maxArity _context decl -> case decl of
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
