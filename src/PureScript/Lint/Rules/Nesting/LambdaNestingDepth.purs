module PureScript.Lint.Rules.Nesting.LambdaNestingDepth (maxLambdaNestingDepth) where

import Prelude

import Control.Monad.State (State, execState, modify_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Traversal (defaultVisitorWithContextM, rewriteDeclWithContextM)
import PureScript.CST.Types (Declaration, Expr(..))
import PureScript.Lint.Rule (DeclarationLint, violations)

maxLambdaNestingDepth :: Int -> DeclarationLint
maxLambdaNestingDepth maxDepth =
  { name: "max-lambda-nesting-depth"
  , description:
      "Flags a lambda nested more than the configured depth inside other lambdas in the same declaration."
  , goodExample: Just "\\a b c -> f a b c"
  , badExample: Just "\\a -> \\b -> \\c -> f a b c"
  , rule: \_context decl -> violations (findings maxDepth decl)
  }

findings :: Int -> Declaration Void -> Array String
findings maxDepth decl = execState
  (rewriteDeclWithContextM (defaultVisitorWithContextM { onExpr = onExpr maxDepth }) 0 decl)
  []

onExpr :: Int -> Int -> Expr Void -> State (Array String) (Int /\ Expr Void)
onExpr maxDepth depth expr = case expr of
  ExprLambda _ -> do
    let depth' = depth + 1
    when (depth' > maxDepth) $ modify_
      (_ <> [ "lambda nested " <> show depth' <> " deep, over the max of " <> show maxDepth ])
    pure (depth' /\ expr)
  _ -> pure (depth /\ expr)
