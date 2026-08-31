module PureScript.Lint.Rules.Branches.SameConstructorArm (SameConstructorArmConfig, sameConstructorArm) where

import Prelude

import Data.Foldable (any, fold)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Types
  ( Binder(..)
  , Expr(..)
  , Guarded(..)
  , Proper(..)
  , QualifiedName(..)
  , Separated(..)
  , Where(..)
  , Wrapped(..)
  )
import PureScript.Lint.Rule
  ( ExprLint
  , LintResult
  , violation
  , violations
  )

type SameConstructorArmConfig = { ctor :: String }

sameConstructorArm :: SameConstructorArmConfig -> ExprLint
sameConstructorArm config =
  { name: "same-constructor-arm"
  , description:
      "Flags a one-argument-constructor case arm that reconstructs the same constructor."
  , goodExample: Just "do x <- m\n   pure (f x)"
  , badExample: Just "case m of\n  Just x -> Just (f x)\n  Nothing -> Nothing"
  , rule: \_context expr -> case expr of
      ExprCase { branches } | any (isSelfReconstructingArm config) branches ->
        violation
          ( fold
              [ "a branch matching "
              , config.ctor
              , " reconstructs "
              , config.ctor
              , " around its result - consider map/lmap/rmap/bimap, or do-notation"
              ]
          )
      _ -> violations []
  }

type CaseBranch = Separated (Binder Void) /\ Guarded Void

-- | `unconditionalResultReconstructs`.
isSelfReconstructingArm :: SameConstructorArmConfig -> CaseBranch -> Boolean
isSelfReconstructingArm config (Separated { head } /\ guarded) =
  isConfiguredBinder config head && unconditionalResultReconstructs config guarded

isConfiguredBinder :: SameConstructorArmConfig -> Binder Void -> Boolean
isConfiguredBinder config = case _ of
  BinderConstructor (QualifiedName { name: Proper n }) [ _ ] -> n == config.ctor
  _ -> false

unconditionalResultReconstructs :: SameConstructorArmConfig -> Guarded Void -> Boolean
unconditionalResultReconstructs config = case _ of
  Unconditional _ (Where { bindings: Nothing, expr }) -> reconstructsConfiguredCtor config expr
  _ -> false

reconstructsConfiguredCtor :: SameConstructorArmConfig -> Expr Void -> Boolean
reconstructsConfiguredCtor config = case _ of
  ExprApp (ExprConstructor (QualifiedName { name: Proper n })) _ -> n == config.ctor
  ExprParens (Wrapped { value }) -> reconstructsConfiguredCtor config value
  _ -> false
