module PureScript.Lint.Rules.Naming.NoStutteringName (noStutteringName) where

import Prelude

import Data.Array (nub, null) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust) as Maybe
import Data.String (Pattern(..))
import Data.String (joinWith, stripPrefix) as Str
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types
  ( Binder(..)
  , Expr(..)
  , Ident(..)
  , Module
  , ModuleName(..)
  , Operator(..)
  , Proper(..)
  , QualifiedName(..)
  , Type(..)
  )
import PureScript.Lint.Rule
  ( LintResult
  , ModuleLint
  , violation
  , violations
  , withHint
  )

noStutteringName :: ModuleLint
noStutteringName =
  { name: "no-stuttering-name"
  , description:
      "Flags a qualified name whose own name repeats its qualifier, like `Kraken.KrakenPair`."
  , goodExample: Just "Kraken.Pair"
  , badExample: Just "Kraken.KrakenPair"
  , rule: \_context cstModule -> report (Array.nub (stutters cstModule))
  }

stutters :: Module Void -> Array String
stutters = foldMapModule
  ( defaultMonoidalVisitor
      { onExpr = fromExpr, onType = fromType, onBinder = fromBinder }
  )

-- | `unOperator`.
fromExpr :: Expr Void -> Array String
fromExpr = case _ of
  ExprIdent qn -> stuttering unIdent qn
  ExprConstructor qn -> stuttering unProper qn
  ExprOpName qn -> stuttering unOperator qn
  _ -> []

fromType :: Type Void -> Array String
fromType = case _ of
  TypeConstructor qn -> stuttering unProper qn
  TypeOpName qn -> stuttering unOperator qn
  _ -> []

fromBinder :: Binder Void -> Array String
fromBinder = case _ of
  BinderConstructor qn _ -> stuttering unProper qn
  _ -> []

stuttering :: ∀ a. (a -> String) -> QualifiedName a -> Array String
stuttering nameText (QualifiedName q) = case q.module of
  Just (ModuleName alias)
    | Maybe.isJust (Str.stripPrefix (Pattern alias) (nameText q.name)) ->
        [ alias <> "." <> nameText q.name ]
  _ -> []

unIdent :: Ident -> String
unIdent (Ident n) = n

unProper :: Proper -> String
unProper (Proper n) = n

unOperator :: Operator -> String
unOperator (Operator n) = n

report :: ∀ a. Array String -> LintResult a
report found = withHint hint
  (violations (map (_ <> " repeats the qualifier in its own name") found))
  where
  hint = "drop the prefix from the name, or import it unqualified"
