module PureScript.Lint.Rules.Naming.NoStutteringName (noStutteringName) where

import Prelude

import Data.Array (nub) as Array
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust) as Maybe
import Data.String (Pattern(..))
import Data.String (stripPrefix) as Str
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types as CST
import PureScript.Lint.Rule
  ( LintResult
  , ModuleLint
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

stutters :: CST.Module Void -> Array String
stutters = foldMapModule
  ( defaultMonoidalVisitor
      { onExpr = fromExpr, onType = fromType, onBinder = fromBinder }
  )

-- | `unOperator`.
fromExpr :: CST.Expr Void -> Array String
fromExpr = case _ of
  CST.ExprIdent qn -> stuttering unIdent qn
  CST.ExprConstructor qn -> stuttering unProper qn
  CST.ExprOpName qn -> stuttering unOperator qn
  _ -> []

fromType :: CST.Type Void -> Array String
fromType = case _ of
  CST.TypeConstructor qn -> stuttering unProper qn
  CST.TypeOpName qn -> stuttering unOperator qn
  _ -> []

fromBinder :: CST.Binder Void -> Array String
fromBinder = case _ of
  CST.BinderConstructor qn _ -> stuttering unProper qn
  _ -> []

stuttering :: ∀ a. (a -> String) -> CST.QualifiedName a -> Array String
stuttering nameText (CST.QualifiedName q) = case q.module of
  Just (CST.ModuleName alias)
    | Maybe.isJust (Str.stripPrefix (Pattern alias) (nameText q.name)) ->
        [ alias <> "." <> nameText q.name ]
  _ -> []

unIdent :: CST.Ident -> String
unIdent (CST.Ident n) = n

unProper :: CST.Proper -> String
unProper (CST.Proper n) = n

unOperator :: CST.Operator -> String
unOperator (CST.Operator n) = n

report :: ∀ a. Array String -> LintResult a
report found = withHint hint
  (violations (map (_ <> " repeats the qualifier in its own name") found))
  where
  hint = "drop the prefix from the name, or import it unqualified"
