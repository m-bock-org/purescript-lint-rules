module Lint.Rules.Naming.NoStutteringName (noStutteringName) where

import Prelude

import Data.Array (nub) as Array
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust) as Maybe
import Data.String (Pattern(..))
import Data.String (stripPrefix) as Str
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types as CST
import Lint.Rule
  ( LintResult
  , ModuleLint
  , violations
  , withHint
  )

-- | Uses `report`, `stutters`.
noStutteringName :: ModuleLint Unit
noStutteringName =
  { name: "no-stuttering-name"
  , description:
      "Flags a qualified name whose own name repeats its qualifier."
  , examples: Just
      { config: unit
      , printConfig: \_ -> Nothing
      , good: [ "Parser.Token" ]
      , bad: [ "Parser.ParserToken" ]
      }
  , rule: \_config _context cstModule -> report (Array.nub (stutters cstModule))
  }

-- | Private. Used only by `noStutteringName`. Uses `fromExpr`, `fromType`, `fromBinder`.
stutters :: CST.Module Void -> Array String
stutters = foldMapModule
  ( defaultMonoidalVisitor
      { onExpr = fromExpr, onType = fromType, onBinder = fromBinder }
  )

-- | Private, depth 2. Used only by `stutters`. Uses `stuttering`, `unIdent`, `unProper`,
-- | `unOperator`.
fromExpr :: CST.Expr Void -> Array String
fromExpr = case _ of
  CST.ExprIdent qn -> stuttering unIdent qn
  CST.ExprConstructor qn -> stuttering unProper qn
  CST.ExprOpName qn -> stuttering unOperator qn
  _ -> []

-- | Private, depth 2. Used only by `stutters`. Uses `stuttering`, `unProper`, `unOperator`.
fromType :: CST.Type Void -> Array String
fromType = case _ of
  CST.TypeConstructor qn -> stuttering unProper qn
  CST.TypeOpName qn -> stuttering unOperator qn
  _ -> []

-- | Private, depth 2. Used only by `stutters`. Uses `stuttering`, `unProper`.
fromBinder :: CST.Binder Void -> Array String
fromBinder = case _ of
  CST.BinderConstructor qn _ -> stuttering unProper qn
  _ -> []

-- | Private.
stuttering :: ∀ a. (a -> String) -> CST.QualifiedName a -> Array String
stuttering nameText (CST.QualifiedName q) = case q.module of
  Just (CST.ModuleName alias)
    | Maybe.isJust (Str.stripPrefix (Pattern alias) (nameText q.name)) ->
        [ alias <> "." <> nameText q.name ]
  _ -> []

-- | Private, depth 3. Used only by `fromExpr`.
unIdent :: CST.Ident -> String
unIdent (CST.Ident n) = n

-- | Private.
unProper :: CST.Proper -> String
unProper (CST.Proper n) = n

-- | Private.
unOperator :: CST.Operator -> String
unOperator (CST.Operator n) = n

-- | Private. Used only by `noStutteringName`.
report :: ∀ a. Array String -> LintResult a
report found = withHint hint
  (violations (map (_ <> " repeats the qualifier in its own name") found))
  where
  hint = "drop the prefix from the name, or import it unqualified"
