module Lint.Rules.Width.TypeLines (typeLines) where

import Prelude

import Data.Array (concatMap, filter, fromFoldable) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\))
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList (toArray) as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types
  ( Binder(..)
  , Declaration(..)
  , DoStatement(..)
  , Expr(..)
  , Guarded(..)
  , GuardedExpr(..)
  , Instance(..)
  , InstanceBinding(..)
  , LetBinding(..)
  , Module
  , Separated(..)
  , SourcePos
  , SourceToken
  , Where(..)
  )

----------------------------------------------------------------------------------------------------
-- Type lines
----------------------------------------------------------------------------------------------------

-- | The lines of a module whose every token lies in a type, a signature or a type declaration.
typeLines :: Module Void -> Set Int
typeLines cstModule =
  let
    typed :: Set SourcePos
    typed =
      Set.fromFoldable
        ( map _.range.start
            ( foldMapModule
                ( defaultMonoidalVisitor
                    { onDecl = declTypeTokens
                    , onExpr = exprTypeTokens
                    , onBinder = binderTypeTokens
                    }
                )
                cstModule
            )
        )

    tokens :: Array SourceToken
    tokens = TokenList.toArray (tokensOf cstModule)

    code :: Array SourceToken
    code = Array.filter (\(token :: SourceToken) -> not (Set.member token.range.start typed)) tokens
  in
    Set.difference
      (Set.fromFoldable (map _.range.end.line tokens))
      (Set.fromFoldable (map _.range.end.line code))

----------------------------------------------------------------------------------------------------
-- Declarations
----------------------------------------------------------------------------------------------------

-- | The type tokens of a declaration: all of a type-level one, the `where` signatures of a value.
declTypeTokens :: Declaration Void -> Array SourceToken
declTypeTokens decl = case decl of
  DeclSignature _ -> TokenList.toArray (tokensOf decl)
  DeclType _ _ _ -> TokenList.toArray (tokensOf decl)
  DeclNewtype _ _ _ _ -> TokenList.toArray (tokensOf decl)
  DeclData _ _ -> TokenList.toArray (tokensOf decl)
  DeclClass _ _ -> TokenList.toArray (tokensOf decl)
  DeclKindSignature _ _ -> TokenList.toArray (tokensOf decl)
  DeclForeign _ _ _ -> TokenList.toArray (tokensOf decl)
  DeclDerive _ _ _ -> TokenList.toArray (tokensOf decl)
  DeclRole _ _ _ _ -> TokenList.toArray (tokensOf decl)
  DeclValue fields -> guardedTypeTokens fields.guarded
  DeclInstanceChain chain -> instanceTypeTokens chain
  _ -> []

-- | The type tokens of an instance chain: each head, each `where`, the methods' signatures.
instanceTypeTokens :: Separated (Instance Void) -> Array SourceToken
instanceTypeTokens (Separated chain) =
  let
    instances :: Array (Instance Void)
    instances = [ chain.head ] <> map Tuple.snd chain.tail

    heads :: Array SourceToken
    heads =
      Array.concatMap
        (\(Instance one) -> TokenList.toArray (tokensOf (Instance one { body = Nothing })))
        instances

    bodies :: Array (SourceToken /\ NonEmptyArray (InstanceBinding Void))
    bodies = Array.concatMap (\(Instance one) -> Array.fromFoldable one.body) instances
  in
    heads
      <> map Tuple.fst bodies
      <> Array.concatMap
        ( \(method :: InstanceBinding Void) -> case method of
            InstanceBindingSignature labeled -> TokenList.toArray (tokensOf labeled)
            InstanceBindingName fields -> guardedTypeTokens fields.guarded
        )
        (Array.concatMap NEA.toArray (map Tuple.snd bodies))

----------------------------------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------------------------------

-- | The type tokens an expression holds itself: an annotation, its `let` and `where` signatures.
exprTypeTokens :: Expr Void -> Array SourceToken
exprTypeTokens = case _ of
  ExprTyped _ separator ty ->
    [ separator ] <> TokenList.toArray (tokensOf ty)
  ExprLet { bindings } ->
    Array.concatMap letTypeTokens (NEA.toArray bindings)
  ExprDo { statements } ->
    Array.concatMap letTypeTokens (Array.concatMap letBindings (NEA.toArray statements))
  ExprAdo { statements } ->
    Array.concatMap letTypeTokens (Array.concatMap letBindings statements)
  ExprCase { branches } ->
    Array.concatMap guardedTypeTokens (map Tuple.snd (NEA.toArray branches))
  _ ->
    []

-- | The bindings of a `let` statement in a `do` or `ado` block.
letBindings :: DoStatement Void -> Array (LetBinding Void)
letBindings = case _ of
  DoLet _ bindings -> NEA.toArray bindings
  _ -> []

-- | The type tokens of a `let` binding: a signature whole, a binding's `where` signatures.
letTypeTokens :: LetBinding Void -> Array SourceToken
letTypeTokens binding = case binding of
  LetBindingSignature _ -> TokenList.toArray (tokensOf binding)
  LetBindingName fields -> guardedTypeTokens fields.guarded
  _ -> []

-- | The `::` and type of an annotated binder.
binderTypeTokens :: Binder Void -> Array SourceToken
binderTypeTokens = case _ of
  BinderTyped _ separator ty -> [ separator ] <> TokenList.toArray (tokensOf ty)
  _ -> []

----------------------------------------------------------------------------------------------------
-- Where clauses
----------------------------------------------------------------------------------------------------

-- | The signatures among the `where` bindings of a right-hand side, guarded or not.
guardedTypeTokens :: Guarded Void -> Array SourceToken
guardedTypeTokens guarded =
  let
    clauses :: Array (Where Void)
    clauses = case guarded of
      Unconditional _ clause -> [ clause ]
      Guarded guards -> map (\(GuardedExpr one) -> one."where") (NEA.toArray guards)

    found :: Array (NonEmptyArray (LetBinding Void))
    found =
      Array.concatMap
        (\(Where clause) -> Array.fromFoldable (map Tuple.snd clause.bindings))
        clauses

    bindings :: Array (LetBinding Void)
    bindings = Array.concatMap NEA.toArray found
  in
    Array.concatMap
      ( \(binding :: LetBinding Void) -> case binding of
          LetBindingSignature _ -> TokenList.toArray (tokensOf binding)
          _ -> []
      )
      bindings

-- ## Context
--
-- m-bock, 2026-10-22: every line that is a type is read at the type
-- width, not only a top-level signature - a `let` or `where`
-- signature, a synonym, a newtype, a class member, a record field.
-- `single-line-let-signature` and `single-line-field-type` put such a
-- type on one line, and at the code width the two rules pulled
-- against this one.
--
-- A line counts by its tokens: it is a type line when every token on
-- it lies in a type. `(crate :: Crate) <- …` holds code, and stays at
-- the code width; a type annotation's continuation line, all type,
-- does not. A `where` binding's own `where` is not reached: the house
-- writes no `where`, and one level covers the rest of the fleet.
