module PureScript.Lint.Scope
  ( BindingId(..)
  , Reference
  , bindingName
  , moduleReferences
  ) where

import Prelude

import Data.Array (concatMap, cons, foldl, head, mapMaybe, null) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA
import Data.Const (Const(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapBinder, traverseExpr)
import PureScript.CST.Types
  ( Binder(..)
  , Declaration(..)
  , DoStatement(..)
  , Expr(..)
  , Guarded(..)
  , GuardedExpr(..)
  , Ident(..)
  , Labeled(..)
  , LetBinding(..)
  , Module(..)
  , ModuleBody(..)
  , Name(..)
  , PatternGuard(..)
  , QualifiedName(..)
  , Separated(..)
  , Type(..)
  , ValueBindingFields
  , Where(..)
  , Wrapped(..)
  )

data BindingId
  = TopLevelBinding String
  | LocalBinding { name :: String, line :: Int, column :: Int }

derive instance eqBindingId :: Eq BindingId
derive instance ordBindingId :: Ord BindingId

type Reference = { from :: BindingId, to :: BindingId }

type Scope = Map String BindingId

bindingName :: BindingId -> String
bindingName = case _ of
  TopLevelBinding n -> n
  LocalBinding { name } -> name

moduleReferences :: Module Void -> Array Reference
moduleReferences (Module { body: ModuleBody { decls } }) =
  let
    callables = Array.mapMaybe (callableDeclName decls) decls

    topLevelScope :: Scope
    topLevelScope = Map.fromFoldable (map (\n -> n /\ TopLevelBinding n) callables)
  in
    Array.concatMap
      ( case _ of
          DeclValue fields ->
            valueBindingReferences topLevelScope (TopLevelBinding $ identName fields.name)
              fields
          _ -> []
      )
      decls

callableDeclName :: Array (Declaration Void) -> Declaration Void -> Maybe String
callableDeclName decls = case _ of
  DeclValue fields -> callableName decls fields
  _ -> Nothing

-- | `isFunctionType`, `signatureOf`.
callableName :: Array (Declaration Void) -> ValueBindingFields Void -> Maybe String
callableName decls fields =
  let
    name = identName fields.name
    callable = isCallable fields || Maybe.maybe true isFunctionType (signatureOf decls name)
  in
    if callable then Just name else Nothing

signatureOf :: Array (Declaration Void) -> String -> Maybe (Type Void)
signatureOf decls name = Array.head $ Array.mapMaybe
  ( case _ of
      DeclSignature (Labeled { label: Name { name: Ident n }, value }) ->
        if n == name then Just value else Nothing
      _ -> Nothing
  )
  decls

isFunctionType :: Type Void -> Boolean
isFunctionType = case _ of
  TypeArrow _ _ _ -> true
  TypeForall _ _ _ t -> isFunctionType t
  TypeConstrained _ _ t -> isFunctionType t
  TypeKinded t _ _ -> isFunctionType t
  TypeParens (Wrapped { value }) -> isFunctionType value
  _ -> false

identName :: Name Ident -> String
identName (Name { name: Ident n }) = n

localIdOf :: Name Ident -> BindingId
localIdOf (Name { token, name: Ident n }) = LocalBinding
  { name: n, line: token.range.start.line, column: token.range.start.column }

binderNames :: Binder Void -> Array String
binderNames = foldMapBinder
  ( defaultMonoidalVisitor
      { onBinder = case _ of
          BinderVar (Name { name: Ident n }) -> [ n ]
          BinderNamed (Name { name: Ident n }) _ _ -> [ n ]
          _ -> []
      }
  )

lambdaNames :: NonEmptyArray (Binder Void) -> Array String
lambdaNames binders = Array.concatMap binderNames (NEA.toArray binders)

shadowAll :: Array String -> Scope -> Scope
shadowAll names scope = Array.foldl (flip Map.delete) scope names

valueBindingReferences :: Scope -> BindingId -> ValueBindingFields Void -> Array Reference
valueBindingReferences scope current fields =
  guardedReferences (shadowAll (Array.concatMap binderNames fields.binders) scope)
    current
    fields.guarded

-- | `whereReferences`.
guardedExprReferences :: Scope -> BindingId -> GuardedExpr Void -> Array Reference
guardedExprReferences scope current (GuardedExpr g) =
  let
    Separated { head, tail } = g.patterns
    step acc pg =
      { scope: patternGuardScope acc.scope pg
      , refs: acc.refs <> patternGuardRefs current acc.scope pg
      }
    seeded = Array.foldl step { scope, refs: [] } (Array.cons head (map Tuple.snd tail))
  in
    seeded.refs <> whereReferences seeded.scope current g.where

patternGuardRefs :: BindingId -> Scope -> PatternGuard Void -> Array Reference
patternGuardRefs current sc (PatternGuard { expr }) = exprReferences sc current expr

patternGuardScope :: Scope -> PatternGuard Void -> Scope
patternGuardScope sc (PatternGuard { binder }) =
  Maybe.maybe sc (\(Tuple b _) -> shadowAll (binderNames b) sc) binder

guardedReferences :: Scope -> BindingId -> Guarded Void -> Array Reference
guardedReferences scope current =
  case _ of
    Unconditional _ w -> whereReferences scope current w
    Guarded ges -> Array.concatMap (guardedExprReferences scope current) (NEA.toArray ges)

whereReferences :: Scope -> BindingId -> Where Void -> Array Reference
whereReferences scope current (Where { expr, bindings }) = Maybe.maybe
  (exprReferences scope current expr)
  ( \(_ /\ bs) ->
      let
        inner = extendWithLetBindings scope (NEA.toArray bs)
      in
        letBindingReferences inner current (NEA.toArray bs) <> exprReferences inner current expr
  )
  bindings

isCallable :: ValueBindingFields Void -> Boolean
isCallable fields = not (Array.null fields.binders)

extendWithLetBindings :: Scope -> Array (LetBinding Void) -> Scope
extendWithLetBindings = Array.foldl
  ( \scope -> case _ of
      LetBindingName fields
        | isCallable fields -> Map.insert (identName fields.name) (localIdOf fields.name) scope
        | otherwise -> Map.delete (identName fields.name) scope
      LetBindingPattern binder _ _ -> shadowAll (binderNames binder) scope
      _ -> scope
  )

letBindingReferences :: Scope -> BindingId -> Array (LetBinding Void) -> Array Reference
letBindingReferences scope current = Array.concatMap case _ of
  LetBindingName fields
    | isCallable fields -> valueBindingReferences scope (localIdOf fields.name) fields
    | otherwise -> valueBindingReferences scope current fields
  LetBindingPattern _ _ w -> whereReferences scope current w
  _ -> []

type CaseBranch = Separated (Binder Void) /\ Guarded Void

branchReferences :: Scope -> BindingId -> CaseBranch -> Array Reference
branchReferences scope current (Separated { head: b, tail: bt } /\ guarded) =
  let
    names = Array.concatMap binderNames (Array.cons b (map Tuple.snd bt))
  in
    guardedReferences (shadowAll names scope) current guarded

-- | `branchReferences`, `doReferences`.
exprReferences :: Scope -> BindingId -> Expr Void -> Array Reference
exprReferences scope current expr = case expr of
  ExprIdent (QualifiedName { module: Nothing, name: Ident n }) ->
    Maybe.maybe [] (\to -> [ { from: current, to } ]) (Map.lookup n scope)

  ExprLet { bindings, body } ->
    let
      inner = extendWithLetBindings scope (NEA.toArray bindings)
    in
      letBindingReferences inner current (NEA.toArray bindings)
        <> exprReferences inner current body

  ExprLambda { binders, body } ->
    exprReferences (shadowAll (lambdaNames binders) scope) current body

  ExprCase { head, branches } ->
    let
      Separated { head: h, tail: t } = head
      headRefs = Array.concatMap (exprReferences scope current)
        (Array.cons h (map Tuple.snd t))
    in
      headRefs <> Array.concatMap (branchReferences scope current) (NEA.toArray branches)

  ExprDo { statements } -> doReferences scope current (NEA.toArray statements)

  other -> un Const
    ( traverseExpr
        { onExpr: \e -> Const (exprReferences scope current e)
        , onBinder: pure
        , onType: pure
        }
        other
    )

doReferences :: Scope -> BindingId -> Array (DoStatement Void) -> Array Reference
doReferences scope current statements =
  (Array.foldl (doStatementStep current) { scope, refs: [] } statements).refs

type ScopeAcc = { scope :: Scope, refs :: Array Reference }

-- | `binderNames`, `letStatementStep`.
doStatementStep :: BindingId -> ScopeAcc -> DoStatement Void -> ScopeAcc
doStatementStep current acc = case _ of
  DoDiscard e -> acc { refs = acc.refs <> exprReferences acc.scope current e }
  DoBind binder _ e -> acc
    { scope = shadowAll (binderNames binder) acc.scope
    , refs = acc.refs <> exprReferences acc.scope current e
    }
  DoLet _ bs -> letStatementStep current acc (NEA.toArray bs)
  _ -> acc

-- | `letBindingReferences`.
letStatementStep :: BindingId -> ScopeAcc -> Array (LetBinding Void) -> ScopeAcc
letStatementStep current acc bs =
  let
    inner = extendWithLetBindings acc.scope bs
  in
    acc { scope = inner, refs = acc.refs <> letBindingReferences inner current bs }
