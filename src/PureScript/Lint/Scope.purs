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
import PureScript.CST.Types as CST

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

moduleReferences :: CST.Module Void -> Array Reference
moduleReferences (CST.Module { body: CST.ModuleBody { decls } }) =
  let
    callables = Array.mapMaybe (callableDeclName decls) decls

    topLevelScope :: Scope
    topLevelScope = Map.fromFoldable (map (\n -> n /\ TopLevelBinding n) callables)
  in
    Array.concatMap
      ( case _ of
          CST.DeclValue fields ->
            valueBindingReferences topLevelScope (TopLevelBinding $ identName fields.name)
              fields
          _ -> []
      )
      decls

callableDeclName :: Array (CST.Declaration Void) -> CST.Declaration Void -> Maybe String
callableDeclName decls = case _ of
  CST.DeclValue fields -> callableName decls fields
  _ -> Nothing

-- | `isFunctionType`, `signatureOf`.
callableName :: Array (CST.Declaration Void) -> CST.ValueBindingFields Void -> Maybe String
callableName decls fields =
  let
    name = identName fields.name
    callable = isCallable fields || Maybe.maybe true isFunctionType (signatureOf decls name)
  in
    if callable then Just name else Nothing

signatureOf :: Array (CST.Declaration Void) -> String -> Maybe (CST.Type Void)
signatureOf decls name = Array.head $ Array.mapMaybe
  ( case _ of
      CST.DeclSignature (CST.Labeled { label: CST.Name { name: CST.Ident n }, value }) ->
        if n == name then Just value else Nothing
      _ -> Nothing
  )
  decls

isFunctionType :: CST.Type Void -> Boolean
isFunctionType = case _ of
  CST.TypeArrow _ _ _ -> true
  CST.TypeForall _ _ _ t -> isFunctionType t
  CST.TypeConstrained _ _ t -> isFunctionType t
  CST.TypeKinded t _ _ -> isFunctionType t
  CST.TypeParens (CST.Wrapped { value }) -> isFunctionType value
  _ -> false

identName :: CST.Name CST.Ident -> String
identName (CST.Name { name: CST.Ident n }) = n

localIdOf :: CST.Name CST.Ident -> BindingId
localIdOf (CST.Name { token, name: CST.Ident n }) = LocalBinding
  { name: n, line: token.range.start.line, column: token.range.start.column }

binderNames :: CST.Binder Void -> Array String
binderNames = foldMapBinder
  ( defaultMonoidalVisitor
      { onBinder = case _ of
          CST.BinderVar (CST.Name { name: CST.Ident n }) -> [ n ]
          CST.BinderNamed (CST.Name { name: CST.Ident n }) _ _ -> [ n ]
          _ -> []
      }
  )

lambdaNames :: NonEmptyArray (CST.Binder Void) -> Array String
lambdaNames binders = Array.concatMap binderNames (NEA.toArray binders)

shadowAll :: Array String -> Scope -> Scope
shadowAll names scope = Array.foldl (flip Map.delete) scope names

valueBindingReferences :: Scope -> BindingId -> CST.ValueBindingFields Void -> Array Reference
valueBindingReferences scope current fields =
  guardedReferences (shadowAll (Array.concatMap binderNames fields.binders) scope)
    current
    fields.guarded

-- | `whereReferences`.
guardedExprReferences :: Scope -> BindingId -> CST.GuardedExpr Void -> Array Reference
guardedExprReferences scope current (CST.GuardedExpr g) =
  let
    CST.Separated { head, tail } = g.patterns
    step acc pg =
      { scope: patternGuardScope acc.scope pg
      , refs: acc.refs <> patternGuardRefs current acc.scope pg
      }
    seeded = Array.foldl step { scope, refs: [] } (Array.cons head (map Tuple.snd tail))
  in
    seeded.refs <> whereReferences seeded.scope current g.where

patternGuardRefs :: BindingId -> Scope -> CST.PatternGuard Void -> Array Reference
patternGuardRefs current sc (CST.PatternGuard { expr }) = exprReferences sc current expr

patternGuardScope :: Scope -> CST.PatternGuard Void -> Scope
patternGuardScope sc (CST.PatternGuard { binder }) =
  Maybe.maybe sc (\(Tuple b _) -> shadowAll (binderNames b) sc) binder

guardedReferences :: Scope -> BindingId -> CST.Guarded Void -> Array Reference
guardedReferences scope current =
  case _ of
    CST.Unconditional _ w -> whereReferences scope current w
    CST.Guarded ges -> Array.concatMap (guardedExprReferences scope current) (NEA.toArray ges)

whereReferences :: Scope -> BindingId -> CST.Where Void -> Array Reference
whereReferences scope current (CST.Where { expr, bindings }) = Maybe.maybe
  (exprReferences scope current expr)
  ( \(_ /\ bs) ->
      let
        inner = extendWithLetBindings scope (NEA.toArray bs)
      in
        letBindingReferences inner current (NEA.toArray bs) <> exprReferences inner current expr
  )
  bindings

isCallable :: CST.ValueBindingFields Void -> Boolean
isCallable fields = not (Array.null fields.binders)

extendWithLetBindings :: Scope -> Array (CST.LetBinding Void) -> Scope
extendWithLetBindings = Array.foldl
  ( \scope -> case _ of
      CST.LetBindingName fields
        | isCallable fields -> Map.insert (identName fields.name) (localIdOf fields.name) scope
        | otherwise -> Map.delete (identName fields.name) scope
      CST.LetBindingPattern binder _ _ -> shadowAll (binderNames binder) scope
      _ -> scope
  )

letBindingReferences :: Scope -> BindingId -> Array (CST.LetBinding Void) -> Array Reference
letBindingReferences scope current = Array.concatMap case _ of
  CST.LetBindingName fields
    | isCallable fields -> valueBindingReferences scope (localIdOf fields.name) fields
    | otherwise -> valueBindingReferences scope current fields
  CST.LetBindingPattern _ _ w -> whereReferences scope current w
  _ -> []

type CaseBranch = CST.Separated (CST.Binder Void) /\ CST.Guarded Void

branchReferences :: Scope -> BindingId -> CaseBranch -> Array Reference
branchReferences scope current (CST.Separated { head: b, tail: bt } /\ guarded) =
  let
    names = Array.concatMap binderNames (Array.cons b (map Tuple.snd bt))
  in
    guardedReferences (shadowAll names scope) current guarded

-- | `branchReferences`, `doReferences`.
exprReferences :: Scope -> BindingId -> CST.Expr Void -> Array Reference
exprReferences scope current expr = case expr of
  CST.ExprIdent (CST.QualifiedName { module: Nothing, name: CST.Ident n }) ->
    Maybe.maybe [] (\to -> [ { from: current, to } ]) (Map.lookup n scope)

  CST.ExprLet { bindings, body } ->
    let
      inner = extendWithLetBindings scope (NEA.toArray bindings)
    in
      letBindingReferences inner current (NEA.toArray bindings)
        <> exprReferences inner current body

  CST.ExprLambda { binders, body } ->
    exprReferences (shadowAll (lambdaNames binders) scope) current body

  CST.ExprCase { head, branches } ->
    let
      CST.Separated { head: h, tail: t } = head
      headRefs = Array.concatMap (exprReferences scope current)
        (Array.cons h (map Tuple.snd t))
    in
      headRefs <> Array.concatMap (branchReferences scope current) (NEA.toArray branches)

  CST.ExprDo { statements } -> doReferences scope current (NEA.toArray statements)

  other -> un Const
    ( traverseExpr
        { onExpr: \e -> Const (exprReferences scope current e)
        , onBinder: pure
        , onType: pure
        }
        other
    )

doReferences :: Scope -> BindingId -> Array (CST.DoStatement Void) -> Array Reference
doReferences scope current statements =
  (Array.foldl (doStatementStep current) { scope, refs: [] } statements).refs

type ScopeAcc = { scope :: Scope, refs :: Array Reference }

-- | `binderNames`, `letStatementStep`.
doStatementStep :: BindingId -> ScopeAcc -> CST.DoStatement Void -> ScopeAcc
doStatementStep current acc = case _ of
  CST.DoDiscard e -> acc { refs = acc.refs <> exprReferences acc.scope current e }
  CST.DoBind binder _ e -> acc
    { scope = shadowAll (binderNames binder) acc.scope
    , refs = acc.refs <> exprReferences acc.scope current e
    }
  CST.DoLet _ bs -> letStatementStep current acc (NEA.toArray bs)
  _ -> acc

-- | `letBindingReferences`.
letStatementStep :: BindingId -> ScopeAcc -> Array (CST.LetBinding Void) -> ScopeAcc
letStatementStep current acc bs =
  let
    inner = extendWithLetBindings acc.scope bs
  in
    acc { scope = inner, refs = acc.refs <> letBindingReferences inner current bs }
