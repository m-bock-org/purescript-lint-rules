module Lint.Rules.Size.MaxLineLength (maxLineLength) where

import Prelude

import Data.Array (concatMap, filter, mapMaybe, range) as Array
import Data.Foldable (fold, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList (toArray) as TokenList
import PureScript.CST.Types (Declaration(..), Module(..), ModuleBody(..), SourceToken, Token(..))
import Lint.Rule (ModuleLint, violations)

type LineWidths = { code :: Int, signature :: Int }

maxLineLength :: LineWidths -> ModuleLint
maxLineLength widths =
  { name: "max-line-length"
  , description:
      "Flags a line over the configured width - a type signature gets its own, wider allowance than ordinary code."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context cstModule -> violations (overLongLines widths cstModule)
  }

overLongLines :: LineWidths -> Module Void -> Array String
overLongLines widths cstModule = Array.mapMaybe
  (tooWide widths (signatureLines cstModule))
  (Map.toUnfoldable (widthByLine cstModule))

tooWide :: LineWidths -> Set Int -> Int /\ Int -> Maybe String
tooWide widths sigLines (line /\ width) =
  let
    isSignature = Set.member line sigLines
    allowed = if isSignature then widths.signature else widths.code
    kind = if isSignature then " for a type signature" else " for code"
    message = fold
      [ "line "
      , show (line + 1)
      , " is "
      , show width
      , " columns, over the max of "
      , show allowed
      , kind
      ]
  in
    if width <= allowed then Nothing else Just message

widthByLine :: Module Void -> Map Int Int
widthByLine cstModule =
  let
    widest acc token = Map.insertWith max token.range.end.line token.range.end.column acc
    tokens = TokenList.toArray (tokensOf cstModule)
  in
    foldl widest Map.empty (Array.filter (not <<< isStringToken) tokens)

isStringToken :: SourceToken -> Boolean
isStringToken token = case token.value of
  TokString _ _ -> true
  TokRawString _ -> true
  _ -> false

signatureLines :: Module Void -> Set Int
signatureLines (Module { body: ModuleBody { decls } }) =
  let
    spannedLines decl = case decl of
      DeclSignature _ -> Array.range (rangeOf decl).start.line (rangeOf decl).end.line
      DeclType _ _ _ -> Array.range (rangeOf decl).start.line (rangeOf decl).end.line
      _ -> []
  in
    Set.fromFoldable (Array.concatMap spannedLines decls)
