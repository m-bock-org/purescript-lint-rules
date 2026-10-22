module Lint.Rules.Width.MaxLineLength (maxLineLength, overLongLines) where

import Prelude

import Data.Array (concatMap, filter, mapMaybe, range) as Array
import Data.Foldable (fold, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Lint.Rules.Width.TypeLines (typeLines)
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList (toArray) as TokenList
import PureScript.CST.Types
  ( Module(..)
  , ModuleHeader(..)
  , SourceToken
  , Token(..)
  )
import Lint.Rule (ModuleLint, violations)

type LineWidths = { code :: Int, types :: Int }

-- |
-- | The examples below are split across source lines with `<>` so this
-- | file stays inside the limit its own examples are about. The example
-- | is what the string says, not how the string is written - and a rule
-- | whose source breaks the rule is the one file where that has to be
-- | got right rather than exempted.
-- | Uses `overLongLines`.
maxLineLength :: ModuleLint LineWidths
maxLineLength =
  { name: "max-line-length"
  , description:
      fold
        [ "Flags a line over the configured width - a line that is all type gets its own, wider"
        , " allowance than code: a signature at the top level, in a `let` or `where`, on a class"
        , " member or an instance method; a `type`, `newtype`, `data`, kind or foreign"
        , " declaration; a record or row type's fields; the `:: Type` of an annotation once it"
        , " stands on a line of its own. A line holding any code is code. Import lines are not"
        , " measured: an import is looked up rather than read, and one line each is the house"
        , " style."
        ]
  , examples: Just
      { config: { code: 100, types: 150 }
      , printConfig: Just <<< show
      , good:
          [ "describe x =\n  \"value: \" <> show x\n"
              <> "    <> \" (\" <> show (length x) <> \" items, \"\n"
              <> "    <> show (total x) <> \" in all)\""
          ]
      , bad:
          [ "describe x = \"value: \" <> show x <> \" (\" <> show (length x)"
              <> " <> \" items, \" <> show (total x) <> \" in all)\""
          ]
      }
  , rule: \widths _context cstModule -> violations (overLongLines widths cstModule)
  }

-- | One finding per line wider than its allowance. Uses `tooWide`, `importLines`, `widthByLine`.
overLongLines :: LineWidths -> Module Void -> Array String
overLongLines widths cstModule =
  let
    imported = importLines cstModule
    measured (line /\ _) = not (Set.member line imported)
  in
    Array.mapMaybe
      (tooWide widths (typeLines cstModule))
      (Array.filter measured (Map.toUnfoldable (widthByLine cstModule)))

-- | Private, depth 2. Used only by `overLongLines`.
tooWide :: LineWidths -> Set Int -> Int /\ Int -> Maybe String
tooWide widths allTypeLines (line /\ width) =
  let
    isType = Set.member line allTypeLines
    allowed = if isType then widths.types else widths.code
    kind = if isType then " for a type" else " for code"
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

-- | Private, depth 2. Used only by `overLongLines`. Uses `isStringToken`.
widthByLine :: Module Void -> Map Int Int
widthByLine cstModule =
  let
    widest acc token = Map.insertWith max token.range.end.line token.range.end.column acc
    tokens = TokenList.toArray (tokensOf cstModule)
  in
    foldl widest Map.empty (Array.filter (not <<< isStringToken) tokens)

-- | Private, depth 3. Used only by `widthByLine`.
isStringToken :: SourceToken -> Boolean
isStringToken token = case token.value of
  TokString _ _ -> true
  TokRawString _ -> true
  _ -> false

-- | Private, depth 2. Used only by `overLongLines`.
importLines :: Module Void -> Set Int
importLines (Module { header: ModuleHeader { imports } }) =
  Set.fromFoldable
    (Array.concatMap (\one -> Array.range (rangeOf one).start.line (rangeOf one).end.line) imports)
