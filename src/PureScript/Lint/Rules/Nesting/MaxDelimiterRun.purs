module PureScript.Lint.Rules.Nesting.MaxDelimiterRun (maxDelimiterRun) where

import Prelude

import Data.Array (dropEnd, foldl, last, mapMaybe, snoc) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList (toArray) as TokenList
import PureScript.CST.Types (Module, Token(..))
import PureScript.Lint.Rule (ModuleLint, violations)

data Side = Opening | Closing

derive instance eqSide :: Eq Side

type Delimiter = { side :: Side, line :: Int }

type Run = { side :: Side, line :: Int, count :: Int }

maxDelimiterRun :: Int -> ModuleLint
maxDelimiterRun maxRun =
  { name: "max-delimiter-run"
  , description:
      "Flags more than the configured number of brackets opening or closing in immediate succession."
  , goodExample: Just "f $ g $ h $ i x"
  , badExample: Just "f (g (h (i x)))"
  , rule: \_context cstModule -> violations (findings maxRun cstModule)
  }

findings :: Int -> Module Void -> Array String
findings maxRun cstModule = Array.mapMaybe (tooLong maxRun)
  (Array.foldl extendRun [] (delimiters cstModule))

tooLong :: Int -> Run -> Maybe String
tooLong maxRun run =
  if run.count <= maxRun then Nothing
  else Just $ fold
    [ "line "
    , show (run.line + 1)
    , ": "
    , show run.count
    , " brackets "
    , sideLabel run.side
    , " in a row, over the max of "
    , show maxRun
    , " - use $ for the outermost application, or name an inner part with a let"
    ]

delimiters :: Module Void -> Array Delimiter
delimiters cstModule = Array.mapMaybe
  (\token -> map (\side -> { side, line: token.range.start.line }) (sideOf token.value))
  (TokenList.toArray (tokensOf cstModule))

sideLabel :: Side -> String
sideLabel = case _ of
  Opening -> "open"
  Closing -> "close"

extendRun :: Array Run -> Delimiter -> Array Run
extendRun acc next =
  let
    fresh = Array.snoc acc { side: next.side, line: next.line, count: 1 }
    bumped run = Array.snoc (Array.dropEnd 1 acc) (run { count = run.count + 1 })
  in
    Maybe.maybe fresh (\run -> if run.side == next.side then bumped run else fresh)
      (Array.last acc)

sideOf :: Token -> Maybe Side
sideOf = case _ of
  TokLeftParen -> Just Opening
  TokLeftSquare -> Just Opening
  TokRightParen -> Just Closing
  TokRightSquare -> Just Closing
  _ -> Nothing
