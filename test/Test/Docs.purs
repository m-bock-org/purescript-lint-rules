-- | Regenerates the rules table in the README from the rules themselves,
-- | so the two cannot disagree.
module Test.Docs (main) where

import Prelude

import Data.Array (fold) as Array
import Data.String (Pattern(..), split) as Str
import Data.String.Common (joinWith) as Str
import Effect (Effect)
import Lint.Rules.Naming.NoStutteringName (noStutteringName)
import Lint.Rules.Nesting.LambdaNestingDepth (maxLambdaNestingDepth)
import Lint.Rules.Nesting.MaxDelimiterRun (maxDelimiterRun)
import Lint.Rules.Size.MaxDeclarationLines (maxDeclarationLines)
import Lint.Rules.Size.MaxLineLength (maxLineLength)
import Lint.Rules.Width.MaxFunctionArity (maxFunctionArity)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

type Described r = { name :: String, description :: String | r }

row :: forall r. Described r -> String
row r = Array.fold [ "| `", r.name, "` | ", r.description, " |" ]

-- | Mirrors the module layout: one section per `Lint.Rules.<Group>`.
groups :: Array { group :: String, rules :: Array String }
groups =
  [ { group: "Naming", rules: [ row noStutteringName ] }
  , { group: "Nesting"
    , rules: [ row (maxDelimiterRun 2), row (maxLambdaNestingDepth 1) ]
    }
  , { group: "Size"
    , rules:
        [ row (maxDeclarationLines 40)
        , row (maxLineLength { code: 100, signature: 150 })
        ]
    }
  , { group: "Width", rules: [ row (maxFunctionArity 4) ] }
  ]

section :: { group :: String, rules :: Array String } -> String
section { group, rules } = Str.joinWith "\n"
  ([ "### " <> group, "", "| | |", "|---|---|" ] <> rules)

table :: String
table = Str.joinWith "\n\n" (map section groups)

startMark :: String
startMark = "<!-- RULES -->"

endMark :: String
endMark = "<!-- /RULES -->"

splice :: String -> String
splice readme = case Str.split (Str.Pattern startMark) readme of
  [ before, rest ] -> case Str.split (Str.Pattern endMark) rest of
    [ _, after ] ->
      Array.fold [ before, startMark, "\n\n", table, "\n\n", endMark, after ]
    _ -> readme
  _ -> readme

main :: Effect Unit
main = do
  readme <- readTextFile UTF8 "README.md"
  writeTextFile UTF8 "README.md" (splice readme)
