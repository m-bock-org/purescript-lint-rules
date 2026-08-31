-- | Regenerates the rules table in the README from the rules themselves,
-- | so the two cannot disagree.
module Test.Docs (main) where

import Prelude

import Data.Array (concat, concatMap, difference, filter, fold, intercalate, null) as Array
import Data.Foldable (for_)
import Data.String (Pattern(..), Replacement(..), replace, split) as Str
import Data.Traversable (for)
import Data.String.Common (joinWith) as Str
import Effect (Effect)
import Lint.Rules.Naming.NoStutteringName (noStutteringName)
import Lint.Rules.Nesting.LambdaNestingDepth (maxLambdaNestingDepth)
import Lint.Rules.Nesting.MaxDelimiterRun (maxDelimiterRun)
import Lint.Rules.Size.MaxDeclarationLines (maxDeclarationLines)
import Lint.Rules.Size.MaxLineLength (maxLineLength)
import Lint.Rules.Width.MaxFunctionArity (maxFunctionArity)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)
import Node.Process (exit')

type Described r =
  { name :: String
  , description :: String
  , goodExamples :: Array String
  , badExamples :: Array String
  | r
  }

-- | One rule: its name, what it flags, and an example of each side.
row :: forall r. Described r -> String
row r = Str.joinWith "\n"
  ( [ "#### `" <> r.name <> "`", "", r.description ] <> examples r )

examples :: forall r. Described r -> Array String
examples r =
  let
    blocks = Array.filter (not <<< Array.null)
      [ labelled "-- good" r.goodExamples, labelled "-- bad" r.badExamples ]
  in
    if Array.null blocks then []
    else [ "", "```purescript" ]
      <> Array.intercalate [ "" ] blocks
      <> [ "```" ]

labelled :: String -> Array String -> Array String
labelled label xs
  | Array.null xs = []
  | otherwise = [ label ] <> xs

-- | Mirrors the module layout: one section per `Lint.Rules.<Group>`.
-- | `modules` is what each entry claims to document, checked against
-- | what is actually on disk before anything is written.
groups :: Array { group :: String, modules :: Array String, rules :: Array String }
groups =
  [ { group: "Naming"
    , modules: [ "NoStutteringName" ]
    , rules: [ row noStutteringName ]
    }
  , { group: "Nesting"
    , modules: [ "MaxDelimiterRun", "LambdaNestingDepth" ]
    , rules: [ row (maxDelimiterRun 2), row (maxLambdaNestingDepth 1) ]
    }
  , { group: "Size"
    , modules: [ "MaxDeclarationLines", "MaxLineLength" ]
    , rules:
        [ row (maxDeclarationLines 40)
        , row (maxLineLength { code: 100, signature: 150 })
        ]
    }
  , { group: "Width"
    , modules: [ "MaxFunctionArity" ]
    , rules: [ row (maxFunctionArity 4) ]
    }
  ]

section :: forall r. { group :: String, rules :: Array String | r } -> String
section { group, rules } = Str.joinWith "\n\n" ([ "### " <> group ] <> rules)

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

-- | Every rule module on disk, as "Group/Name".
onDisk :: Effect (Array String)
onDisk = do
  dirs <- readdir "src/Lint/Rules"
  map Array.concat $ for dirs \dir -> do
    files <- readdir ("src/Lint/Rules/" <> dir)
    pure (map (\f -> dir <> "/" <> dropPurs f) files)

dropPurs :: String -> String
dropPurs f = Str.replace (Str.Pattern ".purs") (Str.Replacement "") f

-- | What this file claims to document.
claimed :: Array String
claimed = Array.concatMap
  (\g -> map (\m -> g.group <> "/" <> m) g.modules)
  groups

main :: Effect Unit
main = do
  actual <- onDisk
  let
    missing = Array.difference actual claimed
    stale = Array.difference claimed actual
  if Array.null missing && Array.null stale then do
    readme <- readTextFile UTF8 "README.md"
    writeTextFile UTF8 "README.md" (splice readme)
  else do
    log "Test.Docs is out of step with the rule modules on disk."
    for_ missing \m -> log ("  on disk but not documented: " <> m)
    for_ stale \m -> log ("  documented but not on disk: " <> m)
    exit' 1
