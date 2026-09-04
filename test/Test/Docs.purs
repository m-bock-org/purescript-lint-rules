-- | Regenerates the rules table in the README from the rules themselves,
-- | so the two cannot disagree.
module Test.Docs (main) where

import Prelude

import Data.Array (concat, concatMap, difference, filter, fold, intercalate, null) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import Lint.Rule (Examples)
import Data.String (Pattern(..), Replacement(..), replace, split) as Str
import Data.Traversable (for)
import Data.String.Common (joinWith) as Str
import Effect (Effect)
import Lint.Rules.Naming.NoStutteringName (noStutteringName)
import Lint.Rules.Nesting.LambdaNestingDepth (maxLambdaNestingDepth)
import Lint.Rules.Nesting.MaxDelimiterRun (maxDelimiterRun)
import Lint.Rules.Height.MaxDeclarationLines (maxDeclarationLines)
import Lint.Rules.Width.MaxLineLength (maxLineLength)
import Lint.Rules.Size.MaxFunctionArity (maxFunctionArity)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)
import Node.Process (exit') as Process

type Described cfg r =
  { name :: String
  , description :: String
  , examples :: Maybe (Examples cfg)
  | r
  }

-- | One rule: its name, what it flags, and an example of each side.
-- | Private. Used only by `groups`. Uses `examples`.
row :: ∀ cfg r. Described cfg r -> String
row r = Str.joinWith "\n"
  ( [ "### ● `" <> r.name <> "`", "", r.description ] <> examples r )

-- | Private, depth 2. Used only by `row`. Uses `quoted`, `labelled`.
examples :: ∀ cfg r. Described cfg r -> Array String
examples r = case r.examples of
  Nothing -> []
  Just shown -> quoted
    ( Array.filter (not <<< Array.null)
        [ Maybe.maybe [] (\c -> [ "**Config** `" <> c <> "`" ])
            (shown.printConfig shown.config)
        , labelled "Good" shown.good
        , labelled "Bad" shown.bad
        ]
    )

-- | Everything about one rule's examples in a single blockquote, so a
-- | reader sees one grouped block with a rule down its left edge rather
-- | than three loose sections.
-- | Private, depth 3. Used only by `examples`. Uses `indent`.
quoted :: Array (Array String) -> Array String
quoted parts
  | Array.null parts = []
  | otherwise = [ "" ] <> Array.concatMap indent (Array.intercalate [ "" ] parts)

-- |
-- | An example is one array element that may itself span several lines,
-- | so it has to be split before prefixing: a continuation line without
-- | its own `>` leaves the blockquote, and takes the closing fence with
-- | it.
-- | Private, depth 4. Used only by `quoted`. Uses `prefix`.
indent :: String -> Array String
indent block = map prefix (Str.split (Str.Pattern "\n") block)

-- | Private, depth 5. Used only by `indent`.
prefix :: String -> String
prefix line = if line == "" then ">" else "> " <> line

-- | One block, or nothing where there is nothing to show.
-- | Private, depth 4. Used only by `labelled`.
fenced :: Array String -> Array String
fenced lines
  | Array.null lines = []
  | otherwise = [ "```purescript" ] <> lines <> [ "```" ]

-- | A side of the rule, under its own label, or nothing where there is
-- | nothing to show. A bold label rather than a heading: these sit
-- | inside a blockquote, and a heading there would add an anchor per
-- | rule per side for no one to link to.
-- | Private, depth 3. Used only by `examples`. Uses `fenced`.
labelled :: String -> Array String -> Array String
labelled label xs
  | Array.null xs = []
  | otherwise = [ "**" <> label <> "**" ] <> fenced xs

-- | Mirrors the module layout: one section per `Lint.Rules.<Group>`.
-- | what is actually on disk before anything is written.
-- | Private. Uses `row`.
groups :: Array { group :: String, modules :: Array String, rules :: Array String }
groups =
  [ { group: "Naming"
    , modules: [ "NoStutteringName" ]
    , rules: [ row noStutteringName ]
    }
  , { group: "Nesting"
    , modules: [ "MaxDelimiterRun", "LambdaNestingDepth" ]
    , rules: [ row maxDelimiterRun, row maxLambdaNestingDepth ]
    }
  , { group: "Height"
    , modules: [ "MaxDeclarationLines" ]
    , rules: [ row maxDeclarationLines ]
    }
  , { group: "Width"
    , modules: [ "MaxLineLength" ]
    , rules: [ row maxLineLength ]
    }
  , { group: "Size"
    , modules: [ "MaxFunctionArity" ]
    , rules: [ row maxFunctionArity ]
    }
  ]

-- | Private. Used only by `table`.
section :: ∀ r. { group :: String, rules :: Array String | r } -> String
section { group, rules } = Str.joinWith "\n\n" ([ "## " <> group ] <> rules)

-- | Private. Uses `section`.
table :: String
table = Str.joinWith "\n\n" (map section groups)

-- | Private.
startMark :: String
startMark = "<!-- RULES -->"

-- | Private.
endMark :: String
endMark = "<!-- /RULES -->"

-- | Private. Used only by `main`.
splice :: String -> String
splice readme = case Str.split (Str.Pattern startMark) readme of
  [ before, rest ] -> case Str.split (Str.Pattern endMark) rest of
    [ _, after ] ->
      Array.fold [ before, startMark, "\n\n", table, "\n\n", endMark, after ]
    _ -> readme
  _ -> readme

-- | Every rule module on disk, as "Group/Name".
-- | Private. Uses `dropPurs`.
onDisk :: Effect (Array String)
onDisk = do
  dirs <- readdir "src/Lint/Rules"
  map Array.concat $ for dirs \dir -> do
    files <- readdir ("src/Lint/Rules/" <> dir)
    pure (map (\f -> dir <> "/" <> dropPurs f) files)

-- | Private. Used only by `onDisk`.
dropPurs :: String -> String
dropPurs f = Str.replace (Str.Pattern ".purs") (Str.Replacement "") f

-- | What this file claims to document.
-- | Private.
claimed :: Array String
claimed = Array.concatMap
  (\g -> map (\m -> g.group <> "/" <> m) g.modules)
  groups

-- | Uses `splice`.
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
    Process.exit' 1
