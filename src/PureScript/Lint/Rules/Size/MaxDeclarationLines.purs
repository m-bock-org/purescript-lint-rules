module PureScript.Lint.Rules.Size.MaxDeclarationLines (maxDeclarationLines) where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CST.Range (rangeOf)
import PureScript.Lint.Rule
  ( DeclarationLint
  , LintResult
  , violation
  , violations
  )

maxDeclarationLines :: Int -> DeclarationLint
maxDeclarationLines maxLines =
  { name: "max-declaration-lines"
  , description:
      "Flags a top-level declaration whose own source line span exceeds the configured maximum."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context decl ->
      let
        range = rangeOf decl
        lineSpan = range.end.line - range.start.line + 1
      in
        if lineSpan > maxLines then
          violation
            ("declaration spans " <> show lineSpan <> " lines, over the max of " <> show maxLines)
        else
          violations []
  }
