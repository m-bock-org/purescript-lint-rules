module Lint.Rules.Height.MaxDeclarationLines (maxDeclarationLines) where

import Prelude

import PureScript.CST.Range (rangeOf)
import Lint.Rule (DeclarationLint, violations)

maxDeclarationLines :: Int -> DeclarationLint
maxDeclarationLines maxLines =
  { name: "max-declaration-lines"
  , description:
      "Flags a top-level declaration whose own source line span exceeds the configured maximum."
  , goodExamples: [ "report r = header r <> body r" ]
  , badExamples:
      [ "report r =\n  let\n    h = header r\n    b = body r\n  in\n    h <> b" ]
  , rule: \_context decl ->
      let
        range = rangeOf decl
        lineSpan = range.end.line - range.start.line + 1
      in
        if lineSpan > maxLines then
          violations
            [ "declaration spans " <> show lineSpan <> " lines, over the max of "
                <> show maxLines
            ]
        else
          violations []
  }
