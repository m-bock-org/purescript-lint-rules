module PureScript.Lint.Rules.Idiom.UnicodeForall (unicodeForall) where

import Prelude

import Data.Array (null) as Array
import Data.Maybe (Maybe(..))
import PureScript.CST.Traversal
  ( defaultMonoidalVisitor
  , defaultVisitor
  , foldMapDecl
  , rewriteDeclBottomUp
  )
import PureScript.CST.Types (Declaration, SourceStyle(..), SourceToken, Token(..), Type(..))
import PureScript.Lint.Rule
  ( DeclarationLint
  , fixed
  , LintResult
  , violations
  )

unicodeForall :: DeclarationLint
unicodeForall =
  { name: "unicode-forall"
  , description: "Requires the unicode forall quantifier rather than the ASCII keyword."
  , goodExample: Just "identity :: \x2200 a. a -> a"
  , badExample: Just "identity :: forall a. a -> a"
  , rule: \_context decl ->
      if hasAsciiForall decl then fixed (toUnicode decl) else violations []
  }

hasAsciiForall :: Declaration Void -> Boolean
hasAsciiForall decl = not (Array.null (asciiForalls decl))

asciiForalls :: Declaration Void -> Array Unit
asciiForalls = foldMapDecl
  ( defaultMonoidalVisitor
      { onType = case _ of
          TypeForall token _ _ _ -> if isAscii token then [ unit ] else []
          _ -> []
      }
  )

toUnicode :: Declaration Void -> Declaration Void
toUnicode = rewriteDeclBottomUp
  ( defaultVisitor
      { onType = case _ of
          TypeForall token bindings dot inner ->
            TypeForall (unicodeToken token) bindings dot inner
          other -> other
      }
  )

isAscii :: SourceToken -> Boolean
isAscii token = token.value == TokForall ASCII

unicodeToken :: SourceToken -> SourceToken
unicodeToken token = token { value = TokForall Unicode }
