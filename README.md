# purescript-lint-rules

A set of general-purpose lint rules for the
[`purescript-lint`](https://github.com/m-bock/purescript-lint) engine.

The engine ships with none. This is a starting set to write your own rule
set from: six rules that each fit on a screen and hold an opinion most
people already share.

```purescript
import Lint.Rules.Size.MaxLineLength (maxLineLength)
import Lint.Rules.Width.MaxFunctionArity (maxFunctionArity)
```

## The rules

| Rule | Scope | Config | Flags |
|---|---|---|---|
| `maxLineLength` | module | `{ code, signature }` | A line over the configured width. Signatures get their own, wider budget, because a type that does not fit is usually saying something true. |
| `maxDelimiterRun` | module | `Int` | More brackets opening or closing in immediate succession than allowed. A blunt proxy for "too much going on in one expression". |
| `noStutteringName` | module | – | A qualified name repeating its qualifier: `Parser.ParserToken` rather than `Parser.Token`. |
| `maxDeclarationLines` | declaration | `Int` | A top-level declaration whose own source span is too long. The one blind spot the nesting rules cannot catch: long, flat and shallow. |
| `maxLambdaNestingDepth` | declaration | `Int` | A lambda nested anywhere inside too many enclosing lambdas - not only directly chained ones. |
| `maxFunctionArity` | declaration | `Int` | A top-level function taking more arguments than allowed. |

## Status

Early. The engine's API has had one consumer so far and will move.

## Licence

MIT.
