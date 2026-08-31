# purescript-lint-rules

A set of general-purpose lint rules for the
[`purescript-lint`](https://github.com/m-bock/purescript-lint) engine.

The engine ships with none. Rules are ordinary values, so a rule set is a
program you write - this package is a starting library to write it from,
not a default configuration.

```purescript
import PureScript.Lint.Rules.Size.MaxLineLength (maxLineLength)
import PureScript.Lint.Rules.Nesting.MaxCallStackDepth (maxCallStackDepth)
```

## The rules

| Rule | Scope | Config | Flags |
|---|---|---|---|
| `maxLineLength` | module | `{ code, signature }` | A line over the configured width. Signatures get their own, wider budget, because a type that does not fit is usually saying something true. |
| `maxDelimiterRun` | module | `Int` | More brackets opening or closing in immediate succession than allowed. A blunt proxy for "too much going on in one expression". |
| `maxCallStackDepth` | module | `Int` | A function whose longest chain of calls through your *own* functions exceeds the limit. |
| `noStutteringName` | module | – | A qualified name repeating its qualifier: `Parser.ParserToken` rather than `Parser.Token`. |
| `maxDeclarationLines` | declaration | `Int` | A top-level declaration whose own source span is too long. The one blind spot the nesting rules cannot catch: long, flat and shallow. |
| `maxLambdaNestingDepth` | declaration | `Int` | A lambda nested inside too many other lambdas in one declaration. |
| `maxFunctionArity` | declaration | `Int` | A top-level function taking more arguments than allowed. |
| `unicodeForall` | declaration | – | `forall` where `∀` is wanted. **Auto-fixable.** |
| `sameConstructorArm` | expression | config record | A case arm that matches a one-argument constructor and rebuilds the same constructor around the result - that is a `map`. |

### On `maxCallStackDepth`

Worth singling out, because it is the only rule here that charges you for
naming a thing. Every other rule rewards extraction; this one pushes
back, by counting how many of your own functions a call passes through
before reaching a leaf.

That tension is deliberate. When both exits close - the function is too
long *and* the call chain is too deep - the decomposition is usually
wrong, and neither splitting nor inlining will fix it on its own.

## Also in here

`PureScript.Lint.Graph`, `.Graph.Components` and `.Scope` are shared
analysis machinery rather than rules: a call graph, strongly-connected
components, and scope resolution. They live here rather than in the
engine because only rules need them - the engine parses and dispatches,
it does not analyse.

## Status

Early. The engine's API has had one consumer so far and will move.

## Licence

MIT.
