# purescript-lint-rules

A set of general-purpose lint rules for the
[`purescript-lint`](https://github.com/m-bock/purescript-lint) engine.

## The rules

| | |
|---|---|
| `noStutteringName` | Flags a qualified name whose own name repeats its qualifier. |
| `maxLambdaNestingDepth` | Flags a lambda nested anywhere inside more than the configured number of enclosing lambdas. |
| `maxDelimiterRun` | Flags more than the configured number of brackets opening or closing in immediate succession. |
| `maxDeclarationLines` | Flags a top-level declaration whose own source line span exceeds the configured maximum. |
| `maxLineLength` | Flags a line over the configured width - a type signature gets its own, wider allowance than ordinary code. |
| `maxFunctionArity` | Flags a top-level function definition with more than the configured number of arguments. |

Each lives in its own module, `Lint.Rules.<Group>.<Name>`.

## Licence

MIT.
