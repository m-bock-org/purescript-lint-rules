# purescript-lint-rules

A set of general-purpose lint rules for the
[`purescript-lint`](https://github.com/m-bock/purescript-lint) engine.

## The rules

<!-- RULES -->

### Naming

| | |
|---|---|
| `no-stuttering-name` | Flags a qualified name whose own name repeats its qualifier. |

### Nesting

| | |
|---|---|
| `max-delimiter-run` | Flags more than the configured number of brackets opening or closing in immediate succession. |
| `max-lambda-nesting-depth` | Flags a lambda nested anywhere inside more than the configured number of enclosing lambdas. |

### Size

| | |
|---|---|
| `max-declaration-lines` | Flags a top-level declaration whose own source line span exceeds the configured maximum. |
| `max-line-length` | Flags a line over the configured width - a type signature gets its own, wider allowance than ordinary code. |

### Width

| | |
|---|---|
| `max-function-arity` | Flags a top-level function definition with more than the configured number of arguments. |

<!-- /RULES -->

Each lives in its own module, `Lint.Rules.<Group>.<Name>`.

## Licence

MIT.
