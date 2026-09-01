<p>
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
    <img alt="purescript-lint-rules" src="assets/logo-light.png" width="480">
  </picture>
</p>

[![CI](https://github.com/m-bock/purescript-lint-rules/actions/workflows/ci.yml/badge.svg)](https://github.com/m-bock/purescript-lint-rules/actions/workflows/ci.yml)

A set of general-purpose lint rules for the
[`purescript-lint`](https://github.com/m-bock/purescript-lint) engine.

Each rule is a value; its setting arrives where it joins a rule set.

```purescript
import Lint.Rules.Naming.NoStutteringName (noStutteringName)
import Lint.Rules.Size.MaxFunctionArity (maxFunctionArity)
import Lint.RuleSet.Do (rule)
import Lint.RuleSet.Do as Rules

rules :: Array Rule
rules = Rules.do
  rule $ perDecl maxFunctionArity 4
  rule $ perModule_ noStutteringName
```

<!-- RULES -->

## Naming

### ● `no-stuttering-name`

Flags a qualified name whose own name repeats its qualifier.

> **Good**
> ```purescript
> Parser.Token
> ```
>
> **Bad**
> ```purescript
> Parser.ParserToken
> ```

## Nesting

### ● `max-delimiter-run`

Flags more than the configured number of brackets opening or closing in immediate succession.

> **Config** `a run of 2`
>
> **Good**
> ```purescript
> f $ g (h (i x))
> ```
>
> **Bad**
> ```purescript
> f (g (h (i x)))
> ```

### ● `max-lambda-nesting-depth`

Flags a lambda nested anywhere inside more than the configured number of enclosing lambdas.

> **Config** `a depth of 2`
>
> **Good**
> ```purescript
> \xs -> map (\x -> f x) xs
> ```
>
> **Bad**
> ```purescript
> \xs -> map (\x -> filter (\y -> p y) x) xs
> ```

## Height

### ● `max-declaration-lines`

Flags a top-level declaration whose own source line span exceeds the configured maximum.

> **Config** `5 lines`
>
> **Good**
> ```purescript
> report r = header r <> body r
> ```
>
> **Bad**
> ```purescript
> report r =
  let
    h = header r
    b = body r
  in
    h <> b
> ```

## Width

### ● `max-line-length`

Flags a line over the configured width - a type signature gets its own, wider allowance than ordinary code.

> **Config** `{ code: 100, signature: 150 }`
>
> **Good**
> ```purescript
> describe x =
  "value: " <> show x
    <> " (" <> show (length x) <> " items, "
    <> show (total x) <> " in all)"
> ```
>
> **Bad**
> ```purescript
> describe x = "value: " <> show x <> " (" <> show (length x) <> " items, " <> show (total x) <> " in all)"
> ```

## Size

### ● `max-function-arity`

Flags a top-level definition binding more parameters than the configured maximum. Counts the binders written in the equation, which need not match the arrows in its type.

> **Config** `max arity 3`
>
> **Good**
> ```purescript
> resize { width, height, quality } img = img
> ```
>
> **Bad**
> ```purescript
> resize width height quality img = img
> ```

<!-- /RULES -->

## Licence

MIT.
