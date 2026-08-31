# purescript-lint-rules

A set of general-purpose lint rules for the
[`purescript-lint`](https://github.com/m-bock/purescript-lint) engine.

<!-- RULES -->

## Naming

### ● `no-stuttering-name`

Flags a qualified name whose own name repeats its qualifier.

```purescript
-- good
parse :: Parser.Token -> Int

-- bad
parse :: Parser.ParserToken -> Int
```

## Nesting

### ● `max-delimiter-run`

Flags more than the configured number of brackets opening or closing in immediate succession.

```purescript
-- good
f $ g (h (i x))

-- bad
f (g (h (i x)))
```

### ● `max-lambda-nesting-depth`

Flags a lambda nested anywhere inside more than the configured number of enclosing lambdas.

```purescript
-- good
\xs -> map f xs

-- bad
\xs -> map (\x -> f x) xs
```

## Height

### ● `max-declaration-lines`

Flags a top-level declaration whose own source line span exceeds the configured maximum.

```purescript
-- good
report r = header r <> body r

-- bad
report r =
  let
    h = header r
    b = body r
  in
    h <> b
```

## Width

### ● `max-line-length`

Flags a line over the configured width - a type signature gets its own, wider allowance than ordinary code.

```purescript
-- good
describe x =
  "value: "
    <> show x

-- bad
describe x = "value: " <> show x <> " and a good deal more"
```

## Size

### ● `max-function-arity`

Flags a top-level definition binding more parameters than the configured maximum. Counts the binders written in the equation, which need not match the arrows in its type.

```purescript
-- good
resize { width, height, quality } img = img

-- bad
resize width height quality img = img
```

<!-- /RULES -->

Each lives in its own module, `Lint.Rules.<Group>.<Name>`.

## Licence

MIT.
