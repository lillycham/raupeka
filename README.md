# raupeka

Raupeka is a simple functional language based on the lambda calculus.
It is somewhat like ML and Haskell.

## Syntax

An example:

```ml
let main = print "Hello World!"

let yComb = (\f -> (\x -> f (x x)) (\x -> f (x x)))

let factorial n = if n == 0 then 1 else n * factorial (n - 1)
```
