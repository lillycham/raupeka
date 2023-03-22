# raupeka

Raupeka is a simple functional language based on the lambda calculus.
It is somewhat like ML and Haskell.

## Syntax

An example:

```ml
-- Functions can be imported from haskell using the foreign import keyword.
foreign import print : String -> ()
-- Modules can be imported using the import keyword.

-- Functions and variables are defined with the let keyword
let greeting = "Hello World!"

-- Syntax is indentation based, but can also be terminated with a semicolon.
let factorial n = if n == 0 then 1 
         else n * factorial (n - 1)

-- A function can have multiple arguments.
let adder x y = x + y

-- Functions w/ multiple arguments can be partially applied.
let add2 = adder 2

-- This is because all functions are reduced to a series of single argument functions.
-- This is called currying.
-- Internally, the adder function is equivalent to:
-- λx -> λy -> x + y
-- and add2 is equivalent to:
-- λy -> 2 + y
-- Lambda expressions can be created w/ the backslash character, or the Greek letter lamda (λ)

-- The main function is called when the program is run.
let main = print greeting
```

## Types

Raupeka has a simple type system, especially compared to Haskell.
It has the standard inbuilts you would expect, such as `Int`, `Bool`,
`String`, etc.

There are no typeclasses currently. They will be added in the future.

## Functions

Raupeka has a simple function syntax, similar to ML.

```ml
let add x y = x + y
```

##  Compiler

The compiler is of course written in Haskell.
It is a simple compiler, which compiles expressions to a simple
intermediate language based on SKI combinators. SKI Combinators
then either evaluated as is (for the REPL/run commands), or emitted as Haskell code.

The internals of the compiler are partially derived from Matthew Naylor's
"Evaluating Haskell in Haskell".
