# Compiler Design

Raupeka's compiler is quite simple.
It's a multipass compiler, operating in several, decoupled phases.

## Frontend
The first pass is the parsing pass, in which we read in the source file.

The second pass involves "desugaring" the consumed input into a simplified "core" langauge, 
eliminating any extraneous syntax forms such as if-then-else, let-in, etc.

Once this is done, the AST is sent to one of several backend modules.

## Backend
The next phase is the compilation phase, which is a modular system with several output formats.

The "standard" compilation strategy is to produce an SKI-combinator calculus expression.
This is interpreted with a simple tree walker.

From the SKI expression we can also produce a Haskell program.

The third backend is the JS backend, an alternative experimental backend. This backend emits
JavaScript code.

## Optimisation

Raupeka is an optimising compiler. It performs some simple optimisations over the SKI expressions
produced.


## Behaviour

It is not guaranteed that Raupeka will behave identically for all backends; in fact, it is generally
safe to assume that the JS backend will have different runtime behaviour. The JS backend *DOES NOT* 
implement any lazy evaluation. As a result, the graph-reducing interpreter and the haskell backend
will almost certainly have different semantics.
