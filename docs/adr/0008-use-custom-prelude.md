# 8. Custom Prelude 

Date: 2021-06-18

## Status

Accepted

## Context

In a Haskell project, we often get to use and re-use the same libraries and functions. Haskell comes with a default `Prelude` package with the `base` library, which provides a good and sensible starting point. However, the base `Prelude` also comes with a few quirks:

- Many commonly used functions or constructors are not exported by default (e.g. `bracket`, `foldM`, `first`, `lift`, `forM`, `when`, `SomeException`, `Set`, `&` ...etc).
- Many functions in the base Prelude are partial, like `head` or `read`. 
- Many functions simply happens in plain `IO`, whereas applications usually try to push IO to the boundary as much as possible (for example, using mtl-style class constraints).
- The interface for I/O operations in the base Prelude is `String`, which comes with quite major performance hit and often forces to convert back and forth to `Text` or `ByteString` equivalents.

All-in-all, while it _does the job_, the base `Prelude` may not necessarily be the most _convenient_ prelude for an active project development. 

## Decision

We'll use a custom prelude to help us get more productive and more importantly, to reduce the daily friction of our interactions with the base prelude. While [`relude`](https://hackage.haskell.org/package/relude) makes for a good candidate, we still chose to re-wrap it in a custom `Hydra.Prelude` module to grant us the ability to add or remove a few things specifics to Hydra and Cardano in general. In particular, we will hide from `relude` all the re-exports of the [`stm`](https://hackage.haskell.org/package/stm) library in favor of [`io-sim-classes`](https://github.com/input-output-hk/ouroboros-network/tree/36d8a4b7792ffcfa0c70eb56065071fbfa59af36/io-sim-classes) which we already use pervasively and which provides (among other things) most of the same capabilities.

## Consequences

- Remove uses of 'cardano-prelude' in favor of a new 'hydra-prelude' module.
- Cleaning up of imports from existing file modules.
- Happier feeling day after day from using a developer-friendly prelude.
- Stop loosing time in often re-importing the same functions over and over. 
- Have an explicit point for discouraging / blessing usage of one or the other function, as well as documenting such decisions
