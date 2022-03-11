---
slug: 5
title: | 
  5. Use io-classes
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

Although we try to contain the use of IO at the outskirt of the Hydra node using [Handle pattern](/adr/4) and [Reactive core](/adr/2), low-level effects are still needed in various places, notably to define concurrently executing actions, and thus need to be tested

Testing asynchronous and concurrent code is notoriously painful

The ouroboros consensus test suite and [hydra-sim](https://github.com/input-output-hk/hydra-sim) simulation have demonstrated the effectiveness of abstracting concurrent primitives through the use of typeclasses (MTL-style pattern) and being able to run these as pure code, harvesting and analysing produced execution traces.

There are other such libraries, e.g. [concurrency](https://hackage.haskell.org/package/concurrency) and [dejafu](https://hackage.haskell.org/package/dejafu), as well as the venerable [exceptions](https://hackage.haskell.org/package/exceptions) (for abstracting exception throwing).

## Decision

For all IO effects covered by the library, use functions from typeclasses exposed by [io-classes](https://github.com/input-output-hk/ouroboros-network/tree/e338f2cf8e1078fbda9555dd2b169c6737ef6774/io-classes). As of this writing, this covers:
  * All STM operations through `MonadSTM`
  * Time and timers through `MonadTime` and `MonadTimer`
  * Concurrency through `MonadAsync`, `MonadFork`
  * Exceptions through `MonadThrow`, `MonadCatch` and `MonadMask`

## Consequences

We can use `io-sim` to evaluate IO-ish functions easily

Instantiation to concrete IO is pushed at the outermost layer, eg. in the `Main` or tests.

As some of these functions and typeclasses clash with the
[cardano-prelude](https://github.com/input-output-hk/cardano-prelude) we might
want to define a custom prelude (candidate for another ADR)
