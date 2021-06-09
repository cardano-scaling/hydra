# 5. Use IOSim for all IOs

Date: 2021-06-08

## Status

Accepted

## Context

* Although we try to contain the use of IO at the outskirt of the Hydra node using [Handle pattern](0004-use-handle-to-model-effects.md) and [Reactive core](0002-reactive-core.md), low-level effects are still needed in various places, notably to define concurrently executing actions, and thus need to be tested
* Testing asynchronous and concurrent code is notoriously painful
* The ouroboros consensus test suite and [hydra-sim](https://github.com/input-output-hk/hydra-sim) simulation have demonstrated the effectiveness of abstracting concurrent primitives through the use of proper typeclasses and and being able to run these as pure code, harvesting and analysing produced execution traces.

## Decision

* For all IO effects covered by the library, use functions from typeclasses exposed by [io-sim-classes](https://github.com/input-output-hk/ouroboros-network/tree/master/io-sim-classes). As of this writing, this covers:
  * All STM operations through `MonadSTM`
  * Time and timers through `MonadTime` and `MonadTimer`
  * Concurrency through `MonadAsync`, `MonadFork`
  * Basic `println` through `MonadSay`
  * Exceptions through `MonadThrow`

## Consequences

* As some of these functions and typeclasses clash with the [cardano-prelude](https://github.com/input-output-hk/cardano-prelude) we might want to [define a custom prelude](0006-define-custom-prelude)

* Instantiation to concrete IO is pushed at the outermost layer, eg. in the `Main` or tests.
