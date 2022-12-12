---
slug: 22
title: |
  22. Label threads and concurrent data structures
authors: []
tags: [Proposed]
---

## Status

Proposed

## Context

- Concurrency in Hydra is expressed using the type-classes provided by [io-sim-classes](https://github.com/input-output-hk/io-sim/tree/main/io-classes).
- This allows us to benefit from speed-up and determinism on gets by running test code in the `IOSim s` monad
- `io-sim` can also provide a wealth of information on the actual behaviour of multithreaded code and potentially help detecting race conditions, deadlocks, livelocks, and other typical issues on encounter when writing concurrent code. This information is typically available through the [SimTrace](https://github.com/input-output-hk/io-sim/blob/main/io-sim/src/Control/Monad/IOSim/Types.hs#L689) result from `runSimTrace` which records all scheduling events
- However, this information can be hard to understand as the threads and variables are labelled using arbitrary numbers

## Decision

- Label all `TVar`, `TQueue`, `Async`... data structures using the appropriate `labelXXX` functions, eg.

  ```
   nodeThread <- async $ labelThisThread ("node-" <> shortLabel hsk) >> runHydraNode (contramap Node tr) node
  ```

## Consequences

- When pretty-printing a `SimTrace` using `(fmap ppEvents) . traceEvents`, one can easily identifies which thread does what and which variables are read/written
