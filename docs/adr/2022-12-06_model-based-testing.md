---
slug: 21
title: |
  21. Test High-level Properties using Model-Based Testing
authors: []
tags: [Proposed]
---

## Status

Proposed

## Context

- We have been experimenting with [quickcheck-dynamic](https://hackage.org/packages/quickcheck-dynamic) for a while, leading to the implementation of basic [Model-Based tests](https://github.com/input-output-hk/hydra/blob/master/hydra-node/test/Hydra/ModelSpec.hs) for the Hydra Head Protocol
- These tests fill a gap in our testing strategy, between [BehaviorSpec](https://github.com/input-output-hk/hydra/blob/master/hydra-node/test/Hydra/BehaviorSpec.hs) tests which test a "network" of nodes but only at the level of the off-chain Head logic, and [EndToEndSpec](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/test/Test/EndToEndSpec.hs) tests which test a full blown network of nodes interconnected through real network connections and to a real cardano-node:
  - The former are fast but do not test the complete lifecycle of a Head. Furthermore, they are only unit tests so do not provide coverage into various corner cases that could arise in practice
  - The latter exercise the full lifecycle but are very slow and brittle
- Because they run in [io-sim](https://github.com/input-output-hk/io-sim), those Model-based tests are fast and robust as they don't depend on system interactions. Moreover, decoupling the _System-under-Test_ from `IO` makes it easy to simulate an environment that deviates from the "happy path" such as delays from the network, filesystem errors, or even adversarial behaviour from the node, or the chain.

## Decision

- We will maintain and evolve the [Model](https://github.com/input-output-hk/hydra/blob/master/hydra-node/test/Hydra/Model.hs) over time to cover more features
- Key properties of the whole system should be written-down as proper `DynamicLogic` properties and thoroughly tested using quickcheck-dynamic. This includes but is not limited to:
  - Liveness of the Head
  - Consistency of the Head
  - Soundness of Chain
  - Completeness of Chain

## Consequences

- We need to ensure the Model covers the full lifecycle of a Hydra Head network which at the time of writing this ADR is not the case
- There cannot be _One Model to Rule Them All_ so we should refrain from defining different `StateModel` or different `RunModel` depending on what needs to be tested
- In particular, testing against adversarial conditions will certainly require defining different instances of the `Network` or `Chain` components, for example:
  - An _Active Adversary_ that fully the controls the protocol and the parties,
  - A _Network Adversary_ that can delay and or drop messages,
  - A _Faulty Filesystem_ that can causes exceptions when reading or writing files,
  - ...
