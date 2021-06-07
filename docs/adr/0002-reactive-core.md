# 2. Reactive core

Date: 2021-06-07

## Status

Accepted

## Context

* The Hydra Head protocol is defined as a _State machine_ whose transitions are inputs that come from different sources which can emit outputs to other instances of the state machine or the mainchain. See the [FC2021](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/) paper for details

## Decision

Implements the Hydra Head core logic as a _loop_ that:
1. Consumes _input events_ sequentially from an internal _queue_,
2. Applies each _event_ to the current _state_ yielding potentially an _updated state_ and a sequence of _effects_,
3. Execute each _effect_ sequentially.

## Consequences

* Internal state is only ever changed through _Events_
* The core state machine _transition_ function _must be pure_
* Side-effects are all handled at the level of the `Node`

## Additional Comments

* This design is basically identical to React's [redux](https://react-redux.js.org/) behaviour which in turn is inspired by [The Elm Architecture](https://guide.elm-lang.org/architecture/) which itself is a simplification of [Functional Reactive Programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) principles.
* This is also similar to _Event Sourcing_ but for the persistence of events part
