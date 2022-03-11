---
slug: 2
title: | 
  2. Reactive Core
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

We are looking for a way of expressing the Hydra Head protocol logic in a Hydra node.

The Hydra Head protocol is defined as a _State machine_ in the paper, whose transitions are inputs that come from different sources which can emit outputs to other instances of the state machine or the mainchain. See the [FC2021](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/) paper for details

It should also be easy to review / feed-back to researchers.

We are familiar with React's [redux](https://react-redux.js.org/) way of structuring applications, which in turn is inspired by [The Elm Architecture](https://guide.elm-lang.org/architecture/) which itself is a simplification of [Functional Reactive Programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) principles.

We have experienced benefits with _Event Sourcing_ in the domain of persistence in the past

## Decision

Implements the Hydra Head core logic as a _loop_ that:
1. Consumes _input events_ from an event _queue_,
2. Applies each _event_ to the current _state_ yielding potentially an _updated state_ and a sequence of _effects_,
3. Execute all _effects_.

## Consequences

The internal state is only ever changed through _Events_.

The core state machine _transition_ function _is pure_ and reviewing it requires minimal Haskell knowledge.

Side-effects are all handled at the level of the `Node`.
