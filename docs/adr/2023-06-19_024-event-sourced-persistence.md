---
slug: 24
title: |
  24. Persist state changes incrementally
authors: [abailly]
tags: [Accepted]
---

## Status

Accepted

## Context

- The state of a Hydra Head is currently persisted as a whole upon each `NewState` _outcome_ from the `update` function: The new state is serialised and the `state` file is overwritten with the corresponding bytes. While this is a straightforward strategy to implement, it has a huge impact on the performance of a Hydra Head as serialising a large data structure like the `HeadState` and completely overwriting a file is costly
  - We revisited our benchmarks and [found](https://github.com/input-output-hk/hydra/issues/186#issuecomment-1584292265) that persistence was the major bottleneck when measuring roundtrip confirmation time,e g. the time it takes from a client's perspective to submit a transaction and observe in a `ConfirmedSnapshot`
- Furthermore, the way we currently handle changes to the `HeadState` in the hydra-node, while conceptually being an `Effect` is handled differently from other `Effect`s: The state is updated transactionally through a dedicated `modifyHeadState` function in the core loop of processing events, and _then_ effects are processed.

## Decision

Implement state persistence using [_Event Sourcing_](https://thinkbeforecoding.com/post/2013/07/28/Event-Sourcing-vs-Command-Sourcing). Practically, this means:

1. Replace the `NewState` outcome with a `StateChanged` _event_ which can be part of the `Outcome` of `HeadLogic`'s `update` function, representing the _change_ to be applied to the current state.
2. Add an `aggregate` function to manage applying `StateChanged` events on top of the current `HeadState` to keep it updated in-memory.
3. Persist `StateChanged`s in an append-only log using a dedicated [handle](/adr/4).
4. Upon node startup, reread `StateChanged` events log and reapply those to reset the `HeadState`.

The following sequence diagram illustrates new event handling in the `HeadLogic`:

```mermaid
sequenceDiagram
   Node ->> Node : nextEvent : event
   critical modifyHeadState : state -> state';
     activate Node
     Node ->>+ HeadLogic: update(state, event)
     HeadLogic -->>- Node : Outcome (sc: StateChanged, oe: OtherEffect)
     Node ->>+ HeadLogic: aggregate(state, sc)
     HeadLogic -->- Node : state'
   end
   deactivate Node
   Node ->> Persistence: append(sc)
   Node ->> Node: processEffect(oe)
```

## Consequences

- :racehorse: The main expected consequence of this change is an increase of the overall performance of Hydra Head network.

- Need to pattern match twice on the `HeadState`, once in `update` and once in `aggregate`.

- Terms from the specification are distributed over `update` and `aggregate` function. For example, the statements about updating all seen transactions would now be in `aggregate` and not anymore in `update`.

- New possibilities this change introduces with respect to `ServerOutput` handling and client's access to a head's state:

  - Instead of having the `HeadLogic` emits directly a `ClientEffect`, the latter could be the result of a client-centric _interpretation_ of a `StateChanged`.
  - Pushing this a little further, we could maintain a _Query Model_ for clients with a dedicated [Query API](https://github.com/input-output-hk/hydra/discussions/686) to ease implementation of stateless clients.

- Calling `StateChanged` an _event_ while treating it in the code alongside _effects_ might introduce some confusion as we already use the word [Event](https://github.com/input-output-hk/hydra/blob/45913954eb18ef550a31017daa443cee6720a00c/hydra-node/src/Hydra/HeadLogic.hs#L64) to designate the inputs (a.k.a. commands) to the Head logic state machine. We might want at some later point to unify the terminology.
