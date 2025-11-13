---
slug: 33
title: |
  33. No input queue
authors: [ch1bo]
tags: [Proposed]
---

## Status

Proposed

## Context

- [ADR 32](/adr/32) expanded the requirements for network layer components and decided to use `etcd` as part of its implementation.
- Reliable broadcast is realized by writing messages to broadcast to the `etcd` key-value store and using revisions to acknowledge delivery.
- More specifically, a `last-known-revision` is kept by the `Hydra.Network.Etcd` module and `deliver` is invoked for all messages which are available on the `etcd` key-value store - thus necessarily deliverable to all other nodes - and the `last-known-revision` is set to not re-deliver.
- Delivering a message into the Hydra Head application means that a `NetworkInput` is constructed and stored in the `InputQueue`.
- While the usage of `etcd` provides us crash-tolerant messaging, delivering into that `InputQueue` is prone to losing messages as inputs may not have been processed (into a persisted state change) should the process crash after `deliver`.

> [!WARNING]
>
> TODO: cite "Introduction to Reliable and Secure Distributed Programming" mention of how delivery into applications should be made fault-resistant

## Decision

- Change `deliver` semantics to be synchronously invoking the logic layer with signature

```haskell
deliver :: msg -> m DeliverResult

data DeliverResult = Delivered | NotDelivered
```

  - Only write `last-known-revision` if `deliver` returns `Delivered`

- Instead of a single thread doing `forever . stepHydraNode`, we have many threads calling a new function

```haskell
processInput :: HydraNode tx -> Input tx -> m ()
```

- Drop the `InputQueue` and have inputs directly drive the logic

## Consequences

- "Tying the knot" is a bit trickier as the `InputQueue` does not decouple the setup.
  - An approach using an `MVar` holding a "promise" of a fully connected node can be used.

- Logic layer was running `stepHydraNode` in dedicated thread; this makes it now multi-threaded
  - Side effects processing of the logic layer must become thread-safe

- No `reenqueue` possible anymore
  - `Wait` outcomes need to result in `NotDelivered`
  
> [!CAUTION]
>
> FIXME: while not marking the revision as acknowledged through `NotDelivered` is easily possible, the re-trial of delivery would need to be implemented by the Network component and this could be a smell of the approach..

- Potential downsides of synchronous invocation of logic layer from components:
    - Reentrancy: If `processInput` for a network message causes a `broadcast`, this must not result into synchronously call to `deliver`
    - Blocking: If `processInput` is slow (expensive computation, waiting on I/O), the network `deliver` callback blocks. Each component would need to ensure reactivity in their domain.
    - Error handling: If `processInput` throws an exception during a network `deliver`, it surfaces through the network component and not through the main loop.

## Alternatives

- Provide a callback to `deliver` and consequently to all inputs processed by the `InputQueue`
