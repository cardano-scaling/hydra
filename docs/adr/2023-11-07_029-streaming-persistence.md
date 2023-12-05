---
slug: 29
title: |
  29. EventServer abstraction for event persistence
authors: [@cardenaso11, @quantumplation]
tags: []
---

## Status
Draft

## Context

The Hydra node represents a significant engineering asset, providing layer 1 monitoring, peer to peer consensus, durable persistence, and an isomorphic Cardano ledger. Because of this, it is being eyed as a key building block not just in Hydra based applications, but other protocols as well.

One remaining difficulty in integrating Hydra into a fully productionalized software stack is the persistence model.  Currently, the Hydra node achieves durability by writing a sequence of "StateChanged" events to disk. If a node is interrupted, upon restarting, it can replay just these events, ignoring their corresponding effects, to recover the same internal state it had when it was interrupted. However, downstream consumers don't have the same luxury.

We propose generalizing the persistence mechanism to open the door to a plugin based approach to extending the Hydra node.

# Decision

We propose adding a "persistence combinator", which can combine one or more "persistenceIncremental" instances.

When appending to the combinator, it will forward the append to each persistence mechanism.

As the node starts up, as part of recovering the node state, it will also ensure "at least once" semantics for each persistence mechanism. It will maintain a notion of a "primary" instance, from which events are loaded, and "secondary instances", which are given the opportunity to re-observe each event.

Each persistence mechanism will be responsible for it's own durability; for example, it may maintain its own checkpoint, and only re-broadcast the event if its after the checkpoint.

The exact implementation details of the above are left unspecified, to allow some flexibility and experimentation on the exact mechanism to realize these goals.

## Consequences

Here are the consequences we forsee from this change:
- The default operation of the node remains unchanged
- Projects forking the hydra node have a natively supported mechanism to extend node persistence
- These extensions can preserve robust "at least once" semantics for each hydra event
- Sundae Labs will build a "Save transaction batches to S3" proof of concept extension
- Sundae Labs will build a "Scrolls source" proof of concept integration
- This may also enable a future ADRs for dynamically loaded plugins without having to fork the Hydra node at all