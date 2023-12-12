---
slug: 29
title: |
  29. EventSource & EventSink abstractions
authors: [@cardenaso11, @quantumplation, @ch1bo]
tags: []
---

## Status
Draft

## Context

* The Hydra node represents a significant engineering asset, providing layer 1 monitoring, peer to peer consensus, durable persistence, and an isomorphic Cardano ledger. Because of this, it is being eyed as a key building block not just in Hydra based applications, but other protocols as well.

* Currently the `hydra-node` uses a very basic persistence mechanism for it's internal `HeadState`, that is saving `StateChanged` events to file on disk and reading them back to load and re-aggregate the `HeadState` upon startup.
  - Some production setups would benefit from storing these events to a service like Amazon Kinesis data stream instead of local files.

* The `hydra-node` websocket-based API is the only available event stream right now and might not fit all purposes.
  - See also ADR [3](/adr/3) and [25](/adr/25)
  - Internally, this is realized as a single `Server` handle which can `sendOutput :: ServerOutput tx -> m ()`
  - These `ServerOutput`s closely relate to `StateChanged` events and `ClientEffect`s are yielded by the logic layer often together with the `StateChanged`. For example:
  ```hs
  onInitialChainAbortTx newChainState committed headId =
    StateChanged HeadAborted{chainState = newChainState}
      <> Effects [ClientEffect $ ServerOutput.HeadIsAborted{headId, utxo = fold committed}]
  ```

* Users of `hydra-node` are interested to add alternative implementations for storing, loading and consuming events of the Hydra protocol.

# Decision

* We create two new interfaces in the `hydra-node` architecture:

  - ```data EventSource e m = EventSource { getEvents :: m [e] }```
  - ```data EventSink e m = EventSink { putEvent :: e -> m () }```

* We realize our current `PersistenceIncremental` used for persisting `StateChanged` events is both an `EventSource` and an `EventSink`

* We drop the `persistence` from the main handle `HydraNode tx m`, add **one** `EventSource` and allow **many** `EventSinks`

```hs
data HydraNode tx m = HydraNode
  { -- ...
  , eventSource :: EventSource (StateChanged tx) m
  , eventSinks :: [EventSink (StateChanged tx) m]
  }
```

* The `stepHydraNode` main loop does call `putEvent` on all `eventSinks` in sequence.
 
  - TBD: transaction semantics? what happens if one fails?

* The default `hydra-node` main loop does use the file-based `EventSource` and a single file-based `EventSink` (using the same file).

* TBD: As the node starts up, when loading events from the `EventSource`, it will also ensure "at least once" semantics for each `EventSink`.

* We realize that the `EventSource` and `EventSink` handles, as well as their aggregation in `HydraNode` are used as an API by forks of the `hydra-node` and try to minimize changes to it.

## Consequences

* The default operation of the `hyda-node` remains unchanged.
* There are other things called `Event` and `EventQueue(putEvent)` right now in the `hydra-node`. This is getting confusing and when we implement this, we should also rename several things first (tidying).
* The API `Server` can be modelled and refactored as an `EventSink`.
  - TBD: Do `Network` and `Chain` parts qualify as `EventSink`s as well or shall those be triggered by `Effect`s still?
* Projects forking the hydra node have a natively extensions points for producing and consuming events.
* TBD: These extensions can preserve robust "at least once" semantics for each hydra event.
* Sundae Labs can build a "Save transaction batches to S3" proof of concept `EventSink`.
* Sundae Labs can build a "Scrolls source" `EventSink`.
* Sundae Labs can build a "Amazon Kinesis" `EventSource` and `EventSink`.
* Extension points like `EventSource` and `EventSink` could be dynamically loaded as plugins without having to fork `hydra-node` (maybe in a future ADR).
