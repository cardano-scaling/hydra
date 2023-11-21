---
slug: 29
title: |
  29. EventServer abstraction for event persistence
authors: [@cardenaso11]
tags: []
---

## Status
N/A

## Context

The current Hydra API is materialized as a firehose WebSocket connection: upon connecting, a client receives a deluge of information, such as all past transactions. Additionally, there is no way to pick and choose which messages to receive, and not all events are exposed via the API.

This forces some awkward interactions when integrating with a Hydra node externally, such as when using it as a component in other protocols, or trying to operationalize the Hydra server for monitoring and persistence. Here's just a few of the things that prove to be awkward:
 - An application that only cared about certain Hydra resources (like say, the current UTxO set), would be unable to subscribe to just that resource. The application would be made to either parse the Hydra incremental persistence log directly (which may change), or subscribe to the full firehose of Websocket API events.
 - An application that wanted to write these hydra events to a message broker like RabbitMQ or Kinesis would need it's own custom integration layer
 - An application that wishes to build on Hydra would need to fork the Hydra project in order to change how file persistence worked.
 

Additionally, much of the changes that would need to happen to solve any of these would apply equally well to the on-disk persistence the Hydra node currently provides.

-------------------------------------

- Current client API is a stateful WebSocket connection. Client API has its own incrementally persisted state that must be maintained, and includes a monotonically increasing `Seq` ID, but this is based off the events a client observes, not the Hydra host's event log.

- Client must be flexible and ready to handle many different events

- There is no way to simply hand off transactions to the hyrda-node currently, a full connection must be initiated before observed transactions can be applied.

- Using history=0 in the query string allows ignoring prior events, but it's not possible to ignore events going forward, only disabling UTxO on Snapshot events.

- Many applications, like SundaeSwap's Gummiworm Protocol, would benefit from using the API in ways which are currently not supported

- Custom event management is intended to be for a minority of Hydra users, that intend to integrate heavily with Hydra. It's a good fit for passing stuff off to a message queue (MQTT, Kafka, whatever) for further processing, probably from a dedicated process (see "Chain Server" below)
    - The event management is intended to modularize and unify the Websocket API, and (incremental, full) persistence
    - The default event management is intended to be transparent to most users
    - There exist "highly compatible" specifications like STOMP or MQTT, but supporting these directly as a substitute for API or persistence, would lock in a significant amount of implementation details


<!-- The following section can be removed from the ADR, but mostly just recorded background, motivation, prior art -->
- Previous mock chain using ZeroMQ was [removed][zmq] as part of [#119][#119], due to complexity and feeling of ZeroMQ being unmaintained (primary author no longer contributing, new tag release versions not being published)
    - This mock chain was used to mock the layer L1 chain, not the L2 ledger
- Attempt in February 2023 to externalize chainsync server as part of [#230][#230]
    - Similar to [#119][#119], but the "Chain Server" component in charge of translating Hydra Websocket messages into direct chain, mock chain, or any hypothetical message queue, not necessarily just ZeroMQ
    - Deemed low priority due to ambiguous use-case at the time. SundaeSwap's Gummiworm Protocol would benefit from the additional control enabled by the Event Server

- Offline mode intended to persist UTxO state to a file for simplified offline-mode persistence. As a standalone feature, the interface would be too ad-hoc. A less ad-hoc way to keep a single updated UTxO state file, would instead allow for keeping an updated file for any Hydra resource.

# Decision
Each internal hydra event will have a durable, monotonically increasing event ID, ordering all the internal events in the persistence log.

A new abstraction, the EventSink, will be introduced. The node's state will contain a non-empty list of EventSinks. Each internal hydra event will be sent to a non-empty list of event sinks, which will be responsible for persisting or serving that event in a specific manner. When multiple event sinks are specified, they run in order. Active event can change at runtime. Initially, we will implement the following:
 - MockEventSink, which discards the event. This is exclusive to offline mode, since this would change the semantics of the Hydra node in online mode. This is the only way to have an "empty" EventSink list
 - APIBroadcastEventSink, which broadcasts the publicly visible resource events over the websocket API.
    - Subsumes the existing Websocket API.
    - Can be created via CLI subcommand if Websocket client IP is known. Can be created at runtime by the top-level API (APIServerEventSource)
    - Two modes:
        - Full event log mode. Similar to existing Websocket API, broadcasts all events. 
        - Single-resource event log mode. Broadcasts the state changes of a single resource.
    - One APIBroadcastEventSink per listener. A Hydra node running with no one listening would have 0 APIBroadcastEventSink's in the EventSink list
    - Establishing a websocket connection will add a new event sink to handle broadcasting messages
    - Resources should all support JSON content type. UTXO resource, Tx resource, should support CBOR.
 - APIBroadcastResourceSink, which broadcasts the latest resource after a StateChanged event
    - Runs in single-resource event log mode, broadcasting the current state of a single resource.
 - EventFileSink, which updates a file with the state changed by a StateChanged event
    - Two modes:
        - Full event log mode. Encapsulates the existing incremental file persistence. Appends all server events incrementally to a file.
            - One of these in the EventSink list is required in Online mode, for proper Hydra semantics
        - Single-resource event log mode. Incrementally appends an event log file for a single resource
    - Persists StateChanged changes 
 - ResourceFileSink, which updates a file with the latest resource after a StateChanged event
    - Two modes:
        - Full event log mode. Encapsulates the existing non-incremental full file persistence mechanism. Appends all server events incrementally to a file.
        - Single-resource event log mode. Maintains an up-to-date file for a single resource
           - Consuming an up-to-date single resource will no longer be coupled with overall Hydra state format, only the encoding schema of that particular resource
            - Generalizes the UTxO persistence mechanism previously discussed in [offline mode][offline-mode]
            - May be configured to only persist resource state:
                - Periodically, if the last write was more than a certain interval ago
                - On graceful shutdown/SIGTERM
                - Allows for performant in-memory Hydra usage, for offline mode usecases where the transactions ingested are already persisted elsewhere, and only certain resources are considered important

    - One configuration which we expect will be common and useful, is the usage of a ResourceFileSink on the UTxO resource in tandem with a MockEventServer in offline mode.

The event server will be configured via a new subcommand ("initial-sinks"), which takes an unbounded list of positional arguments. Each positional argument adds a sink to the initial event sink list. There is one argument constructor per EventSink type. Arguments are added in-order to the initial EventSink list. The default parameters configure an EventFileSink for full incremental persistence.

The top-level API will change to implement the API changes described in [ADR 25][adr-25]
  - Top level Websocket subscription adds a Full event log EventFileSink
  - API on /vN/ (for some N) will feature endpoints different resources
    - POST verbs emit state change events to modify the resource in question
    - Websocket upgrades on GET emit state change events for the EventSink list (itself a resource) to establish new ongoing Websocket client subscriptions
      - This will expose the new single-resource APIBroadcastEventSink and APIBroadcastResourceSink

<!-- Full EventSource implementation probably should be in its own ADR. This is included here, for now, to give an idea of what the bigger picture is-->
A new abstraction, the EventSource, will be introduced.
  - APIServerEventSource
    - Top level API
      - Top level Websocket API adds a Full event log EventFileSink 
    - Handles non-websocket-upgraded single-shot HTTP API verbs [ADR 25][adr-25]
      - POST verbs emit state change events to modify the resource in question
      - Websocket upgraded verbs modify the EventSink list (itself a resource) to establish new ongoing subscribers

## Consequences

The primary consequence of this is to enable deeper integration and better operationalization of the Hydra node. For example:
- Users may now use the new sinks to implement custom integrations with existing ecosystem tools
- Users may use the file sinks to reduce overhead significantly in Offline mode
- Developers may more easily maintain downstream forks with custom implementations that aren't appropriate for community-wide adoption, such as the Gummiworm Protocol
- Logging, metrics, and durability can be improved or tailored to the application through such integrations

Note that while a future goal of this work is to improve the websocket API, making it more stateless and "subscription" based, this ADR does not seek to make those changes, only make them easier to implement in the future.

[adr-25]: https://hydra.family/head-protocol/adr/25/
[offline-mode]: 2023-10-16_028_offline_adr.md
[#119]: https://github.com/input-output-hk/hydra/pull/119
[zmq]: https://github.com/input-output-hk/hydra/blob/41598800a9e0396c562a946780909732e5332245/CHANGELOG.md?plain=1#L710-
[#230]: https://github.com/input-output-hk/hydra/pull/230