---
slug: 29
title: |
  29. EventServer abstraction for event persistence
authors: []
tags: []
---

## Status
N/A

## Context

The current Hydra API is materialized as a firehose WebSocket connection: upon connecting, a client receives a deluge of information, such as all past transactions. Additionally, there is no way to pick and choose which messages to receive, and not all events are exposed via the API.

This forces some awkward interactions when integrating with a Hydra node externally, such as when using it as a component in other protocols, or trying to operationalize the Hydra server for monitoring and persistence. Here's just a few of the things that prove to be awkward:
 - An application would need to store it's own notion of progress through the event log, so it knew which messages to "ignore" on reconnection
 - An application that wanted to write these hydra events to a message broker like RabbitMQ or Kinesis would need it's own custom integration layer
 - An application that only cared about a limited set of message types may be overwhelmed (and slowed down) by the barage of messages on connection
 - An application that wanted a deeper view into the workings of the hydra protocol would have no access to several of the internal event types 

Additionally, much of the changes that would need to happen to solve any of these would apply equally well to the on-disk persistence the Hydra node currently provides.

-------------------------------------

- Current client API is a stateful WebSocket connection. Client API has its own incrementally persisted state that must be maintained, and includes a monotonically increasing `Seq` ID, but this is based off the events a client observes, not the Hydra host's event log.

- Client must be flexible and ready to handle many different events

- There is no way to simply hand off transactions to the hyrda-node currently, a full connection must be initiated before observed transactions can be applied, and bulky JSON objects can slow down "bulk" operations

- Many applications, like SundaeSwap's Gummiworm Protocol, do not need need the full general API, and benefit from any performance improvement

- The challenge of subscribing to event types is complex to handle, but would be applicable to many parts of Hydra, not just for subscribing to new transactions. It's a good fit for passing stuff off to a message queue (MQTT, Kafka, whatever), probably from a dedicated process (see "Chain Server" below)
    - Could also just directly use a "compatible" spec like STOMP or MQTT, but that would lock in implementation details

- Previous mock chain using ZeroMQ was [removed][zmq] as part of [#119][#119], due to complexity and feeling of ZeroMQ being unmaintained (primary author no longer contributing, new tag release versions not being published)
    - This mock chain was used to mock the layer L1 chain, not the L2 ledger
- Attempt in February 2023 to externalize chainsync server as part of [#230][#230]
    - Similar to [#119][#119], but the "Chain Server" component in charge of translating Hydra Websocket messages into direct chain, mock chain, or any hypothetical message queue, not necessarily just ZeroMQ
    - Deemed low priority due to ambiguous use-case at the time. SundaeSwap's Gummiworm Protocol would benefit from the additional control enabled by the Event Server

# Decision

A new abstraction, the EventServer, will be introduced. Each internal hydra event will be sent to the event server, which will be responsible for persisting that event and returning a durable monotonically increasing global event ID. Different implementations of the event server can handle this event differently. Initially, we will implement the following:
 - MockEventServer, which increments a counter for the ID, and discards the event
 - FileEventServer, which increments a counter for the ID, and encapsulates the existing file persistence mechanism
 - SocketEventServer, which increments a counter for the ID, and writes the event to an arbitrary unix socket
 - WebsocketBroadcastEventServer, which broadcasts the publicly visible events over the websocket API
 - UTxOStateServer, which increments a counter for the ID, and updates a file with the latest UTxO after the event
    - Generalizes the UTxO persistence mechanism introduced in [offline mode][offline-mode]
    - May be configured to only persist UTxO state periodically and/or on SIGTERM, allowing for performant in-memory Hydra usage. This is intended for offline mode usecases where the transactions ingested are already persisted elsewhere.
    - One configuration which we expect will be common and useful, is the usage of a MultiplexingEventServer configured with a primary MockEventServer, and a secondary UTxOStateServer, which will persist the UTxO state to disk.
 - MultiplexingEventServer, which has a primary event server (which it writes to first, and uses its ID) and a list of secondary event servers (which it writes to in sequence, but discards the ID)

New configuration options will be introduced for choosing between and configuring these options for the event server. The default will be a multiplexing event server, using the file event server as its primary event server, and a websocket broadcast event server as its secondary event server.

## Consequences

The primary consequence of this is to enable deeper integration and better operationalization of the Hydra node. For example:
- Users may now use the SocketEventServer to implement custom integrations with existing ecosystem tools
- To avoid the overhead of a unix socket, they may submit pull requests to add integrations with the most popular tools
- Developers may more easily maintain downstream forks with custom implementations that aren't appropriate for community-wide adoption, such as the Gummiworm Protocol
- Developers can get exposure to events that aren't normally surfaced in the websocket API
- Logging, metrics, and durability can be improved or tailored to the application through such integrations

Note that while a future goal of this work is to improve the websocket API, making it more stateless and "subscription" based, this ADR does not seek to make those changes, only make them easier to implement in the future.

[offline-mode]: 2023-10-16_028_offline_adr.md
[#119]: https://github.com/input-output-hk/hydra/pull/119
[zmq]: https://github.com/input-output-hk/hydra/blob/41598800a9e0396c562a946780909732e5332245/CHANGELOG.md?plain=1#L710-
[#230]: https://github.com/input-output-hk/hydra/pull/230