---
slug: 3
title: 
  3. Asynchronous Duplex Client API
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

The [_reactive_ nature of the Hydra node](/adr/2) means that
clients produce a _stream_ of _inputs_ to a node which in turns issues a stream
of _outputs_ representing the outcome of previous inputs or resulting from
interaction with peers in the network.

For example, a client may send a _command_ as _input_, upon which the node might
do something. When that something is finished, a _output_ does indicate that.
However, there might also be an _output_ emitted to the client when another peer
interacted with "our" node.

Queries, messages by clients which do only fetch information from the node, are
not in scope of this ADR.

## Decision

* We use a single, full-duplex communication channel per client connected to a Hydra node
* This is implemented using a simple [Websocket](https://datatracker.ietf.org/doc/html/rfc6455) with messages corresponding to `Input`s and `Output`s.

## Consequences

* Clients needing a synchronous API need to implement it on top
* Clients can receive _outputs_ decorrelated from any _inputs_ and at any time
