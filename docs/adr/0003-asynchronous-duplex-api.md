# 3. Asynchronous Duplex Client API

Date: 2021-06-07

## Status

Accepted

## Context

The [_reactive_ nature of the Hydra node](0001-record-architecture-decisions) means _Request-response_ paradigm is not a good fit to provide a _Client API_: Clients input a _stream_ of _commands_ to a node which in turns issues a stream of _responses_ representing the observable outcome of previous commands and interaction with peers in the network.

## Decision

* The main API is exposed using the [Websocket](https://datatracker.ietf.org/doc/html/rfc6455) protocol with inputs and outputs conforming to some textual representation of `Command`s and `Result`s.

## Consequences

* Client software should be implemented accordingly and most notably cannot rely on any kind of synchronous response to give feedback to potential users or higher-level clients.
* Clients can receive "responses" decorrelated from any requests, for example as the consequence of actions carried out by other nodes
