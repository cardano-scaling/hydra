---
slug: 25
title: |
  25. Event-sourced, resource-based API
authors: []
tags: [Draft]
---

## Status

Draft

## Context

- [ADR-3](/adr/3) concluded that a full-duplex communication channels are
  desirable to interact with a _reactive_ system.

- The Client API communicates several types of messages to clients. Currently
  this ranges from node-level `PeerConnected`, over head-specific `HeadIsOpen`
  to messages about transactions like `TxValid`. These messages are all of type
  `ServerOutput`.

- Current capabilities of the API:

  - Clients can retrieve the whole history of `ServerOutput` messages or
    opt-out using a query parameter - all or nothing.

  - There is a welcome message called `Greetings` which is always sent, that
    contains the last `headStatus`.

  - There exists a `GetUTxO` query-like `ClientInput`, which will respond with a
    `GetUTxOResponse` containing the confirmed UTxO in an open head, or (!) the
    currently committed UTxO when the head is initializing.

  - While overall `json` encoded, clients can choose choose between `json` or
    binary (`cbor`) output of `transaction` fields in several of these using a
    query parameter.

- Many of these features have been added in a "quick and dirty" way, by monkey
  patching the encoded JSON.

- The current capabalities even do not satisfy all user needs:

  - Need to wade through lots of events to know the latest state (except the
    very basic `headStatus` from the `Greetings`).

  - Need to poll `GetUTxO` _or_ aggregate confirmed transactions on client side
    to know the latest UTxO for constructing transactions.

  - Inclusion of the whole UTxO in the head is not always desirable and
    filtering by address would be beneficial. (not addressed in this ADR though)

  - As [ADR-15](/adr/15) also proposes, some clients may not need (or should
    not have) access to administrative information.

- It is often a good idea to separate the responsibilities of Commands and
  Queries (CQRS), as well as the model they use.

## Decision

- Do not add a general purpose querying interface (e.g. graphql) to the
  `hydra-node` and stay true to [ADR-3](/adr/3) of using duplex communication
  channels (using websockets).

- **Drop** `GetUTxO` and `GetUTxOResponse` messages as they advocate a
  request/response way of querying.

- Realize that `ClientInput` data is actually a `ClientCommand` (renaming them)
  and that data available as `ServerOutput` are events that get `projected` into
  read models in the API layer.

  - **Not in scope**, but thinkable in the CQRS context: split HeadLogic into an
    `aggregate` (rolling up events into a `HeadState`) and `policy` part
    (reacting on events to create `Effects`).

- (TBD): Keep the direct/raw output of `ServerOutput` of events on `/`, which
  also accepts all `Clientinput`.

- Compose the API out of resource `models`, which compartmentalize the domain
  into topics on the API layer.

  - Each resource has a _latest_ state and clients may subscribe to changes to
    them.

  - A resource's `model` needs to be a result of a pure `projection` from server
    output events, i.e. `project :: model -> ServerOutput -> model`.

  - Each resource is available at some HTTP path, also called "endpoint", using
    a websocket upgrade connection. (TBD): also define REST-like verbs?

  - Each resource is only available as `JSON` encoding. (TBD): already specify
    that `Accept` headers should be used to negotiate a `content-type`?

### Resources

- `/node` contains node-specific messages

  <!--
    - sends outputs: `PeerConnected`, `PeerDisconnected`
  -->

  - Resources:

    - `/node/peers` with a list of peers

- `/head` contains head-specific messages

  <!--
    - sends outputs: `HeadIsInitializing`, `Committed`, `HeadIsOpen`,
    `HeadIsClosed`, `HeasIsContested`, `ReadyToFanout`, `HeadIsAborte`,
    `HeadIsFinalized`, `RolledBack`, `PostTxOnChainFailed`, `TxValid`,
    `TxInvalid`, `SnapshotConfirmed`
  -->

  - accepts inputs: `Init`, `Abort`, `Close`, `Contest`, `Fanout`, `NewTx`

  - Resources:

    - `/head/state` is a "for-clients" projection of the internal
      `HeadState` that is more flat and does not contain unconfirmed or
      temporary data (only `confirmedSnapshot` data)

## Consequences

- Clear separation of what types are used for querying and gets subscribed to by
  clients and we have dedicated types for sending data to clients (TBD: only if
  we not keep the raw `/`).

- Changes on the querying side of the API are separated from the business logic
  (Head protocol). (TBD: not 100%, we still rely a lot on how the `ClientEffect`
  are yielded in the head logic)

- Clients do not need to aggregate data that is already available on the server
  side without coupling the API to internal state representation.

- Need to rewrite how the `hydra-tui` is implemented.

- Separation of Head operation and Head usage, e.g. one can be operated with
  authentication.

- Paves the way to a fully event-sourced storage in the `hydra-node`.
