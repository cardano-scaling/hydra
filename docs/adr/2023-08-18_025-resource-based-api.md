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
  `StateChanged`.

- Current capabilities of the API:

  - Clients can retrieve the whole history of `StateChanged` messages or
    opt-out using a query parameter - all or nothing.

  - There is a welcome message called `Greetings` which is always sent, that
    contains the last `headStatus`.

  - There exists a `GetUTxO` query-like `ClientInput`, which will respond with a
    `GetUTxOResponse` containing the confirmed UTxO set in an open head, or (!)
    the currently committed UTxO set when the head is initializing.

  - While overall `json` encoded, clients can choose choose between `json` or
    binary (`cbor`) output of `transaction` fields in several of these using a
    query parameter.

- Many of these features have been added in a "quick and dirty" way, by monkey
  patching the encoded JSON.

- The current capabalities even do not satisfy all user needs:

  - Need to wade through lots of events to know the latest state (except the
    very basic `headStatus` from the `Greetings`).

  - Need to poll `GetUTxO` _or_ aggregate confirmed transactions on client side
    to know the latest UTxO set for constructing transactions.

  - Inclusion of the whole UTxO set in the head is not always desirable and
    filtering by address would be beneficial. (not addressed in this ADR though)

  - As [ADR-15](/adr/15) also proposes, some clients may not need (or should
    not have) access to administrative information.

- It is often a good idea to separate the responsibilities of Commands and
  Queries (CQRS), as well as the model they use.

## Decision

- Drop `GetUTxO` and `GetUTxOResponse` messages as they advocate a
  request/response way of querying.

- Realize that `ClientInput` data is actually a `ClientCommand` (renaming them)
  and that `ServerOutput` are just `projections` of the [internal event stream
  (see ADR-24)](/adr/24) into read `models` on the API layer.

- Compose the API out of resource `models`, which compartmentalize the domain
  into topics on the API layer.

  - A resource has a `model` type and the _latest_ value is the result of a pure
    `projection` folded over the `StateChanged` event stream, i.e. `project :: model -> StateChanged -> model`.

  - Each resource is available at some HTTP path, also called "endpoint":

    - `GET` requests must respond with the _latest_ state in a single response.

    - `GET` requests with `Upgrade: websocket` headers must start a websocket
      connection, push the _latest_ state as first message and any resource
      state updates after.

    - Other HTTP verbs may be accepted by a resource handler, i.e. to issue
      resource-specific _commands_. Any commands accepted must also be available
      via the corresponding websocket connection.

  - `Accept` request headers can be used to configure the `Content-Type` of the
    response

    - All resources must provide `application/json` responses

    - Some resources might support more content types (e.g. CBOR-encoded binary)

  - Query parameters may be used to further configure responses of some
    resources. For example, `?address=<bech32>` could be used to filter UTxO by
    some address.

- Keep the semantics of `/`, which accepts websocket upgrade connections and
  sends direct/raw output of `ServerOutput` events on `/`, while accepting all
  `ClientCommand` messages.

  - Define `ServerOutput` also in terms of the `StateChanged` event stream
Multiple heads are out of scope now and hence paths are not including a
`<headId>` variable section.

## Consequences

- Clear separation of what types are used for querying and gets subscribed to by
  clients and we have dedicated types for sending data to clients

- Changes on the querying side of the API are separated from the business logic.

- Clients do not need to aggregate data that is already available on the server
  side without coupling the API to internal state representation.

- Separation of Head operation and Head usage, e.g. some HTTP endpoints can be
  operated with authentication.

- Clients have a fine-grained control over what to subscribe to and what to
  query.

- Need to rewrite how the `hydra-tui` is implemented.
