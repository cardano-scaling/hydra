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

- Do not add a general purpose querying interface (e.g. graphql) to the
  `hydra-node` and stay true to [ADR-3](/adr/3) of using duplex communication
  channels (using websockets).

- **Drop** `GetUTxO` and `GetUTxOResponse` messages as they advocate a
  request/response way of querying.

- Realize that `ClientInput` data is actually a `ClientCommand` (renaming them)
  and that `ServerOutput` are just `projections` of the [internal event stream
  (see ADR-24)](/adr/24) into read `models` on the API layer.

- Compose the API out of resource `models`, which compartmentalize the domain
  into topics on the API layer.

  - Each resource has a _latest_ state, which can be queried or clients may
    subscribe to any model changes.

  - A resource's `model` type needs to be a result of a pure `projection` from
    server output events, i.e. `project :: model -> StateChanged -> model`.

  - Each resource is available at some HTTP path, also called "endpoint":

    - `GET` requests must respond with the _latest_ state in a single response.

    - `GET` requests with websocket upgrade headers must start a websocket
      connection, push the _latest_ state as first message and any resource
      state updates after.

    - Other HTTP verbs may be accepted by a resource handler, i.e. to issue
      resource-specific commands. Any commands accepted must also be available
      via the corresponding websocket connection.

  - Each resource is only available as `JSON` encoding (for now).

    - (TBD): already specify that `Accept` headers should be used to negotiate a
      `content-type`?

- (TBD): Keep the direct/raw output of `ServerOutput` events on `/`, which
  also accepts all `ClientCommand` messages.

### Resources

Resource paths + HTTP verbs mapped to existing things:

| Path                        | GET                         | POST                   | PATCH   | DELETE             |
| :-------------------------- | :-------------------------- | :--------------------- | ------- | :----------------- |
| `/node/head/status`         | `HeadStatus(..)`            | -                      | -       | -                  |
| `/node/head/utxo`           | confirmed utxo              | -                      | -       | -                  |
| `/node/head/transactions`   | confirmed txs               | `NewTx` + responses    | -       | -                  |
| `/node/head/ledger`         | `localUTxO` and `localTxs`  | -                      | -       | -                  |
| `/node/head/commit`         | -                           | `Chain{draftCommitTx}` | -       | -                  |
| `/node/head`                | all `/node/head/*` data     | `Init`                 | `Close` | `Fanout` / `Abort` |
| `/node/protocol-parameters` | current protocol parameters |                        |         |                    |
| `/node/cardano-transaction` | -                           | `Chain{submitTx}`      | -       | -                  |
| `/node/peers`               | a list of peers             | -                      | -       | -                  |
| `/node`                     | all `/node/*` data          | -                      | -       | -                  |

Each `GET` entry would also be available as a websocket on HTTP connections with
the `Upgrade: websocket` header and deliver the same data as repeated `GET`
queries would yield.

Similarly, any `POST`, `PUT`, `PATCH` or `DELETE` request bodies (= commands)
must be accepted on the corresponding websocket and have the same effect.

Multiple heads are out of scope now and hence paths are not including a
`<headId>` variable section.

## Consequences

- Clear separation of what types are used for querying and gets subscribed to by
  clients and we have dedicated types for sending data to clients (TBD: only if
  we not keep the raw `/`).

- Changes on the querying side of the API are separated from the business logic
  (Head protocol).

- Clients do not need to aggregate data that is already available on the server
  side without coupling the API to internal state representation.

- Need to rewrite how the `hydra-tui` is implemented.

- Separation of Head operation and Head usage, e.g. some HTTP endpoints can be
  operated with authentication.
