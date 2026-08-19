# API behavior

:::caution Deprecated
This page will soon move into the [API reference](/api-reference) itself.
:::

This page documents the behavior of a `hydra-node` at the API layer. That is, how the system behaves given [ClientInputs](pathname:///haddocks/hydra-node/Hydra-API-ClientInput.html#t:ClientInput) and what [ServerOutputs](pathname:///haddocks/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput) are produced in response to it. See also the [API reference](/api-reference/) for more details about individual API messages. The only discrepancy is http `POST /commit` action which is not a state transition but a user action that drafts a deposit transaction — once signed and submitted on-chain, the node will emit `CommitRecorded` and eventually `CommitFinalized` outputs.

The formalism uses [UML statechart](https://en.wikipedia.org/wiki/UML_state_machine) language where transitions are labeled: `input [condition] / output`. When two outputs (e.g. `A` and `B`) are expected we write `A,B`, while `{A,B}` denotes mutual exclusiveness of outputs.

![](https://www.plantuml.com/plantuml/svg/ZP71JW8n48RlVOevge5mvs0mH2CN8RBnGZWKEiWaxNJJ3hfWV7VRBIw87hnrPhvl_-vq54K7sJchjcGGqDMo1uDn7QWMygpKucO9_VujJ9Y4jAK3yIiCsn86y8pQx2i_ziwHAFK3-YrTpQRp2WRhbhvEUl44pOMPr0TYRPDpj_8X9pscf4dCrP_uj4PEz3UNIwNQvcduXEzLav2Fgdb9hkbLpOJVZgVxfgQ0vhCtPrt7hPUnvmq5XwPy9eUChOzeO5WENLXfAtKSduCTubam2feEoh-esUzavcEabSL4BuGSGgrZn0Xw8nZ09DqIu_AqxA8fTQ7tBMxaR75btDsWRTCXtxCGXV_VmuwGpSxPBm00)

[Edit this diagram](https://www.plantuml.com/plantuml/uml/ZP71JW8n48RlVOevge5mvs0mH2CN8RBnGZWKEiWaxNJJ3hfWV7VRBIw87hnrPhvl_-vq54K7sJchjcGGqDMo1uDn7QWMygpKucO9_VujJ9Y4jAK3yIiCsn86y8pQx2i_ziwHAFK3-YrTpQRp2WRhbhvEUl44pOMPr0TYRPDpj_8X9pscf4dCrP_uj4PEz3UNIwNQvcduXEzLav2Fgdb9hkbLpOJVZgVxfgQ0vhCtPrt7hPUnvmq5XwPy9eUChOzeO5WENLXfAtKSduCTubam2feEoh-esUzavcEabSL4BuGSGgrZn0Xw8nZ09DqIu_AqxA8fTQ7tBMxaR75btDsWRTCXtxCGXV_VmuwGpSxPBm00)

Not pictured is the `CommandFailed` output, which is implicit emitted whenever an input is used when no transition below applies. Also non-state-changing or life-cycle relevant inputs like `Greetings` are not mentioned, as well as outputs like `InvalidInput`, `NetworkConnected` and `NetworkDisconnected`.

#### API configuration

There are some options for API clients to control the server outputs. Server outputs are controlled using the following query parameters:

+ `history=yes` -> Replays historical outputs on connection. All server outputs are recorded, but a connecting client is sent only the outputs produced from then on unless it asks for the history with `history=yes`.
+ `snapshot-utxo=no` -> In case of a `SnapshotConfirmed` message the `utxo` field in the inner `Snapshot` will be omitted.
+ `encoding=cbor` -> All messages on this connection are exchanged as binary WebSocket frames containing a compact CBOR encoding instead of JSON text frames. Each message starts with a text tag identical to the JSON `tag` value, followed by the constructor fields in declaration order. HTTP endpoints negotiate the same encoding per request via `Content-Type: application/cbor` (request bodies) and `Accept: application/cbor` (responses). Note that combined with `snapshot-utxo=no`, the snapshot `utxo` is sent as an empty set rather than omitted.
+ `address=$address` -> In the case of a `TxValid` or a `TxInvalid` message, it will be filtered if its `transaction` address does not contain a reference to the provided. In the case of a `SnapshotConfirmed` message, it will be filtered if its `confirmed` transactions do not contain an address that references the one provided. An `address` given without a value, as in `?address` or `?address=`, is ignored rather than applied as a filter that matches nothing.

## Replay of past server outputs

A `hydra-node` records all server outputs in persistence, and a client that asks for them with `history=yes` gets them replayed on connection so it can re-establish its state. Some of those outputs are obviously no longer relevant when replayed, `NetworkConnected` and `NetworkDisconnected` being the clearest examples. To make the end of the replayed history recognisable, client applications can use the `Greetings`, which is emitted after the history on every connection. See the `hydra-tui` example client for how this is handled.

Replay is opt-in: a client that passes no `history` parameter receives only the `Greetings` and the outputs produced from then on.

For example, a client that wants the server history from a local `hydra-node` but no utxo display in `SnapshotConfirmed` messages would connect on the default port `4001` with the full path `ws://localhost:4001/?history=yes&snapshot-utxo=no`.
