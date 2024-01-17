---
sidebar_position: 4
---

# API Behavior

This page documents the behavior of a `hydra-node` at the API layer. That is, how the system behaves given [ClientInputs](pathname:///haddock/hydra-node/Hydra-API-ClientInput.html#t:ClientInput) and what [ServerOutputs](pathname:///haddock/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput) are produced in response to it. See also the [API reference](/api-reference/) for more details about individual API messages. The only discrepancy is http `POST /commit` action which is not a state transition but a user action that submits a commit transaction which should produce `Committed` output.

The formalism uses [UML statechart](https://en.wikipedia.org/wiki/UML_state_machine) language where transitions are labeled: `input [condition] / output`. When two outputs (e.g. `A` and `B`) are expected we write `A,B`, while `{A,B}` denotes mutual exclusiveness of outputs.

![](https://www.plantuml.com/plantuml/svg/ZP71JW8n48RlVOevge5mvs0mH2CN8RBnGZWKEiWaxNJJ3hfWV7VRBIw87hnrPhvl_-vq54K7sJchjcGGqDMo1uDn7QWMygpKucO9_VujJ9Y4jAK3yIiCsn86y8pQx2i_ziwHAFK3-YrTpQRp2WRhbhvEUl44pOMPr0TYRPDpj_8X9pscf4dCrP_uj4PEz3UNIwNQvcduXEzLav2Fgdb9hkbLpOJVZgVxfgQ0vhCtPrt7hPUnvmq5XwPy9eUChOzeO5WENLXfAtKSduCTubam2feEoh-esUzavcEabSL4BuGSGgrZn0Xw8nZ09DqIu_AqxA8fTQ7tBMxaR75btDsWRTCXtxCGXV_VmuwGpSxPBm00)

[Edit this diagram](https://www.plantuml.com/plantuml/uml/ZP71JW8n48RlVOevge5mvs0mH2CN8RBnGZWKEiWaxNJJ3hfWV7VRBIw87hnrPhvl_-vq54K7sJchjcGGqDMo1uDn7QWMygpKucO9_VujJ9Y4jAK3yIiCsn86y8pQx2i_ziwHAFK3-YrTpQRp2WRhbhvEUl44pOMPr0TYRPDpj_8X9pscf4dCrP_uj4PEz3UNIwNQvcduXEzLav2Fgdb9hkbLpOJVZgVxfgQ0vhCtPrt7hPUnvmq5XwPy9eUChOzeO5WENLXfAtKSduCTubam2feEoh-esUzavcEabSL4BuGSGgrZn0Xw8nZ09DqIu_AqxA8fTQ7tBMxaR75btDsWRTCXtxCGXV_VmuwGpSxPBm00)

Not pictured is the `CommandFailed` output, which is implicit emitted whenever an input is used when no transition below applies. Also non-state-changing or life-cycle relevant inputs like `GetUTxO` are not mentioned, as well as outputs like `Greetings`, `InvalidInput`, `PeerConnected`, `PeerDisconnected` and `GetUTxOResponse`.

#### API configuration

There are some options for API clients to control the server outputs. Server outputs are controlled using the following query parameters:

+ `history=no` -> Prevents historical outputs display. All server outputs are recorded and when a client re-connects these outputs are replayed unless `history=no` query param is used.
+ `snapshot-utxo=no` -> In case of a `SnapshotConfirmed` message the `utxo` field in the inner `Snapshot` will be omitted.

## Replay of past server outputs

When a `hydra-node` restarts, by default it will load its history from persistence and replay previous server outputs to enable clients to re-establish their state upon re-connection. If that happens, obviously some of these outputs are not relevant anymore. One example of this is the `PeerConnected` and `PeerDisconnected`. To make it possible to determine the end of replayed history, client applications can use the `Greetings`, which will be emitted on every `hydra-node` start. See the `hydra-tui` example client for how this is handled.

Clients can optionally decide to skip history outputs and receive only the `Greetings` and following ones. In order to do that they can use query param `history=no`.

For example if the client wants to connect to a local `hydra-node` and doesn't want to view the server history but also wants to have the transactions encoded as CBOR (base16) and prevent utxo display in `SnapshotConfirmed` messages, they would connect using default port `4001` and the full path `ws://localhost:4001/?history=no`.
