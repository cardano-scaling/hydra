---
sidebar_position: 4
---

# API Behavior

This page documents the behavior of a `hydra-node` at the API layer. That is, how the system behaves given [ClientInputs](pathname:///haddock/hydra-node/Hydra-API-ClientInput.html#t:ClientInput) and what [ServerOutputs](pathname:///haddock/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput) are produced in response to it. See also the [API reference](/api-reference/) for more details about individual API messages.

The formalism uses [UML statechart](https://en.wikipedia.org/wiki/UML_state_machine) language where transitions are labeled: `input [condition] / output`. When two outputs (e.g. `A` and `B`) are expected we write `A,B`, while `{A,B}` denotes mutual exclusiveness of outputs.

![](https://www.plantuml.com/plantuml/svg/ZP4_J_Cm48Vt-nIUNpLwzmweA18Y2o28MAeEBdUX9zddABv02kAxOzVGG65Wkp_FSr5-NaMrWuxUmOwH3FbUzmOfFWpSAmS1MF_RcAewCusmidNyml9ebeVM_3UtP77VXZfupmhm3Vef5InffL324oCf5opM9VPy6uQCNfB59kRkL_ow9qdq9vTRWCDmNbxHSpibvBMTxfEBYtPgv2bNRuixiNtS1Qs3T3numYBdqxKBaT0iIt7yH1a3VAo_W3CIH2aguH7AFsObZ7eJOkIjeqIlbJsPtiP8qTXEQJ2OTCT2cpdQeW6Sw9MZJkUuolKvhpJEAH425ABmVtl65GcEthq3)

[Edit this diagram](https://www.plantuml.com/plantuml/uml/ZP6zJWCn383tF8Ldr8gz0oggG7G112UMgaEANMefSKw92zSYtftaKbrBOM3pp_UTVSuqgOswzSvi60d8jxe3fFGQkLKEOipYOWdZyHNCXPMjuptB6qpw52xXYIpkcZ0BthCQymFwBLKiQpLGmZCZAxSircNsUXLYGU_8uZoLx4_yeIM1oS2Lr9Y-U6pUqqVJPUIpwLwoYwEccylx8RhfMew4NwDdiMtQ19q5MNFqXFKpPSZCXKG8aneCmshPc4Fx51oG84f92GUe_AALiDN7a1Al76LweUm9MfiI9R1hfGYOTST2o-EHgsRCcJvsjDQJmzNsi45VvdtPinAL_z-3Jabmwzqt)

Not pictured is the `CommandFailed` output, which is implicit emitted whenever an input is used when no transition below applies. Also non-state-changing or life-cycle relevant inputs like `GetUTxO` are not mentioned, as well as outputs like `Greetings`, `InvalidInput`, `PeerConnected`, `PeerDisconnected` and `GetUTxOResponse`.

#### API configuration

There are some options for API clients to control the server outputs. Server outputs are controlled using the following query parameters:

+ `history=no` -> Prevents historical outputs display. All server outputs are recorded and when a client re-connects these outputs are replayed unless `history=no` query param is used.
+ `tx-output=cbor` -> Outputs transaction fields encoded as CBOR instead of default JSON.
+ `snapshot-utxo=no` -> In case of a `SnapshotConfirmed` message the `utxo` field in the inner `Snapshot` will be omitted.

## Replay of past server outputs

When a `hydra-node` restarts, by default it will load it's history from persistence and replay previous server outputs to enable clients to re-establish their state upon re-connection. If that happens, obviously some of these outputs are not relevant anymore. One example of this is the `PeerConnected` and `PeerDisconnected`. To make it possible to determine the end of replayed history, client applications can use the `Greetings`, which will be emitted on every `hydra-node` start. See the `hydra-tui` example client for how this is handled.

Clients can optionally decide to skip history outputs and receive only the `Greetings` and following ones. In order to do that they can use query param `history=no`.

For example if the client wants to connect to a local `hydra-node` and doesn't want to view the server history but also want to have the transactions encoded as CBOR (base16) and prevent utxo display in `SnapshotConfirmed` messages, they would connect using default port `4001` and the full path `ws://localhost:4001/?history=no&tx-output=cbor`.
