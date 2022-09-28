---
sidebar_position: 3
---

# API Behavior

This page documents the behavior of a `hydra-node` at the API layer. That is, how the system behaves given [ClientInputs](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-ClientInput.html#t:ClientInput) and what [ServerOutputs](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-ServerOutput.html#t:ServerOutput) are produced in response to it. See also the [API reference](https://hydra.family/head-protocol/api-reference/) for more details about individual API messages.

The formalism uses [UML statechart](https://en.wikipedia.org/wiki/UML_state_machine) language where transitions are labeled: `input [condition] / output`. When two outputs (e.g. `A` and `B`) are expected we write `A,B`, while `{A,B}` denotes mutual exclusiveness of outputs.

![](https://www.plantuml.com/plantuml/png/ZP6zJWCn383tF8Ldr8gz0oggG7G112UMgaEANMefSKw92zSYtftaKbrBOM3pp_UTVSuqgOswzSvi60d8jxe3fFGQkLKEOipYOWdZyHNCXPMjuptB6qpw52xXYIpkcZ0BthCQymFwBLKiQpLGmZCZAxSircNsUXLYGU_8uZoLx4_yeIM1oS2Lr9Y-U6pUqqVJPUIpwLwoYwEccylx8RhfMew4NwDdiMtQ19q5MNFqXFKpPSZCXKG8aneCmshPc4Fx51oG84f92GUe_AALiDN7a1Al76LweUm9MfiI9R1hfGYOTST2o-EHgsRCcJvsjDQJmzNsi45VvdtPinAL_z-3Jabmwzqt)

[Edit this diagram](https://www.plantuml.com/plantuml/uml/ZP6zJWCn383tF8Ldr8gz0oggG7G112UMgaEANMefSKw92zSYtftaKbrBOM3pp_UTVSuqgOswzSvi60d8jxe3fFGQkLKEOipYOWdZyHNCXPMjuptB6qpw52xXYIpkcZ0BthCQymFwBLKiQpLGmZCZAxSircNsUXLYGU_8uZoLx4_yeIM1oS2Lr9Y-U6pUqqVJPUIpwLwoYwEccylx8RhfMew4NwDdiMtQ19q5MNFqXFKpPSZCXKG8aneCmshPc4Fx51oG84f92GUe_AALiDN7a1Al76LweUm9MfiI9R1hfGYOTST2o-EHgsRCcJvsjDQJmzNsi45VvdtPinAL_z-3Jabmwzqt)

Not pictured is the `CommandFailed` output, which is implicit emitted whenever an input is used when no transition below applies. Also non-state-changing or life-cycle relevant inputs like `GetUTxO` are not mentioned, as well as outputs like `Greetings`, `InvalidInput`, `PeerConnected`, `PeerDisconnected` and `GetUTxOResponse`.

A special case is the `RolledBack` output. This means that the chain rolled back, but it includes no particular information in which state the Hydra Head is now. Frankly, this is quite hard to use - we will improve on this!, but should not invalidate any of the behavioral rules.

