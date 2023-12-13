---
slug: 29
title: |
  29. Use CBOR in external representation of Cardano objects
authors: [abailly]
tags: []
---

## Status

Proposed

## Context

* The [Hydra.Ledger.Cardano](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano.hs#L127] module provides `ToJSON/FromJSON` instances for [Alonzo.Tx](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano/Json.hs#L361)
* These representations are not _canonical_ and therefore require maintenance when the underlying cardano-ledger API changes
* The format is [formally specified](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/json-schemas/api.yaml#L1473) as part of Hydra API
* These instances appear in a few places as part of Hydra API:
  * In the [ServerOutput](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/ServerOutput.hs#L51) sent by the node to clients
  * In the [HydraNodeLog](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Node.hs#L122) as part of Hydra's logging output
  * More importantly, in the [StateChanged](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/HeadLogic/Outcome.hs#L46) events which are persisted and allow hydra-node to restart gracefully after stopping
* In other places the hydra-node produces or expects a CBOR-encoded transaction:
  * In the [Network.Message](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Network/Message.hs#L20) exchanged between the nodes
  * In the [ClientInput](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/ClientInput.hs#L9) from clients submitting `NewTx` commands
  * In the [HTTPServer](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/HTTPServer.hs#L297) API
* Note that in the latter 2 cases, the hydra-node _accepts_ a hex-CBOR-encoded _JSON string_ to represent a transaction and this particular case is handled directly in the [FromJSON](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano/Json.hs#L388) instance for transactions where 2 different representations are even accepted:
  * Either directly a JSON string
  * Or using a `TextEnvelope` which wraps the CBOR transaction in a simple JSON object

## Decision

## Consequences

The Hydra node would be usable offline, for transaction validation, and other custom L2 applications. The lifecycle & state machine associated with a Hydra would remain unchanged in both online, and offline mode.

The Hydra node can be deployed and run without an accompanying Cardano node, simplifying deployment and testing.
