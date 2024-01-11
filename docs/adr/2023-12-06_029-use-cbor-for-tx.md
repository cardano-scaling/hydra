---
slug: 30
title: |
  30. Use CBOR in external representation of Cardano transactions
authors: [abailly]
tags: []
---

## Status

Proposed

## Context

* The [Hydra.Ledger.Cardano](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano.hs#L127] module provides `ToJSON/FromJSON` instances for [Alonzo.Tx](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano/Json.hs#L361)
  * We have specified this format as part of [Hydra API](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/json-schemas/api.yaml#L1473)
* These instances appear in a few places as part of Hydra API:
  * In the [ServerOutput](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/ServerOutput.hs#L51) sent by the node to clients
  * In the [HydraNodeLog](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Node.hs#L122) as part of Hydra's logging output
  * In the [StateChanged](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/HeadLogic/Outcome.hs#L46) events which are persisted and allow hydra-node to restart gracefully after stopping
* In other places the hydra-node produces, expects, or accepts a CBOR-encoded transaction:
  * In the [Network.Message](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Network/Message.hs#L20) exchanged between the nodes
  * In the [ClientInput](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/ClientInput.hs#L9) from clients submitting `NewTx` commands
  * In the [HTTPServer](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/HTTPServer.hs#L297) API
* Note that in the latter 2 cases, the hydra-node _accepts_ a hex-CBOR-encoded _JSON string_ to represent a transaction and this particular case is handled directly in the [FromJSON](https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano/Json.hs#L388) instance for transactions where 3 different representations are even accepted:
  * JSON object detailing the transaction
  * A JSON string representing CBOR-encoding of a transaction
  * Or a `TextEnvelope` which wraps the CBOR transaction in a simple JSON object
* Using JSON-based representation of Cardano transactions is problematic because:
  * The representation we are providing is not _canonical_ nor widely used, and therefore require maintenance when the underlying cardano-ledger API changes
  * **More importantly** the JSON representation contains a `txId` field which is computed from the CBOR encoding of the transaction. When this encoding changes, the transaction id changes even though no other part of the transaction has changed. This implies that we could send and receive transactions with incorrect or inconsistent identifiers.
* This is true for any content-addressable piece of data, eg. any piece of data whose unique identifier is derived from the data itself, but not of say UTxO which is just data.

## Decision

* Drop support of "structured" JSON encoding of transactions in log messages, external APIs, and local storage of a node state
* Require JSON encoding for transactions that consists in:
  * A `cborHex` string field containing the base16 CBOR-encoded transaction
  * An optional `txId` string field containing the base16 encoding of the Blake2b256 hash of the transaction's bytes
  * When present, the `txId` MUST be consistent with the `cborHex`. This will be guaranteed for data produced by Hydra, but input data (eg. through a `NewTx` message) that does not respect this constraint will be rejected

## Consequences

* By providing a `txId` field alongside the CBOR encoding, we still allow clients to observe the lifecycle of a transaction inside a Head as it gets validated and confirmed without requiring from them to be able to decode the CBOR body and compute the txId themselves
  * This is particularly important for monitoring which usually does not care about the details of transactions
* We should point users to existing tools for decoding transactions' content in a human-readable format as this can be useful for troubleshooting:
  * `cardano-cli transaction view --tx-file <path to tx enveloppe file>` is one example
* We need to _version_ the data that's persisted and exchanged, e.g the Head state and network messages, in order to ensure nodes can either gracefully migrate stored data or detect explicitly versions inconsistency
* We should use the [cardanonical](https://github.com/CardanoSolutions/cardanonical) schemas should the need arise to represent transaction in JSON again
