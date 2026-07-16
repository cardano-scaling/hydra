---
slug: 34
title: |
  34. Binary CBOR encoding for the client API
authors: [v0d1ch]
tags: [Accepted]
---

## Status

Accepted

## Context

- Following [ADR-9](/adr/9), the client API (WebSocket and HTTP) of the `hydra-node` exchanges JSON messages exclusively.

- During high-throughput operation (notably the Hydra Doom event, see [#1585](https://github.com/cardano-scaling/hydra/issues/1585)), the JSON encoding proved to be a significant overhead: transactions of ~480 bytes inflate to several kilobytes because they are hex-encoded inside a JSON text envelope. [#2543](https://github.com/cardano-scaling/hydra/issues/2543) suggested a binary protocol option over the WebSocket for the same reason.

- Baseline measurements (criterion micro-benchmarks over the API hot-path messages) show a `SnapshotConfirmed` over a 1000-UTxO head weighs ~446 KB of JSON, costing ~20 ms to encode on the node and ~48 ms to decode in the client, per message and per connected client. The `?snapshot-utxo=no` filter even increases server-side cost because it rewrites already-encoded JSON bytes.

- The network layer already serializes messages with CBOR via the `cardano-binary` `ToCBOR`/`FromCBOR` classes (see `Hydra.Network.Message`), establishing a codec convention: a leading text tag naming the constructor, followed by the constructor fields in declaration order. Node-to-node traffic is therefore already binary; the client API is the only JSON wire surface.

- gRPC/protobuf was considered and rejected: the dominant payloads are Cardano ledger types whose canonical serialization is CBOR, so protobuf messages would either wrap opaque CBOR blobs (no payload-level schema benefit) or require modeling the entire ledger in proto. gRPC would furthermore replace the WebSocket API instead of extending it, breaking browser-based clients.

## Decision

- All client API message types (and the protocol types they embed) get **native, hand-written `ToCBOR`/`FromCBOR` instances** following the existing `Hydra.Network.Message` convention. Text tags are identical to the JSON `tag` values, so both encodings share one schema vocabulary.

  - Types with derived fields (e.g. the `Snapshot` accumulator, `SeenSnapshot` signable bytes) mirror their JSON instances: derived data is not transmitted and gets reconstructed on decode.

- The **client API encoding is negotiated per connection**, JSON remains the default and no node-wide flag exists:

  - WebSocket clients opt in with the `encoding=cbor` query parameter; messages are then exchanged as binary frames containing one CBOR term (`Cardano.Binary.serialize'` / `decodeFull'`). The server resolves the negotiated encoding once per connection into a `WsCodec` and dispatches on that, never on the frame type.
  - HTTP clients negotiate per request with `Content-Type: application/cbor` (request bodies) and `Accept: application/cbor` (responses).

- For CBOR connections, the `?snapshot-utxo=no` filter is applied as a **typed transformation before encoding** instead of the byte-level rewriting used for JSON: the snapshot `utxo` is sent as an empty set rather than omitted.

- A CI-enforced property suite (`Hydra.CBORSpec`) roundtrips every codec, keeping the hand-written encoder/decoder pairs in sync as types evolve.

## Consequences

- High-throughput deployments can shrink their wire footprint substantially: transaction payloads travel as raw CBOR bytes instead of hex text inside JSON, and JSON syntax overhead disappears (see `benchmarks` on [#2543](https://github.com/cardano-scaling/hydra/issues/2543) for before/after numbers).

- Existing clients (JSON) are entirely unaffected; hydra-cluster tests, the bench (`--cbor`) and hydra-tui (`--cbor`) can exercise the binary path.

- Every new constructor in an API type now needs a CBOR codec next to its JSON instance; the roundtrip property suite fails CI when one is missing or inconsistent.

- Event persistence is unchanged (JSON in SQLite), and the log stream remains JSON; a binary log format can be revisited separately.
