---
slug: 34
title: |
  34. Binary CBOR encoding for client API and logs
authors: [v0d1ch]
tags: [Accepted]
---

## Status

Accepted

## Context

- Following [ADR-9](/adr/9), the `hydra-node` emits JSON logs to `stdout` and the client API (WebSocket and HTTP) exchanges JSON messages exclusively.

- During high-throughput operation (notably the Hydra Doom event, see [#1585](https://github.com/cardano-scaling/hydra/issues/1585)), the JSON encoding proved to be a significant overhead: transactions of ~480 bytes inflate to several kilobytes because they are hex-encoded inside a JSON text envelope, and nodes produced terabytes of output within days at ~200 TPS. [#2543](https://github.com/cardano-scaling/hydra/issues/2543) suggested a binary protocol option over the WebSocket for the same reason.

- The network layer already serializes messages with CBOR via the `cardano-binary` `ToCBOR`/`FromCBOR` classes (see `Hydra.Network.Message`), establishing a codec convention: a leading text tag naming the constructor, followed by the constructor fields in declaration order.

## Decision

- All client API message types and the whole log message closure (`Envelope (HydraLog tx)`) get **native, hand-written `ToCBOR`/`FromCBOR` instances** following the existing `Hydra.Network.Message` convention. Text tags are identical to the JSON `tag` values, so both encodings share one schema vocabulary.

  - Types carrying raw `Aeson.Value` payloads (e.g. `APIServerLog`, `EtcdLog`) embed the standard JSON-in-CBOR mapping from `cborg-json` for those fields.
  - Types with derived fields (e.g. the `Snapshot` accumulator, `SeenSnapshot` signable bytes, the `Environment` signing key) mirror their JSON instances: derived data is not transmitted and gets reconstructed (or replaced with a placeholder) on decode.

- The **client API encoding is negotiated per connection**, JSON remains the default and no node-wide flag exists:

  - WebSocket clients opt in with the `encoding=cbor` query parameter; messages are then exchanged as binary frames containing one CBOR term (`Cardano.Binary.serialize'` / `decodeFull'`). The server dispatches on the negotiated encoding, never the frame type.
  - HTTP clients negotiate per request with `Content-Type: application/cbor` (request bodies) and `Accept: application/cbor` (responses).

- The **log stream encoding is a node-wide setting**: `--log-format json|cbor` (default `json`, also settable via the YAML config). The CBOR log stream is an [RFC 8742](https://www.rfc-editor.org/rfc/rfc8742) CBOR sequence; every item is wrapped in tag 55799 ("self-described CBOR"), so files start with the magic bytes `D9 D9 F7`, and appending/concatenating log files remains safe.

- A `hydra-node convert-logs` subcommand converts CBOR logs back to the usual JSON lines for inspection, streaming with constant memory, auto-detecting JSON input, and converting the decodable prefix of truncated files.

- A CI-enforced property suite (`Hydra.CBORSpec`) roundtrips every codec, keeping the hand-written encoder/decoder pairs in sync as types evolve.

## Consequences

- High-throughput deployments can shrink their wire and log footprint substantially: transaction payloads travel as raw CBOR bytes instead of hex text inside JSON, and JSON syntax overhead disappears.

- Existing clients (JSON) are entirely unaffected; hydra-cluster tests and hydra-tui can exercise the binary path (`--cbor` flag in the TUI).

- Every new constructor in an API or log type now needs a CBOR codec next to its JSON instance; the roundtrip property suite fails CI when one is missing or inconsistent.

- Event persistence is unchanged (JSON in SQLite) and can be revisited separately.
