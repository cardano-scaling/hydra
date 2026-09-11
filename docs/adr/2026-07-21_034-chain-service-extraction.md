---
slug: 34
title: |
  34. Extract chain layer into a dedicated chain service
authors: [v0d1ch]
tags: [Proposed]
---

## Status

Proposed

## Context

- Every `hydra-node` embeds its own chain layer today: `Hydra.Chain.Direct`
  (Ouroboros node-to-client protocols over a local socket) or
  `Hydra.Chain.Blockfrost` (REST polling), plus the block-following
  `TinyWallet`, transaction observation, `LocalChainState` and transaction
  submission — all in-process under `hydra-node/src/Hydra/Chain/`.

- Consequently, running N hydra-nodes means every mainchain block is fetched
  and processed N times:
  - Against Blockfrost, API costs multiply linearly with fleet size.
  - Against a local `cardano-node`, chain-sync connections and redundant
    per-node observation work multiply the same way.

- Catching up after downtime replays raw blocks, even though a node only needs
  the handful of head-relevant observations contained in them.

- Several existing building blocks already point toward centralizing this
  concern:
  - [ADR 26](/adr/26) made observation **stateless and pure**
    (`observeHeadTx :: NetworkId -> UTxO -> Tx -> HeadObservation` in
    `hydra-tx`), reusable outside the node.
  - [ADR 32](/adr/32) established the precedent of **delegating a whole
    concern to a supervised external process** (`etcd` for the network layer).
  - `hydra-chain-observer` already follows the chain once and observes **all
    heads in a single pass**.
  - Issue [#2738](https://github.com/cardano-scaling/hydra/issues/2738)
    (a prerequisite to this work) makes a single observer instance handle
    **all Head script versions** via a pre-recorded script hash registry.

- The long-term goal beyond this ADR is a chain backend able to serve
  **thousands of heads** while fetching each block from the upstream source
  **exactly once**, and eventually a `hydra-node` hosting **multiple heads**.
  This ADR is deliberately only the first step: extracting the chain layer.

## Decision

### 1. Extract the chain layer into a dedicated service process

- Evolve `hydra-chain-observer` into the chain service: it already provides
  `follow`, `observeAll` and the `NodeClient` abstraction, and the
  multi-version observation of
  [#2738](https://github.com/cardano-scaling/hydra/issues/2738) lands there.
- The `Direct` and `Blockfrost` backends (including the `ChainBackend`
  type class) move **into** the service; the `hydra-node` run mode no longer
  touches them and talks to the service the way it talks to a `cardano-node`
  today: through a client/server protocol, never through shared state.
- There is **no second executable**: the standalone `hydra-chain-observer`
  binary is absorbed into `hydra-node` as the `hydra-node chain-observer`
  sub-command (following the `publish-scripts` precedent), while the
  `hydra-chain-observer` package remains the library home. Nothing extra to
  distribute or discover at runtime.
- `hydra-explorer` later becomes just another consumer of the same service,
  converging node and explorer observation onto one codebase.
- The offline mode of [ADR 28](/adr/28) stays embedded in `hydra-node` — it
  has no L1 to follow and is not affected by this ADR.

### 2. The service streams typed observations, not blocks

- The service fetches and processes each block **once** (the deduplication
  layer), runs the multi-version `observeHeadTx` over every transaction, and
  streams **per-head observations**, **ticks** and **rollback events** to its
  subscribers.
- Rationale: raw block bandwidth is not the bottleneck (~5 KB/s on mainnet).
  The wins of observation streaming are:
  - per-head catch-up is served from an **observation log** instead of
    replaying weeks of blocks,
  - `hydra-node` sheds block processing entirely,
  - clients stay thin enough that one service instance can serve thousands of
    head subscriptions.
- Version coupling (which script versions the service can observe) is
  resolved by the script hash registry of
  [#2738](https://github.com/cardano-scaling/hydra/issues/2738), a
  prerequisite to this ADR.

### 3. The extraction is interface-preserving toward the head logic

- The `Chain tx m` handle and the `ChainEvent tx` callbacks
  (`Observation{observedTx, newChainState}`, `Rollback`, `Tick`) remain
  **unchanged** — only their implementation moves behind a socket.
- `HeadLogic`, the persistence of `StateChanged` events ([ADR 24](/adr/24))
  and node-side rollback handling ([ADR 23](/adr/23)) are untouched.

### 4. The service is a full L1 gateway

- Besides following the chain, the service also handles **transaction
  submission** (submit, await confirmation) and **queries**: UTxO, protocol
  parameters, era history, system start and tip.
- Rationale: the service caches slow-changing answers (microseconds from
  memory instead of 100–300 ms Blockfrost round-trips) and pools submission
  connections — faster than per-node upstream access, and `hydra-node` ends
  up with **zero L1 credentials**: no Blockfrost project key, no node socket.

### 5. Wire protocol: WebSocket + CBOR

- One WebSocket connection per `hydra-node` carries all of its subscriptions,
  queries and submissions.
- The connection handshake carries the network id: the node states the
  network it expects and the service rejects a mismatch at connect time,
  mirroring the node-to-client `NetworkMagic` handshake, so pointing a node
  at another network's instance fails loudly instead of yielding a silent,
  empty subscription and wrong-network query answers.
- Message inventory:
  - client → service: `Subscribe{participantKeys, headIds, perHeadResumeSeq}`,
    `SubmitTx`, `Query{UTxO | PParams | EraHistory | SystemStart | Tip}`
  - service → client: `Observation{headId, seq, observedTx, newChainState}`,
    `Tick{point, time}`, `Rollback{headId, point, rollbackToSeq}`, query
    responses and submission results
- CBOR is the wire encoding, per the measurements of
  [#2543](https://github.com/cardano-scaling/hydra/issues/2543) (roughly 3×
  smaller on the wire, up to 21× faster to decode than JSON). JSON content
  negotiation may be kept as a debugging aid.

### 6. Subscription by participant key, delivery at-least-once

- A node subscribes with its Cardano verification key hash / on-chain ID. The
  service pushes any `Init` observation involving that key and from then on
  automatically attaches the subscriber to the resulting head ID. This solves
  the bootstrap problem that a node cannot know its head ID before `Init` is
  observed.
- Subscription by key also replays from the log the `Init` observations of
  heads involving that key which are **still live** (not yet fanned out or
  aborted), skipping finalized ones: a node with lost state recovers its
  open heads with no chain-point bookkeeping, replacing today's
  `--start-chain-from` bootstrapping.
- Explicit head-ID subscriptions are also supported (rejoining after restart,
  observer mode).
- Filtering happens **server-side**, sized for thousands of heads.
- Every per-head observation carries a **monotonic sequence number**. On
  (re)connect the node presents its last-processed sequence number per head
  and the service replays from its log: **at-least-once delivery with
  resume**. Duplicates are possible in crash windows and the node
  deduplicates by sequence number — the cursor rides along with the
  `StateChanged` events it already persists.
- Ticks are live-only and never replayed. Rollbacks are per-head, in-stream
  events.

### 7. Deployment: embedded by default, shared via `--chain-observer-url`

- One binary, two ways to run the observer: embedded inside `hydra-node`, or
  standalone via the `hydra-node chain-observer` sub-command.
- Without further configuration, `hydra-node` runs the chain observer as an
  **in-process component** and connects to it over a loopback WebSocket —
  zero config, zero extra processes, one log stream, run-it-yourself by
  construction: the zero-config spirit of the `etcd` setup in
  [ADR 32](/adr/32), without the extra process.
- `hydra-node chain-observer --host <ip> --port <port>` runs **only** the
  observer, serving the WebSocket API on the given address. A fleet operator
  runs this as a long-lived service and points many nodes at it with
  `--chain-observer-url`: the fetch-once invariant then holds **fleet-wide
  from day 1**, and the Blockfrost cost reduction scales with the number of
  nodes and heads served. A service instance follows exactly **one
  network**; a fleet spanning several networks runs one instance per
  network, and nodes point at the instance matching their own network.
- `--start-chain-from` becomes a service-side concern: embedded, the flag
  is forwarded to the in-process observer, which starts following upstream
  from that point when its database is fresh, preserving today's semantics.
  Against a shared instance the flag does not apply: history scanning is
  the service's job and per-head catch-up starts at the head's first
  observation; heads older than the shared instance's own start point are
  the documented limitation listed under non-goals.
- The client code path and connection handshake are identical in both modes
  — the node presents its persisted resume point and its participant keys;
  only the URL differs (loopback vs. remote).
- Failure handling: a crashed embedded component is restarted in-process; a
  lost connection to a shared instance is re-established with backoff. In
  both modes the node stays up; the outage is surfaced loudly (logs and API
  server output); `postTx` fails fast with `PostTxError` while disconnected;
  the observation stream resumes losslessly afterwards via the sequence
  numbers.

### 8. The wallet becomes query-on-demand

- `TinyWallet` stops following the chain. At `postTx` time it fetches fresh
  fuel UTxO for its address through the service query API, and awaits
  confirmation through the service after submitting.
- Signing keys, transaction construction and balancing
  (`Hydra.Chain.Direct.State` builders, `ChainContext`) **stay in
  hydra-node** — the service never holds key material.
- Rationale: posting protocol transactions is rare, so one query per post is
  negligible — and the stale-wallet / reset-on-rollback machinery disappears
  structurally rather than being ported: every post starts from a fresh
  query, so the wallet's view is post-rollback-correct by construction and
  wallet-address rollbacks no longer need to be observed at all. Head-level
  rollbacks are unaffected — they remain per-head `Rollback` events handled
  by the head logic as before.
- Residual races, unchanged from today's tracked wallet: a rollback (or
  external spend) can invalidate a chosen fuel input between query and
  submission — the submission fails and `postTx` is retried against a fresh
  query, as it is today. Consecutive posts within one block window can
  contend for the same fuel input, as they can today; the service, being the
  single submission path, can later overlay in-flight transactions onto
  query results if this needs tightening.

### 9. Persistence: SQLite observation index

- The service persists to an embedded SQLite database (mirroring
  `hydra-node`'s own event store):
  - `observations(head_id, seq, chain_point, block_no, payload_cbor)` with a
    unique index on `(head_id, seq)`,
  - a single-row `cursor` table with the last processed chain point,
  - `recent_blocks(hash, slot, state_checkpoint)` bounded to the rollback
    window `k`, for intersection finding, rollback detection and rewinding
    the observation state: `state_checkpoint` persists the head-relevant
    UTxO threaded through `observeHeadTx`, written atomically with the
    block's observations and copy-on-write: only blocks containing a
    head-relevant transaction write a new checkpoint, all others reference
    the latest one, keeping the window's footprint proportional to head
    activity rather than block count.
- A rollback restores the observation state from the checkpoint at the
  rollback point, deletes the affected heads' observations above it and
  emits in-stream `Rollback` events.
- Why SQLite:
  - Writes arrive at **chain rate** (one block per ~20 s with a few
    observations each), not subscriber rate — fan-out is served from memory,
    so even a shared instance stays far below SQLite's limits.
  - Catch-up reads are `(head_id, seq)` range scans over a covering index.
  - The service is the database's **single writer** (WAL mode, concurrent
    readers) — exactly SQLite's sweet spot.
  - A component that users do not configure cannot depend on a database
    someone must operate: an embedded, zero-ops file database is the only
    choice consistent with the zero-config embedded default. Operators of
    shared instances manage the database file like any other service state.
  - It reuses the team's fresh SQLite event-store experience and idioms from
    `hydra-node`.
- The observation log is append-only in this stage: it grows with L1
  protocol activity only (roughly 100 KB per head lifetime), so pruning is
  deliberately deferred (see non-goals).
- A service restart resumes from the cursor and its observation-state
  checkpoint with **zero upstream re-fetch**.
  Adding a new script version later requires a one-time, bounded backfill
  from the upstream source. Postgres is explicitly deferred to the stage-2
  ADR, where multi-instance/high-availability deployments may warrant it.

### 10. Trust model: trusted, operator-run

- The chain service carries the **same trust assumption** as today's
  `cardano-node` socket or Blockfrost project key: you run it yourself or
  point at infrastructure you trust.
- Stated explicitly: a withholding or lying chain service can censor a
  `Close` observation and cost a node its contest window. The embedded
  default satisfies "run it yourself" by construction; with
  `--chain-observer-url` the operator explicitly takes responsibility for
  pointing at infrastructure they trust — typically their own fleet's
  instance.
- There is **no authentication in v1**: a shared instance must only be
  reachable over a trusted network. Authentication and multi-tenancy
  hardening are the second stage of the roadmap.
- Cryptographic verification of the stream (block headers, Mithril) is a
  named non-goal of this ADR.

### 11. Performance goals (qualitative)

- **Fetch-once invariant**: each block is fetched from the upstream source
  exactly once per service instance.
- Low latency from block arrival to observation delivery.
- Per-head catch-up is served from the index without upstream re-fetch.
- Bounded memory via streaming, in the spirit of [ADR 31](/adr/31).
- The protocol is shaped so that one instance can serve thousands of
  concurrent head subscriptions. Internally: one follower thread (fetch,
  observe, persist, route) fans out to per-connection lightweight threads
  over bounded outbound queues; subscribers that fall behind or reconnect
  catch up from the index and rejoin the live stream by sequence number,
  so the follower never blocks on any subscriber.

### 12. Architecture

```
Before:                                  After:

  hydra-node A                            hydra-node A          hydra-node B
 ┌───────────────────────┐               ┌──────────────┐      ┌──────────────┐
 │ HeadLogic             │               │ HeadLogic    │      │ HeadLogic    │
 │ Chain handle          │               │ Chain handle │      │ Chain handle │
 │  ├─ chain-sync client │               │  └─ WS+CBOR client  │  └─ WS+CBOR client
 │  ├─ observeHeadTx     │               └──────┬───────┘      └──────┬───────┘
 │  ├─ LocalChainState   │                      │ observations,       │
 │  ├─ TinyWallet (sync) │                      │ ticks, rollbacks,   │
 │  └─ submit / queries  │                      │ queries, submit     │
 └──────────┬────────────┘                      ▼                     ▼
            │                            ┌─────────────────────────────────┐
            ▼                            │ chain service (embedded in node │
      Blockfrost / cardano-node          │  or via --chain-observer-url)   │
                                         │  fetch once ─ observe (all      │
                                         │  script versions, #2738)        │
  hydra-node B ── same stack ──┐         │  ─ SQLite observation index     │
            │                  │         │  ─ WS/CBOR fan-out              │
            ▼                  │         │  ─ query cache ─ submission     │
      Blockfrost / cardano-node          └───────────────┬─────────────────┘
      (every block fetched N×)                           ▼
                                          Blockfrost / cardano-node
                                          (every block fetched once)
```

### 13. Staged roadmap

1. **This ADR**: extract the chain layer into a dedicated observer service;
   observation streaming, gateway queries and submission, SQLite index;
   embedded in `hydra-node` by default and shareable across nodes and heads
   via `hydra-node chain-observer` + `--chain-observer-url`.
2. **Hardening shared deployments** (future ADR): authentication,
   multi-tenancy and rate limiting for instances serving many operators;
   high availability, with Postgres if warranted.
3. **Multi-head hydra-node** (future ADR): one node hosting many heads over
   one subscription connection. The chain side is ready after this ADR; the
   remaining work is the per-head `etcd` network layer and head-keyed
   `HeadLogic`, persistence and API.

### 14. Non-goals of this ADR

- Cryptographic verification of the observation stream.
- Authentication and multi-tenant hardening of shared deployments (stage 2).
- Backfill for heads created before the service's start point — a documented
  limitation; targeted backfill via upstream address queries is future work.
- Pruning of the observation log: append-only is affordable in this stage
  since the log grows with L1 protocol activity only. Natural future knobs,
  deferred to the hardening stage: deleting finalized heads' observations
  after a grace period, and etcd-style compaction of long-lived heads'
  logs, with resumes below the retained window rejected explicitly so a
  straggler falls back to fresh joining instead of silently missing events.
- Pinned performance numbers — goals stay qualitative until benchmarks exist.

## Consequences

Positive:

- Each block is fetched once per node, and once per fleet when nodes share
  an instance via `--chain-observer-url` — a direct Blockfrost cost
  reduction and consolidated upstream connections.
- The `hydra-node` run mode gets thinner: no L1 credentials, no block
  processing, no wallet chain-following. The chain code remains linked into
  the binary (it is the observer component) but sits entirely behind the
  protocol boundary.
- Reconnect and restart catch-up become observation-log replays instead of
  block replays — effectively instant per head.
- One observation codebase serves node, explorer and future consumers; the
  multi-version work of
  [#2738](https://github.com/cardano-scaling/hydra/issues/2738) pays off in
  one place.
- The chain side of the multi-head `hydra-node` end goal is done in advance.
- A whole bug class disappears structurally: stale `TinyWallet` state and
  reset-on-rollback races.

Negative / accepted trade-offs:

- A new protocol boundary introduces new failure modes — and in shared
  deployments it is also a process and network boundary. Mitigated by
  in-process restart of the embedded component, reconnect with backoff,
  at-least-once resume, loud outage surfacing and fail-fast `postTx`.
- Wallet UTxO freshness moves to query time — acceptable because protocol
  transaction posting is rare.
- The service is a trusted component: censorship / withholding risk is
  documented rather than cryptographically mitigated, and operators must
  monitor connectivity since deadlines (contest windows) depend on timely
  observations.
- Shared deployments span two processes and machines (log correlation
  needed); the embedded default keeps everything in a single process. The
  `hydra-node` CLI grows a sub-command and a two-mode matrix to test.
- A shared instance has no authentication in v1 — operators must restrict
  network access to it until the hardening stage lands.
- Heads created before the service's start point cannot be served in this
  first stage.
