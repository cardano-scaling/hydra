---
sidebar_position: 9
---

# HeadLogic execution time

Use the `hydra-node:snapshot` benchmark to locate work in the pure node input
paths. Confirm any improvement with the cluster benchmark: transaction
throughput, confirmation latency and time to drain a backlog are the objectives.
Allocation helps explain a timing result but is not itself a speed improvement.

## Reproducible inputs

From the development shell, start with a small, single-core sample:

```shell
BENCH_MAX_UTXO=10 BENCH_SEED=42 just bench-snapshot \
  '--time-limit 0.1 --resamples 100 --regress cpuTime:iters --match prefix utxo-10/txs-1/acksn/first/full-update +RTS -N1 -T -RTS'
```

Increase the time limit and resamples and remove the name filter for a broader
comparison. Results are written to `test-reports/snapshot-bench.json`. Preserve each run
before starting the next one. Record the commit, GHC version, Cabal optimization
level, RTS options and hardware. The default Cabal build uses `-O1`; use identical
settings for both revisions. Avoid concurrent builds or tests during measurement.

Fixtures use deterministic QuickCheck generators, with separate seeds derived
from `BENCH_SEED`, UTxO size and transaction count. The default grid contains
10, 100 and 1,000 UTxO entries and 1, 10 and 100 transactions per snapshot.
`BENCH_MAX_UTXO=32767` also includes 4,095, 8,191, 16,383 and 32,767 entries.
For a shorter investigation, select a cell with Criterion's `--match prefix`,
for example `--match prefix utxo-1000/txs-100/`.

## What each case measures

| Case | Work |
| --- | --- |
| `reqtx` | Apply a fresh transaction at a follower with no snapshot in flight. |
| `reqsn` | Validate, sign and retain a snapshot containing the selected transactions. |
| `acksn/first`, `acksn/second` | Record one partial acknowledgement, with the fixture's signing cache warmed. |
| `acksn/final-follower`, `acksn/final-leader` | Verify the complete certificate and confirm, with another batch of the same size waiting locally. The next leader also selects a request. |
| `reqsn-and-three-acks` | Process a fresh request and all acknowledgements, aggregating after each input. This includes creation of the candidate cache rather than repeatedly using a warmed fixture. |

Every input has `full-update`, `update-and-aggregate`, and
`update-and-aggregate-and-api` cases. They validate the same successful outcome
and demand its network payload, including the actual signature for `ReqSn`.
The latter two additionally force the resulting node state to WHNF, matching
the strict state update in the node and API projection. The API case includes
its separate fold of individual events through a retained typed runtime.
`aggregate-only` and `api-projection-only` operate on a prepared outcome;
`api-projection-restarted` compares reconstructing the API's machines for each
event. The retained runtime is prepared outside the measurement, as it survives
between inputs in the server.

These projections deliberately exclude networking, persistence, output
encoding and the other API read models. Differences between timings are useful
diagnostics, not exact additive costs: evaluation and sharing can interact.
Fixture drift produces an error instead of silently measuring rejection or a
duplicate-input no-op. Incoming signatures and the initial accumulator are
prepared outside the measurement.

## Comparing changes

Build and retain separate binaries for the merged baseline and each experiment,
using the same benchmark source and fixtures. Alternate execution order within
pairs (baseline/change, then change/baseline), retain the JSON, and compute
within-pair changes before taking medians. Measure signing-byte reuse separately
from skipping request selection at followers.

The opaque `CachedSnapshot` pairs a snapshot with its trusted signing-byte cache.
Its smart constructor derives the bytes, and read-only accessors prevent updating
either part independently. `SeenSnapshot` and the live `Candidate` share this
value through owner restoration and event aggregation. New candidates and
JSON/CBOR decoding derive a new cache; serialization keeps the existing format
and omits the cache. Candidate identity and ordered-party checks still apply,
and final acknowledgement still verifies the aggregate signature. The cache
stays lazy so historical replay does not force unused commitments.

For end-to-end confirmation, use the three workloads and dataset seeds in
`.github/workflows/bench-e2e-diff.yaml`: three-node closed loop (750 transactions,
seed 2754), three-node sustained load (15,000 transactions, seed 2752), and
one-node plateau (1,000 UTxO entries, 4,000 transactions, seed 2753). Generate
each dataset once and reuse its exact bytes. Use the same node and client RTS
options and storage location in each pair. Inspect snapshot counts and batch
sizes alongside elapsed time, throughput and latency; open-loop batch boundaries
can move percentiles substantially. See the [metrics reference](./metrics.md)
for the meaning and limits of each reported value.

To compare changes in CI, dispatch `bench-e2e-diff.yaml` with exact `head_ref`
and `base_ref` commits. Each of the four runners measures one pair, with
opposite execution orders on half the runners. Compare an optimization against
its immediate parent to isolate it, then compare the combined changes against
the merged baseline. An A/A run with both refs equal estimates variation without
a code change. An uncolored row does not prove that performance is unchanged.
