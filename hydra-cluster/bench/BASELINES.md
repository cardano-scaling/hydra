# Snapshot throughput baselines (10x-perf project)

Reference numbers every optimization PR in the 10x snapshot-throughput project
is compared against. Captured before any optimization landed.

## Environment

- CPU: Intel Core Ultra 7 258V (8 cores), 30 GiB RAM
- Kernel: Linux 7.1.1 (NixOS), GHC 9.6.7, cabal -O1
- Base commit: 32ba5d4a3 (master at project start)
- No concurrent load during capture; benchmarks run sequentially.

## Commands

Micro benchmarks (criterion JSON next to each log in `bench-baselines/`):

```sh
BENCH_MAX_UTXO=4000 cabal bench hydra-tx:accumulator-bench \
  --benchmark-options '--time-limit 10 --json accumulator.json'
BENCH_MAX_UTXO=4000 cabal bench hydra-node:snapshot \
  --benchmark-options '--time-limit 10 --json snapshot.json'
```

End-to-end cluster benchmark, one run per dataset (`hydra-node` and
`bench-e2e` cabal-built from the base commit):

```sh
bench-e2e single hydra-cluster/datasets/<dataset>.json \
  --output-directory <dir> --timeout 1800
```

Per-round attribution from the run's node logs:

```sh
scripts/bench-logs-analyze.py <dir>/**/hydra-node-*.log
```

## Baseline numbers

Single capture run, criterion --time-limit 10 (means; raw JSON and logs under
`bench-baselines/`, not committed).

### Micro: accumulator (hydra-tx), per snapshot signing cost

| UTxO size | Build map | Commitment (uncached) | Full cycle (build+hash) |
| --- | --- | --- | --- |
| 10 | 26.0 us | 739 us | 821 us |
| 50 | 138 us | 3.59 ms | 3.88 ms |
| 100 | 290 us | 7.45 ms | 8.17 ms |
| 500 | 1.68 ms | 54.1 ms | 57.2 ms |
| 1000 | 3.48 ms | 164 ms | 170 ms |
| 2000 | 7.27 ms | 531 ms | 572 ms |
| 4000 | 15.3 ms | 2.13 s | 2.26 s |

Shape: the commitment is a ~70 us/point naive MSM (linear, dominates to
n~1000) plus O(n^2) boxed-Integer polynomial expansion (dominates beyond).
The map build (serialization + hashing) is linear at ~3.8 us/output.

### Micro: snapshot (hydra-node), full ReqSn->AckSn update

| cell | full-update | accumulator-only | ledger-reapply | sign-only |
| --- | --- | --- | --- | --- |
| utxo-10/txs-1 | 944 us | 827 us | 50 us | 16 us |
| utxo-100/txs-1 | 8.80 ms | 8.02 ms | 68 us | 17 us |
| utxo-100/txs-10 | 10.2 ms | - | 1.03 ms | - |
| utxo-100/txs-100 | 23.7 ms | - | 8.05 ms | - |
| utxo-1000/txs-1 | 182 ms | 178 ms | 229 us | 16 us |
| utxo-1000/txs-10 | 201 ms | - | 3.36 ms | - |
| utxo-1000/txs-100 | 315 ms | - | 51.9 ms | - |
| utxo-4000/txs-1 | 2.46 s | 2.42 s | 914 us | 17 us |
| utxo-4000/txs-10 | 2.89 s | - | 25.1 ms | - |
| utxo-4000/txs-100 | 3.99 s | - | 237 ms | - |

The accumulator commitment is >90% of full-update at every UTxO size; Ed25519
signing is constant ~16 us; ledger re-application is the txs-count term
(roughly 2x per snapshotted tx: requireApplyTxs + pruneTransactions).

### End-to-end cluster

| dataset | TPS | snapshots/s | conf p50 (ms) | conf p95 (ms) | snapshots |
| --- | --- | --- | --- | --- | --- |
| 1-node.json | 477.4 | 6.37 | 616 | 622 | 4 |
| 3-nodes.json | 613.9 | 6.82 | 1357 | 1452 | 10 |
| 1-node-1kutxo.json | 151.0 | 1.76 | 2711 | 3958 | 7 |

Caveats: the committed 1-node/3-nodes datasets submit all transactions in a
burst, so few snapshots are observed and snapshots/s is dominated by round
latency, not sustained throughput. The tiny-UTxO regime numbers are the
network/loop fixed-cost signal; the plateau dataset is the accumulator
signal. The 1kutxo run reported "Fanout failed" while still fanning out 1000
outputs; final partial-fanout completion at this size is flaky pre-M1.

Per-round attribution on the 1kutxo run (scripts/bench-logs-analyze.py over
the node log): ReqSn input processing p50 446 ms (micro accumulator share
~180 ms at n=1000; the rest is snapshot-event JSON encoding, API projections
and state aggregation on the single node-loop thread), snapshot round wall
time p50 458 ms, implying a 2.2 snapshots/s ceiling on this hardware.

## Named baseline quantities

- `A_1k` = 178 ms, `A_4k` = 2.42 s: micro accumulator-only at UTxO 1000 / 4000
- `C_1k` = 201 ms, `C_4k` = 2.89 s: micro full-update at (utxo-1000, txs-10) / (utxo-4000, txs-10)
- `T_small` = 614 tx/s: end-to-end TPS on 3-nodes.json
- `S_small` = 6.8 /s: snapshots/s on 3-nodes.json
- `S_1k` = 1.76 /s: snapshots/s on 1-node-1kutxo.json

## The 10x definition

- Large-UTxO regime: `full-update(1000,10) <= C_1k / 10` and
  `full-update(4000,10) <= C_4k / 10` on the same hardware, and
  `S_1k' >= 10 * S_1k` on the cluster bench, subject to the network floor
  below.
- Small-UTxO regime: `T_small' >= 10 * T_small` on 3-nodes.json.
- No dataset regresses more than 5% at any milestone.

Network floor caveat: a snapshot round cannot complete faster than Ed25519
signing plus one etcd Raft commit and watch delivery per message. If the
cluster-level rate hits that floor before 10x, the floor (measured via
`bench-logs-analyze.py` round times minus CPU share) becomes the documented
target and the residual budget shifts to the network milestone (M3).

## Results after the first optimization pass (2026-07-07, same hardware)

Single runs, cabal-built binaries, for orientation; CI same-runner A/B is the
authoritative per-PR comparison. Changes: rust-FFI commitment, shared
commitment cache, incremental accumulator, lazy signable bytes, event
encoding on the writer thread, network effects dispatched first, etcd
connection reuse, batched etcd broadcasts (ProtocolVersion 2).

Micro (per-snapshot signing work):

| cell | baseline | after | factor |
| --- | --- | --- | --- |
| commitment @1000 | 164 ms | 6.5 ms | 25x |
| commitment @4000 | 2.13 s | 25.4 ms | 84x |
| full-update utxo-1000/txs-10 (C_1k) | 201 ms | 9.3 ms | 21.7x |
| full-update utxo-4000/txs-10 (C_4k) | 2.89 s | 43.7 ms | 66x |

End-to-end (maxTxsPerSnapshot = 100 unless noted):

| dataset | metric | baseline | after | cap=1000 experiment |
| --- | --- | --- | --- | --- |
| 1-node | TPS | 477 | 1961 | 2157 |
| 3-nodes | TPS (T_small) | 614 | 1374 | 2211 |
| 1-node-1kutxo | TPS | 151 | 248 | 387 |
| 1-node-1kutxo | snapshots/s (S_1k) | 1.76 | 2.90 | 1.93 |

Caveats: the committed datasets are too small (300-900 txs) to show sustained
throughput at these rates; client-side submission begins to bound the runs.
Remaining identified costs: ReqSn input processing carries ~200ms of state
aggregation at 1k UTxO done twice per event (node loop + API projection
duplicate, see XXX at API/Server.hs), and the round floor is now one Raft
commit + watch delivery. The maxTxsPerSnapshot default was left at 100; the
cap=1000 column is the experiment data for changing it.

## Sustained-load sweep and the maxTxsPerSnapshot promotion (2026-07-07)

Larger generated datasets (not committed; bench-baselines/datasets-large/):
`3-nodes-9k` = 3 clients x 3000 constant self-transfers; `1-node-1kutxo-3k` =
plateau 1000 with 3000 txs. Unlike the committed burst datasets these hold a
deep transaction backlog, exposing per-round costs that scale with the
backlog (pruneTransactions and the localUTxO refold walk all pending txs on
every snapshot).

| config | 3-nodes-9k TPS | conf p50 | 1kutxo-3k TPS | peak node RSS |
| --- | --- | --- | --- | --- |
| cap=100, queue=100 (old defaults) | 264 | 24.8 s | 77 | 824 MB |
| cap=1000, queue=100 | 1502 | 5.2 s | 357 | 826 MB |
| cap=1000, queue=500 | 1585 | 4.9 s | 367 | 828 MB |

Memory is flat, the input-queue raise is marginal (left at 100, see #2442),
and cap=1000 recovers 4.6-5.7x under sustained load, so the default was
promoted to 1000 (leader-side only; followers accept larger requests, pinned
by a HeadLogicSpec test).

Logging share probe: running the 1kutxo dataset with `hydra-node -q` (no
logging) improves TPS by ~40% (170 -> 237) and conf p50 by 630 ms. The
tracer serializes multi-hundred-KB LogicOutcome envelopes and blocks the
node loop when its 500-slot queue fills; evidence attached to issue #2685
rather than changing logging semantics here.

The large datasets are not committed (multi-MB); regenerate with:

```sh
bench-e2e datasets --number-of-txs 3000 --cluster-size 3        # 3-nodes-9k
bench-e2e datasets --utxo-size 'Plateau 1000' --number-of-txs 3000 --cluster-size 1
```
