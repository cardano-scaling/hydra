---
sidebar_label: "Metrics reference"
sidebar_position: 2
---

# Performance metrics reference

This page is a legend for every performance metric reported by Hydra's
benchmarking and monitoring tooling: what each one means, how it is computed,
and where in the code it comes from.

Metrics are gathered from three different vantage points, which is important
when comparing them:

- **Client-observed** (the benchmark client's wall clock): the end-to-end
  results table.
- **Node-internal** (hydra-node tracer timestamps or Prometheus counters): the
  log analyzer and the runtime metrics.
- **Isolated** (criterion, in-process, no network or disk): the
  micro-benchmarks.

The absolute numbers produced by the cloud CI runners are noisy. Treat them as
relative signals (how a value moves as the code changes), not as absolute
hardware figures. GitHub's hosted fleet mixes CPU models with a large
performance spread, so even the relative signal is only trustworthy when both
sides ran on the same machine: the PR comparison workflow measures the PR and
its merge-base interleaved on each runner and aggregates the same-machine pair
deltas (see "PR comparison methodology" in `hydra-cluster/README.md`).

## End-to-end benchmark results

Produced by the cluster benchmark (`bench-e2e`) and published on the
_End-to-end benchmarks_ page in this section. The benchmark client timestamps
each transaction when it submits it and again when it observes the transaction's
`TxValid` and its `SnapshotConfirmed`, so every latency and throughput figure
below is derived from those client-side timestamps (not from inside the node).

Rendered by `Bench.Summary` (`hydra-cluster/bench/Bench/Summary.hs`) from values
computed in `Bench.EndToEnd` (`hydra-cluster/bench/Bench/EndToEnd.hs`). The
PR-versus-master comparison table is produced by `scripts/bench-e2e-diff.py`.

| Metric | Meaning (unit) | How it is computed |
| --- | --- | --- |
| Number of txs | Confirmed transactions in the run | count of transactions that reached a `confirmedAt` (`numberOfTxs`) |
| Avg. Confirmation Time (ms) | Mean submit-to-confirm latency | `sum(confirmedAt - submittedAt) / numberOfTxs` (`averageConfirmationTime`) |
| P50 / P95 / P99 (ms) | Percentiles of submit-to-confirm latency | `makeQuantiles` over every confirmed tx's confirmation time (100 buckets, seconds scaled to ms) |
| Tx validation time p50 (ms) | Median submit-to-`TxValid` latency | median of `validAt - submittedAt` (`medianMilliseconds validationTimes`) |
| End-to-end TPS (tx/s) | Overall confirmed throughput | `numberOfTxs / wallClock`, where `wallClock = max(confirmedAt) - min(submittedAt)` |
| Sustained TPS (tx/s) | Steady-state throughput with the run's ends trimmed | transactions confirmed between the snapshots that first reach 10% and 90% of cumulative confirmed txs, divided by that time span; omitted when fewer than 10 snapshots were observed (`sustainedSnapshotTps`) |
| Backlog drain time (s) | Time from the last submit to the last confirm | `max(confirmedAt) - max(submittedAt)` (`drainSeconds`) |
| Snapshots observed | Distinct confirmed snapshots seen | `Map.size` of the observed snapshots (`numberOfSnapshots`) |
| Snapshots per second (/s) | Snapshot confirmation rate | `numberOfSnapshots / wallClock` (`snapshotsPerSecond`) |
| Avg txs per snapshot | Mean snapshot batch size | `numberOfTxs / numberOfSnapshots` |
| Peak node RSS (MB) | Highest hydra-node memory during the run | peak `VmHWM` across this scenario's hydra-node processes, Linux only (`readPeakNodeRssMb`) |
| Number of Invalid txs | Transactions the node rejected as invalid | count of transactions that reached an `invalidAt` (`numberOfInvalidTxs`) |
| Fanout outputs | UTxO entries fanned out when the head closed | member count of the final `finalizedUTxO`; reported as 0 if fanout did not finalize within the time budget (`numberOfFanoutOutputs`) |
| Incremental commit / decommit: count, avg (ms), max (ms) | On-chain incremental (de)commit finalisation latency | per event, `finalisedAt - startedAt`; the run's count, mean, and maximum |
| Load mode | How transactions were submitted | `open-loop` (fire and forget) or `closed-loop` (`--wait-for-tx-valid`); recorded so consumers pick the right rows to compare |
| Outcome | Only present when the run failed | short failure reason (`runOutcome`); a failed dataset stays in the report next to its siblings instead of clobbering them |
| Alloc MB per confirmed tx | GHC heap allocation per transaction, summed over nodes | delta of `hydra_rts_allocated_bytes_total` across the tx-processing window, divided by confirmed txs (`rtsAggregates`); needs nodes running with `+RTS -T` |
| Alloc MB per snapshot | Same allocation total per confirmed snapshot | as above, divided by snapshots; batching moves it, so read together with _Avg txs per snapshot_ |
| Mutator CPU s per 1k txs | Node CPU time spent outside GC, summed over nodes | delta of `hydra_rts_mutator_cpu_seconds_total`, normalized per 1000 confirmed txs |
| Max live MB (max node) | Peak live heap of the largest node | `hydra_rts_max_live_bytes` at scrape time (peak since node start, not windowed) |

The allocation and CPU counters exist because wall-clock numbers from shared
runners never fully settle: bytes allocated per unit of work is nearly
machine-independent (GHC's own CI gates on it for that reason) and directly
catches the "extra copying or serialization" class of regression. They are
only present when the nodes were started with `+RTS -T`, which the CI
workflow enables through `HYDRA_NODE_RTS_FLAGS` (see the bench `--help`).

The report also carries a machine-readable twin, `end-to-end-benchmarks.json`,
with the raw snapshot series and per-transaction confirmation times.
`scripts/bench-e2e-diff.py` computes derived estimators from those series with
one implementation for both compared sides (each side runs its own bench
binary, so estimators computed inside the binaries could silently diverge in
definition). In particular _Sustained TPS (slope)_, the diffed variant of
Sustained TPS, is the least-squares slope of cumulative confirmed transactions
over time across snapshot points in the middle 80% by cumulative count; unlike
the endpoint-based trim it does not move in whole-snapshot steps and works
from 4 snapshot points up.

Not every row above appears in the PR diff table: open-loop scenarios run
saturated, so their confirmation avg/percentiles mostly restate throughput and
are omitted there (they remain on this site's report pages); P99 is omitted
everywhere because confirmations arrive in per-snapshot bursts and the top
percentile is a handful of atoms. Coloring uses per-metric noise thresholds
from `scripts/bench-e2e-thresholds.json`, calibrated from nightly A/A runs.

A note on the latency statistics: the percentiles are computed over every
confirmed transaction in the run, not per snapshot. Because confirmations arrive
in per-snapshot bursts that share a single client timestamp, per-transaction
quantiles would be quantized by snapshot batch size. That is why _Sustained TPS_
is trimmed on snapshot boundaries instead (see the `sustainedSnapshotTps`
comment for the full rationale).

## Micro-benchmarks

In-process [criterion](https://hackage.haskell.org/package/criterion) timings
with no network or disk, used to attribute cost to one specific operation. Each
figure is a criterion OLS mean; recorded baselines live in
`hydra-cluster/bench/BASELINES.md`.

| Benchmark (source) | Measures | How to run |
| --- | --- | --- |
| `hydra-node:snapshot` (`hydra-node/bench/snapshot/Main.hs`) | Per-snapshot `ReqSn` to `AckSn` work over a UTxO-size by txs-per-snapshot grid: `full-update` (the whole `update` handling a `ReqSn`), `ledger-reapply-only`, `accumulator-only`, `sign-only`, and `update-and-aggregate` | `just bench-snapshot` |
| `hydra-tx:accumulator` (`hydra-tx/bench/accumulator/Main.hs`) | Accumulator operations across UTxO-set sizes: build, TxOut extraction and serialization, membership-proof creation, and commitment / hash | `cabal bench hydra-tx:accumulator` (set `BENCH_MAX_UTXO` to include the largest sizes) |
| `hydra-node:micro` (`hydra-node/bench/micro-bench`) | Cardano ledger apply cost inside a head; published on the _Ledger micro-benchmarks_ page | `cabal bench hydra-node:micro` |
| `hydra-node:tx-cost` (`hydra-node/bench/tx-cost`) | Per-transaction on-chain cost for each protocol transaction: serialized size, memory and CPU execution units, and minimum fee | `cabal bench hydra-node:tx-cost` |

## Node log analysis

`scripts/bench-logs-analyze.py` reads hydra-node JSON logs and reports p50, p95,
and max for the metrics below. These come from the node's own tracer timestamps,
so they are node-internal timings and can be compared against the client-observed
end-to-end numbers to see where a round's time is spent.

| Metric | Meaning |
| --- | --- |
| Per-input processing time (ms) | `EndInput - BeginInput`, bucketed by input kind (`ReqTx`, `ReqSn`, `AckSn`, other network, chain, client) |
| Per-effect dispatch time (ms) | `EndEffect - BeginEffect`, bucketed by effect kind |
| Snapshot round wall time (ms) | from a `ReqSn`'s `BeginInput` until the `LogicOutcome` that carries `SnapshotConfirmed`, per node |

## Runtime metrics (Prometheus)

Exposed by any running hydra-node on its `--monitoring-port` (`/metrics`), and
defined in `hydra-node/src/Hydra/Logging/Monitoring.hs`. Unlike the benchmark
metrics above, these are live counters suitable for production dashboards.

| Series | Type | Meaning |
| --- | --- | --- |
| `hydra_head_inputs` | counter | inputs processed by the node's event loop |
| `hydra_head_requested_tx` | counter | transactions requested (a `ReqTx` was seen) |
| `hydra_head_confirmed_tx` | counter | transactions confirmed; incremented on each `SnapshotConfirmed` by the number of transaction ids it carries |
| `hydra_head_tx_confirmation_time_ms` | histogram | per-transaction request-to-confirmation time; buckets 5, 10, 50, 100, 1000 |
| `hydra_head_snapshot_confirmation_time_ms` | histogram | `SnapshotRequested` to `SnapshotConfirmed` time; buckets 5, 10, 50, 100, 500, 1000, 5000, 10000, 30000 |
| `hydra_head_peers_connected` | gauge | number of currently connected peers |

When the node runs with `+RTS -T`, the endpoint additionally serves GHC RTS
work counters, refreshed at scrape time (without `-T` these series are absent
and the endpoint output is unchanged):

| Series | Type | Meaning |
| --- | --- | --- |
| `hydra_rts_allocated_bytes_total` | gauge (monotone) | total bytes allocated since process start |
| `hydra_rts_mutator_cpu_seconds_total` | gauge (monotone) | CPU time spent running the program (excludes GC) |
| `hydra_rts_gc_cpu_seconds_total` | gauge (monotone) | CPU time spent in garbage collection |
| `hydra_rts_max_live_bytes` | gauge | peak live heap since process start |
| `hydra_rts_major_gcs_total` | gauge (monotone) | major garbage collections since process start |
