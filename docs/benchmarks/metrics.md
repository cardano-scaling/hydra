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
hardware figures. Even relative signals need care: GitHub's runner fleet mixes
CPU models with a large performance spread, so the PR comparison workflow
measures the PR and its merge-base interleaved on each runner and aggregates
same-machine pair deltas (see "PR comparison methodology" in
`hydra-cluster/README.md`). The diff comment omits the open-loop
confirmation-latency rows (they restate throughput) and P99 everywhere
(confirmations arrive in per-snapshot bursts, so the top percentile is a
handful of atoms); a failed run carries an explicit `Outcome` row instead of
silently missing numbers.

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
| Refused submissions | `NewTx` submissions the node refused because its outbound broadcast queue was stalled | count of `RejectedInputBecauseBroadcastStalled` for the scenario's transactions; each refused transaction is resubmitted with backoff, up to 10 times, so one transaction can count several times (`numberOfRefusals`) |
| Fanout outputs | UTxO entries fanned out when the head closed | member count of the final `finalizedUTxO`; reported as 0 if fanout did not finalize within the time budget (`numberOfFanoutOutputs`) |
| Incremental commit / decommit: count, avg (ms), max (ms) | On-chain incremental (de)commit finalisation latency | per event, `finalisedAt - startedAt`; the run's count, mean, and maximum |
| Alloc MB per confirmed tx / per snapshot | GHC heap allocation summed over nodes, per unit of work | delta of `hydra_rts_allocated_bytes` across the tx-processing window (`rtsAggregates`); only when nodes run with `+RTS -T` |
| Mutator CPU s per 1k txs | Node CPU time outside GC, summed over nodes | delta of `hydra_rts_mutator_cpu_seconds`, per 1000 confirmed txs |
| Max live MB (max node) | Peak live heap of the largest node | `hydra_rts_max_live_bytes` (peak since node start, not windowed) |

The work counters exist because wall-clock numbers from shared runners never
fully settle: bytes allocated per unit of work is nearly machine-independent
and directly catches the extra-copying/serialization class of regression.
Reports also carry an `end-to-end-benchmarks.json` twin with raw series;
`scripts/bench-e2e-diff.py` derives percentiles and _Sustained TPS (slope)_
(least-squares over the middle 80% by cumulative count) from it with one
implementation for both compared sides.

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
| `hydra-node:snapshot` (`hydra-node/bench/snapshot/Main.hs`) | Per-snapshot `ReqSn` to `AckSn` work over a UTxO-size by txs-per-snapshot grid: `full-update` (the whole `update` handling a `ReqSn`), `ledger-apply-only`, `accumulator-only`, `sign-only`, and `update-and-aggregate` | `just bench-snapshot` |
| `hydra-tx:accumulator` (`hydra-tx/bench/accumulator/Main.hs`) | Accumulator operations across UTxO-set sizes: build, TxOut extraction and serialization, membership-proof creation, and commitment / hash | `cabal bench hydra-tx:accumulator-bench` (set `BENCH_MAX_UTXO` to include the largest sizes) |
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
| `hydra_chain_drift_seconds` | gauge | how far behind the chain the node is, updated on each observed block |
| `hydra_chain_last_block_timestamp_seconds` | gauge | wall-clock time the node last observed a block; alert on `time() - hydra_chain_last_block_timestamp_seconds` to catch a stalled backend, which freezes the drift gauge rather than growing it |
| `hydra_head_broadcast_stalled` | gauge | 1 while the node cannot hand its outbound messages to the hydra network, 0 otherwise; the same condition clients are told about via `NetworkBroadcastStalled` |
| `hydra_head_pending_broadcasts` | gauge | outbound messages accepted from the head logic but not yet handed to the network |
| `hydra_head_broadcast_no_progress_seconds` | gauge | how long the outbound hand-off has completed nothing, 0 while it holds nothing |
| `hydra_head_inputs_refused_broadcast_stalled` | counter | `NewTx` and `Decommit` submissions refused because the hand-off was stalled |

### Diagnosing a stalled broadcast

The last four series are the ones to look at when clients are being refused
with `RejectedInputBecauseBroadcastStalled` (HTTP 503), or when
`<persistence-dir>/pending-broadcast/` is growing. The node cannot deliver
off-chain messages while it is short of an `etcd` quorum, and refuses the
client inputs that would grow the backlog rather than let it grow without
bound; closing, contesting and fanning out are never refused.

`hydra_head_broadcast_stalled` is what to alert on. The other three say how
bad it is and whether it is moving:

- `pending_broadcasts` flat and `no_progress_seconds` climbing means nothing
  is getting out at all, so look for peers whose `etcd` member is down
  (`hydra_head_peers_connected` should agree).
- `pending_broadcasts` climbing while `no_progress_seconds` stays small means
  the network is delivering but more slowly than this node is producing.
- `no_progress_seconds` peaking a little under ten seconds without ever
  tripping the stall is the healthy-but-slow case, and the signal to watch if
  refusals start appearing.

A deep backlog is not by itself a stall: the node refuses inputs for a backlog
(`BacklogFull`) only once it would take more than ten seconds to drain at the
rate it has recently been draining, or once it reaches 10000 messages.

The two gauges are sampled roughly every ten seconds, and only while there is
a backlog, so a burst shorter than that may not appear.

When the node runs with `+RTS -T`, the endpoint additionally serves GHC RTS
work counters, refreshed at scrape time (absent otherwise, so the output is
unchanged without `-T`): `hydra_rts_allocated_bytes`,
`hydra_rts_mutator_cpu_seconds`, `hydra_rts_gc_cpu_seconds`,
`hydra_rts_max_live_bytes` and `hydra_rts_major_gcs`.
