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
| Mean live MB under load (max node) | Live heap of the largest node, averaged over the major GCs inside the tx-processing window | delta of `hydra_rts_cumulative_live_bytes` divided by the delta of `hydra_rts_major_gcs`, per node; omitted when no node had a major GC in the window. The since-start peak (`hydra_rts_max_live_bytes`) is not reported: small scenarios set it during node startup, before the head exists |

The work counters exist because wall-clock numbers from shared runners never
fully settle: bytes allocated per unit of work is nearly machine-independent
and directly catches the extra-copying/serialization class of regression.
Reports also carry an `end-to-end-benchmarks.json` twin with raw series;
`scripts/bench-e2e-diff.py` derives percentiles and _Sustained TPS (slope)_
(least-squares over the middle 80% by cumulative count, reported only with
at least 8 snapshot points inside that window) from it with one
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

Enable `/metrics` with `--monitoring-port`. The endpoint binds before node
initialization and its failure terminates the node. Scrape availability (`up`)
and application readiness (`hydra_node_ready`) are separate signals.

Every family has `HELP` and `TYPE` metadata. New counters use `_total`, durations
use seconds, and sizes use bytes. Histograms expose `_bucket`, `_sum` and
`_count`. Duration buckets are 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2.5, 5,
10, 30, 60, 120 and 300 seconds, plus infinity. Quantiles are bucket estimates;
observations above the largest finite bucket cannot yield an accurate quantile.

Counters describe work observed by **one process** and reset on restart. Use
`rate()` or `increase()` over a window. Peers in the same Head observe the same
transactions: summing their confirmation rates does not measure unique Head
throughput. On-chain observations and lifecycle counters include re-observation
after rollback; current-state gauges rewind with state instead.

### Accounting and timing boundaries

`hydra_head_requested_tx_total` counts incoming ReqTx messages at their first
processing attempt. An internal retry retains the input ID and neither counts
again nor overwrites the transaction's first timestamp. An external duplicate
message is another request, while its transaction retains its first timing.
The input attempt and wait counters show the retries separately.

Transaction timers are bounded to 100000 entries and one hour. They are removed
on confirmation, invalidation, dropping and local-state/head resets. Scraping
also expires idle timers. Eviction affects timing coverage, not confirmation
counts. `hydra_monitoring_timing_evictions_total` reports this loss of coverage.

Confirmations match the head, version and snapshot number. Re-adopting the same
number, or advancing its version alone, does not count its transactions again.
Signed confirmations and adopted snapshots are separated by `source`. Adoption
counts the transactions listed in that snapshot, not the contents of skipped
snapshots. Loading persisted state seeds the confirmed watermark and any pending
snapshot contents without increasing process counters. A later confirmation can
count those restored contents, but contributes no invented pre-restart latency.

Transaction confirmation time starts at **local ReqTx processing**, excluding
its first queue wait and the client's network latency. Snapshot confirmation time
starts after validating and accepting `SnapshotRequested`, excluding request
validation. Input queue and processing durations, API response duration, and the
outstanding-round start timestamp expose the adjacent phases. A restored round
has an unknown start (timestamp and age zero); its inflight and missing-signature
gauges still identify outstanding work.

State and queue gauges are sampled every second; outbox gauges use its stall
poll period (normally ten seconds). These samples bypass the logging queue and
the input queue. Timing histograms and counters are updated at their actual
producer, including successful etcd acknowledgement, SQLite transaction completion
and log flush. An L1 submission acceptance is distinct from observing its
transaction on chain. SQLite uses `synchronous=NORMAL`: commit completion is not
a guarantee against power-loss data loss.

### Metric catalogue

Definitions and bounded label domains live in
`hydra-node/src/Hydra/Logging/Monitoring/Metrics.hs`. Route labels normalize the
recover path to `/commits/:id` and map unknown paths to `unknown`. No transaction
IDs, snapshot numbers, raw paths or error messages are labels. L1 work metrics
include the configured backend (`direct`, `blockfrost` or `offline`).

| Family | Type | Meaning |
| --- | --- | --- |
| `hydra_head_inputs_total` | counter | Completed input processing attempts, including retries. |
| `hydra_head_requested_tx_total` | counter | ReqTx network messages first processed by this node; retries excluded and external duplicate messages included. |
| `hydra_head_confirmed_tx_total` | counter | Transactions in newly confirmed or adopted snapshot numbers observed in this process; repeated adoption excluded. |
| `hydra_head_tx_confirmation_seconds` | histogram | Time from first local processing of ReqTx to confirmation; excludes queue admission and unavailable timings. |
| `hydra_head_snapshot_request_processing_seconds` | histogram | BeginInput ReqSn to its accepted SnapshotRequested outcome; excludes queue wait and subsequent effects. |
| `hydra_head_snapshot_confirmation_seconds` | histogram | Time from accepted SnapshotRequested after validation to confirmation of the same head, version and number. |
| `hydra_head_snapshots_total` | counter | New snapshot numbers observed, split by signed confirmation or adoption. |
| `hydra_head_snapshot_transactions` | histogram | Transaction count in each newly confirmed or adopted snapshot. |
| `hydra_monitoring_tracked_transactions` | gauge | Transaction timing entries retained by monitoring, capped at 100000 and expired after one hour. |
| `hydra_monitoring_timing_evictions_total` | counter | Transaction timings discarded due to age or capacity; confirmation counters are unaffected. |
| `hydra_head_peers_connected` | gauge | Distinct connected peers observed by the processing loop. |
| `hydra_chain_drift_seconds` | gauge | Wall clock minus chain time at the latest processed tick; may be negative with clock skew. |
| `hydra_chain_last_block_timestamp_seconds` | gauge | Wall-clock time of the last tick processed by the main loop; zero before the first tick. |
| `hydra_chain_backend_last_block_timestamp_seconds` | gauge | Wall-clock time of the last raw backend roll-forward; independent of main-loop progress. |
| `hydra_head_broadcast_stalled` | gauge | Live outbox stall predicate; independent of queued client notifications. |
| `hydra_head_broadcast_stalled_no_progress` | gauge | Live outbox stall caused by no progress. |
| `hydra_head_broadcast_stalled_backlog_full` | gauge | Live outbox stall caused by the backlog limit. |
| `hydra_head_pending_broadcasts` | gauge | Messages in the outer network hand-off; excludes the persistent network queue. |
| `hydra_head_broadcast_no_progress_seconds` | gauge | Seconds without outer hand-off progress while it has pending messages. |
| `hydra_head_inputs_refused_broadcast_stalled_total` | counter | Client inputs refused because the outbound hand-off is stalled. |
| `hydra_node_input_attempts_total` | counter | Input processing attempts by source, including retries. |
| `hydra_node_input_waits_total` | counter | Wait outcomes by bounded reason, including inputs parked during catch-up. |
| `hydra_node_input_drops_total` | counter | Inputs discarded after exhausting retry eligibility. |
| `hydra_node_input_processing_seconds` | histogram | BeginInput-to-EndInput duration, including inline effects and synchronous trace handling. |
| `hydra_node_input_queue_wait_seconds` | histogram | Time spent waiting for admission and dequeue on each attempt; retry delay and parked time excluded. |
| `hydra_node_input_queue_depth` | gauge | Inputs currently admitted to the bounded processing queue. |
| `hydra_node_parked_inputs` | gauge | Network inputs parked pending chain sync or awaiting readmission after sync. |
| `hydra_node_delayed_input_workers` | gauge | Delayed retry and parked-release workers, including workers blocked on admission. |
| `hydra_node_ready` | gauge | One when the main loop has started, the node is synced, the network is connected and the outbox is not stalled. |
| `hydra_node_synced` | gauge | One when the current node state is in sync. |
| `hydra_head_phase` | gauge | Current head phase as a one-hot state set. |
| `hydra_chain_processed_slot` | gauge | Latest chain slot in current node state; rewinds after rollback. |
| `hydra_network_connected` | gauge | Live network connectivity callback status; zero before a successful connection. |
| `hydra_head_confirmed_snapshot_number` | gauge | Current confirmed snapshot number; zero with no head. |
| `hydra_head_confirmed_snapshot_version` | gauge | Version of the current confirmed snapshot; zero with no head. |
| `hydra_head_snapshot_inflight` | gauge | One while a snapshot request is outstanding or signatures are being collected. |
| `hydra_head_snapshot_started_timestamp_seconds` | gauge | Wall-clock start of the outstanding round observed in this process; zero when absent or restored with unknown start. |
| `hydra_head_snapshot_inflight_age_seconds` | gauge | Age of the outstanding round observed in this process; zero when its start is unknown. |
| `hydra_head_snapshot_missing_signatures` | gauge | Participants whose signatures are still missing from the outstanding snapshot. |
| `hydra_head_outstanding_transactions` | gauge | Current allTxs entries awaiting inclusion in a snapshot. |
| `hydra_head_last_snapshot_timestamp_seconds` | gauge | Wall-clock time of the last newly confirmed or adopted snapshot in this process. |
| `hydra_head_transaction_outcomes_total` | counter | Transaction and command outcomes observed by this node, with bounded reason labels. |
| `hydra_node_commands_total` | counter | Client commands first processed by the main loop, by command type. |
| `hydra_network_persistent_queue_depth` | gauge | Messages awaiting successful etcd broadcast, including the pinned in-flight batch. |
| `hydra_network_persistent_queue_oldest_age_seconds` | gauge | Age of the oldest persisted queued message, using its file timestamp across restart. |
| `hydra_network_last_put_timestamp_seconds` | gauge | Wall-clock time of the last successful etcd put or deduplicated retry. |
| `hydra_network_last_watch_timestamp_seconds` | gauge | Wall-clock time of the last watch response, including empty responses; idle traffic may leave it unchanged. |
| `hydra_network_put_seconds` | histogram | Duration of successful etcd put attempts, including deduplicated retries. |
| `hydra_network_broadcast_messages_total` | counter | Messages in batches acknowledged by etcd, including deduplicated retries once. |
| `hydra_network_broadcast_batch_messages` | histogram | Messages per acknowledged network batch. |
| `hydra_network_events_total` | counter | Network failures, retries and recovery events by bounded category. |
| `hydra_chain_operations_total` | counter | L1 protocol posts, API deposit drafts and user transaction submissions attempted. |
| `hydra_chain_operation_failures_total` | counter | Failed L1 protocol posts, API deposit drafts and user transaction submissions. |
| `hydra_chain_operation_seconds` | histogram | L1 operation construction and backend submission duration; successful return does not imply on-chain inclusion. |
| `hydra_chain_operations_inflight` | gauge | L1 protocol posts, API deposit drafts and user transaction submissions currently executing. |
| `hydra_chain_submissions_total` | counter | Actual backend transaction submission responses, separate from on-chain observations. |
| `hydra_chain_observations_total` | counter | Protocol transactions observed by the node; re-observation after rollback is counted. |
| `hydra_chain_rollbacks_total` | counter | Raw backend rollback notifications. |
| `hydra_chain_rollback_slots` | histogram | Slots from the last backend roll-forward to the rollback point. |
| `hydra_chain_blockfrost_retries_total` | counter | Retryable Blockfrost errors observed by the outer retry loop. |
| `hydra_chain_blockfrost_rate_limits_total` | counter | HTTP 429 responses across node startup, wallet queries, chain polling and transaction submissions. |
| `hydra_head_pending_deposits` | gauge | Current unconsumed deposits tracked by this node, corrected by rollback. |
| `hydra_head_oldest_deposit_timestamp_seconds` | gauge | Creation time of the oldest unconsumed deposit; zero if none. |
| `hydra_head_next_deposit_deadline_timestamp_seconds` | gauge | Earliest deadline of unconsumed deposits, including expired recoverable deposits; zero if none. |
| `hydra_head_decommit_state` | gauge | Current decommit state as a one-hot set. |
| `hydra_head_decommit_started_timestamp_seconds` | gauge | Time this process observed a new decommit; zero if restored or absent. |
| `hydra_head_incremental_events_total` | counter | Deposit and decommit lifecycle observations in this process; re-observation after rollback counts again. |
| `hydra_head_fanout_remaining_outputs` | gauge | Outputs still awaiting distribution in the current partial fanout state. |
| `hydra_head_fanout_mode` | gauge | Current fanout mode as a one-hot set; deferred work is intentionally waiting for selection. |
| `hydra_head_fanout_chunks_total` | counter | Observed partial and final fanout transactions; re-observation after rollback counts again. |
| `hydra_head_fanout_evaluation_failures_total` | counter | Failed fanout candidate evaluations; the builder may recover by selecting a smaller chunk. |
| `hydra_head_fanout_failures_total` | counter | Failed fanout protocol operation attempts. |
| `hydra_head_last_fanout_timestamp_seconds` | gauge | Wall-clock time of the last observed fanout chunk in this process. |
| `hydra_head_contestation_deadline_timestamp_seconds` | gauge | Current contestation deadline; zero for idle or open heads. |
| `hydra_api_http_inflight` | gauge | HTTP requests whose handler and response callback have not finished. |
| `hydra_api_http_responses_total` | counter | Completed HTTP responses by normalized route and status class. |
| `hydra_api_http_seconds` | histogram | HTTP request duration through completion of the WAI response callback, by normalized route. |
| `hydra_api_operation_timeouts_total` | counter | Operations returning HTTP 202 because their response wait expired; distinct from accepted submissions. |
| `hydra_api_websocket_connections` | gauge | WebSocket handlers currently active, including handshake and history replay. |
| `hydra_api_errors_total` | counter | API failures by bounded category. |
| `hydra_persistence_queue_depth` | gauge | SQLite queue items awaiting the writer, including flush markers. |
| `hydra_persistence_write_inflight` | gauge | One while a SQLite event batch is being encoded and committed. |
| `hydra_persistence_written_events_total` | counter | Events whose SQLite transaction has completed; synchronous NORMAL does not promise power-loss durability. |
| `hydra_persistence_stat_failures_total` | counter | Failures collecting database or WAL file sizes; previous size samples remain visible. |
| `hydra_persistence_write_failures_total` | counter | Failed SQLite event batches. |
| `hydra_persistence_write_seconds` | histogram | SQLite batch encoding and transaction completion duration. |
| `hydra_persistence_oldest_event_seconds` | histogram | Enqueue-call-to-commit latency of the oldest event in each SQLite batch, including blocked admission. |
| `hydra_persistence_batch_events` | histogram | Events per completed SQLite batch. |
| `hydra_persistence_rotation_seconds` | histogram | Completed archive and checkpoint rotation duration, including queue flush. |
| `hydra_persistence_replay_seconds` | histogram | Lifetime of SQLite event stream reads, including consumer processing until closure. |
| `hydra_persistence_database_bytes` | gauge | Current SQLite database file size. |
| `hydra_persistence_wal_bytes` | gauge | Current SQLite WAL file size; zero if no WAL file exists. |
| `hydra_logging_queue_depth` | gauge | Log envelopes awaiting the writer. |
| `hydra_logging_written_entries_total` | counter | Log lines in batches whose output flush completed. |
| `hydra_logging_unconfirmed_entries_total` | counter | Entries in batches with a write failure; some lines may have been partially written. |
| `hydra_logging_encoding_failures_total` | counter | Log entries replaced by an unencodable-entry diagnostic. |
| `hydra_logging_write_failures_total` | counter | Log batches with an output write or flush failure. |
| `hydra_logging_write_seconds` | histogram | Log batch encoding, write and flush duration. |
| `hydra_logging_enqueue_seconds` | histogram | Time spent admitting a log envelope to its bounded queue. |
| `hydra_logging_batch_entries` | histogram | Entries per successfully flushed log batch. |

Additional runtime families:

| Family | Type | Availability / meaning |
| --- | --- | --- |
| `hydra_build_info{version}` | gauge | Always 1 for the running build. |
| `hydra_process_cpu_seconds_total` | counter | Process user and system CPU time. |
| `hydra_process_resident_memory_bytes` | gauge | Current operating-system RSS; Linux only. |
| `hydra_process_start_time_seconds` | gauge | Operating-system process start time; Linux only. |
| `hydra_monitoring_start_time_seconds` | gauge | Time monitoring initialized in this process. |
| `hydra_rts_allocated_bytes_total` | counter | Cumulative allocations; requires `+RTS -T`. |
| `hydra_rts_mutator_cpu_seconds_total` | counter | Cumulative mutator CPU time; requires `+RTS -T`. |
| `hydra_rts_gc_cpu_seconds_total` | counter | Cumulative GC CPU time; requires `+RTS -T`. |
| `hydra_rts_max_live_bytes` | gauge | Maximum live heap at a major GC; requires `+RTS -T`. |
| `hydra_rts_cumulative_live_bytes_total` | counter | Sum of major-GC live heap measurements; requires `+RTS -T`. |
| `hydra_rts_major_gcs_total` | counter | Major GC count; requires `+RTS -T`. |

The five cumulative RTS values are counters; maximum live heap remains a gauge.
For mean live heap over a window divide the cumulative-live rate by the major-GC
rate. A window with no major collection has no mean. RSS and maximum live heap
measure different things.

### Compatibility

Existing names remain aliases produced from the same scrape sample; do not add
an alias to its canonical series. Existing benchmark readers keep working.

| Existing family | Canonical family |
| --- | --- |
| `hydra_head_inputs` | `hydra_head_inputs_total` |
| `hydra_head_requested_tx` | `hydra_head_requested_tx_total` |
| `hydra_head_confirmed_tx` | `hydra_head_confirmed_tx_total` |
| `hydra_head_inputs_refused_broadcast_stalled` | `hydra_head_inputs_refused_broadcast_stalled_total` |
| `hydra_head_tx_confirmation_time_ms` | `hydra_head_tx_confirmation_seconds` |
| `hydra_head_snapshot_confirmation_time_ms` | `hydra_head_snapshot_confirmation_seconds` |
| Five cumulative `hydra_rts_*` families without `_total` | Same name with `_total` |

The millisecond aliases scale **both bucket boundaries and sums** by 1000;
counts are identical. Their finite buckets now extend to 300000 ms. The old RTS
names retain their values and now correctly declare counter type. Other existing
gauges retain their names. The corrected counting semantics apply to aliases too.

### Queries and alerts

Per-node throughput:

```promql
rate(hydra_head_confirmed_tx_total[5m])
```

Windowed mean and estimated p95 transaction latency in seconds:

```promql
rate(hydra_head_tx_confirmation_seconds_sum[5m])
  / rate(hydra_head_tx_confirmation_seconds_count[5m])
histogram_quantile(0.95,
  sum by (job, instance, le) (rate(hydra_head_tx_confirmation_seconds_bucket[5m])))
```

Keep no-traffic windows empty instead of presenting them as zero latency. The
demo dashboards use these windowed queries and instant scrapeable-node counts,
plus separate configured/unavailable-target counts. Both provisioning variants
use the explicit datasource UID `prometheus`.

`demo/prometheus/hydra.rules.yml` supplies alerts for absent/down targets, missing
metric instrumentation, readiness, queue pressure, snapshot/fanout progress,
backend silence and logging loss. Both demo Prometheus configurations load it.
Adjust thresholds for the deployment and configure Alertmanager destinations to
receive notifications. A removed individual target cannot be detected from `up`
alone: compare service discovery against your expected target inventory. No
traffic alone is not a stalled Head, and deferred fanout is intentionally waiting
for selection.

### Diagnosing a stalled broadcast

`hydra_head_broadcast_stalled` and its `no_progress` / `backlog_full` cause gauges
sample the live outer hand-off. Client notifications remain debounced and may
arrive later during catch-up; a full input queue cannot stop the live samples.
Cause changes remain visible even when the overall stalled predicate stays true.

The outer `hydra_head_pending_broadcasts` count excludes the persistent queue.
Check `hydra_network_persistent_queue_depth` and its oldest age as well: those
include the first 100 messages waiting for etcd and the pinned in-flight batch,
so an outage is visible before the outer hand-off fills. Oldest age comes from
the queued file timestamp and survives restart; a concurrent removal may produce
one zero-age sample. A successful put includes a deduplicated retry, and watch
response age is diagnostic only: a healthy idle watch need not receive traffic.

`hydra_chain_backend_last_block_timestamp_seconds` measures raw backend progress;
`hydra_chain_last_block_timestamp_seconds` measures ticks processed by the main
loop. Both start at zero. A changing backend timestamp with a frozen processed
timestamp suggests the main loop is blocked, rather than a silent backend.

Logging write errors count batches and **unconfirmed** entries: a partial write
cannot identify exactly which lines reached the reader. Encoding failures count
entries replaced with a diagnostic. These observations use a direct callback to
monitoring, so observing a broken logger does not enqueue another log message.
