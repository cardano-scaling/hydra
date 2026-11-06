# Head logic performance

The current design runs input rules as Programs inside crem and applies retained
events through composed state owners. The [review guide](../head-logic-review.md)
links to that code. These measurements cover the complete design and its
[capacity policy](capacity.md). Different baselines answer different questions;
their percentages must not be added together.

## What does this cost in TPS?

**The current design has a measurable cost in the small-Head cluster workload:
3.54–7.82% lower TPS across two balanced pairs.** Allocation per confirmed
transaction rises by 12.25–14.58%. This exceeds the provisional 5% investigation
threshold in one timing pair and both allocation pairs. The range describes
these runs, not a confidence interval or a bound for deployments.

With the Head growing to 3,000 outputs, TPS is **0.47–2.86% lower** and allocation
per transaction increases by **0.19–0.67%**. The effect depends on the workload;
the large-Head result does not cancel the small-Head regression.

The short burst is more variable. Three of four paired runs are **7.97–15.15%
slower**; one is **2.57% faster**. Allocations increase in all four pairs. These
results call for profiling before merging the design; they do not support a
general claim that its TPS cost is below 5%.

The complete local-round comparison measures **−0.98% to +2.76% processing time**
against the frozen free-program handlers and original reducer. Allocations
increase by **177–182 KiB per small round**, and about **1.8 MiB with 10,000
background outputs**. That comparison includes capacity checks absent from the
original code; it does not measure introducing Program itself. The
[current cluster comparison](#current-cluster-comparison)
uses the capacity-fixed PR #2927 as its baseline and measures the complete change.

Production runs compiled Haskell with tracing disabled. Development tracing and
exploration do not run in the node; production recovery applies retained events.

## Composed state owners

Measured on 2026-11-06 with GHC 9.6.7, `-O1`, Criterion 1.6.5.0, crem 0.1.1.0,
Intel Core Ultra 7 258V, CPU 0 affinity and one RTS capability. Builds and tests
were stopped. The benchmark source is `bdc5bcb0d`; its production code is
`968ea82f7`. The [measurement data](measurements/head-logic-owners-v1.json) records
commands, binary hash, estimates, confidence errors, allocation regressions,
outlier variance and sample counts.

The `complete` group compares frozen pre-Crem input handlers **and the original
reducer** with current input execution **and aggregation**. Its `original` path
calls `LegacyHandlers.update`, whose ReqTx, ReqSn and AckSn already interpret the
frozen free programs. Both sides therefore contain Program. This comparison
includes the ownership refactor and new capacity checks, but neither fixture has
a decommit or deposit.
Complete ordered outcomes and states must agree, and the round must confirm,
before timing starts.

Each fixture has one incoming transaction, zero backlog and 100 or 10,000
background outputs. Four two-second runs use 1,000 resamples in
frozen/crem/crem/frozen order. Two ten-second repeats use 10,000 resamples,
one in each order. Times below are medians of the six means; paired ranges are
observed differences, not confidence intervals.

| Ledger / background UTxO | Frozen → current, median ms | Paired time change, six runs | Added allocation per round |
| --- | ---: | ---: | ---: |
| Simple / 100 | 2.938 → 2.955 | +0.01% to +1.78% | 176.6 KiB (+28.6%) |
| Cardano / 100 | 3.242 → 3.277 | +0.23% to +2.76% | 182.0 KiB (+15.6%) |
| Simple / 10,000 | 193.989 → 193.115 | −0.98% to +0.74% | 1.76 MiB (+4.2%) |
| Cardano / 10,000 | 198.217 → 198.273 | −0.38% to +0.96% | 1.77 MiB (+3.1%) |

The large fixtures have four measured samples per implementation in the short
runs and nine in the longer runs. The small Cardano repeats give
+0.23% and +1.42%; large Cardano gives −0.10% and +0.12%. No paired mean exceeds
the provisional 5% time threshold in this grid. That is not a general bound:
allocation increases exceed 5% in both small fixtures, and queueing and garbage
collection under load need the whole-node comparison.

### What a local round includes

[`runRound`](../../hydra-node/bench/HeadLogicProgram.hs) executes one node's
`ReqTx`, `ReqSn` and three `AckSn` handlers, including ledger application, snapshot
construction, signing, verification and state aggregation. It synthesises the
other parties' signatures. The backlog is already applied locally before timing;
only one new `ReqTx` is processed. Neither `1 / round time` nor
`transactions in the snapshot / round time` is an end-to-end TPS result.

This benchmark excludes the other nodes, network transport, API work,
message/event serialisation, persistence and queueing under load. It forces the
state projection used by the checksum, not every field in the complete state.

### Retained-event application

The `event-replay-crem` group applies the already-computed events from one round.
It excludes CBOR decoding, disk I/O, ledger validation and cryptography. Four
two-second runs use the same balanced order and 1,000 resamples.

| Ledger / background UTxO | Original → current, median µs | Median added µs per round |
| --- | ---: | ---: |
| Simple / 100 | 1.84 → 6.11 | 4.26 |
| Cardano / 100 | 2.66 → 6.82 | 4.17 |
| Simple / 10,000 | 1.93 → 6.20 | 4.27 |
| Cardano / 10,000 | 2.91 → 7.36 | 4.48 |

Every fixture adds **69.4 KiB per round**. Application is 2.5–3.3 times as slow as
the small original reducer, though the absolute increase is a few microseconds.
This cost is already included in the complete-round results. The node currently
reconstructs the composed runtime per retained event. Keeping a runtime between
events could reduce that work, but needs its own recovery and checkpoint tests.

### Reproduce the local comparisons

Build first, then stop builds and tests while measuring. Use the same binary and
CPU affinity for both sides:

```sh
cabal build hydra-node:head-logic-program --builddir=dist-newstyle-lint
owners_bench=$(cabal list-bin hydra-node:head-logic-program --builddir=dist-newstyle-lint)
run=0
for order in legacy-first program-first program-first legacy-first; do
  run=$((run + 1))
  HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER="$order" \
    taskset -c 0 "$owners_bench" \
    --match pattern /complete/ --time-limit 2 --resamples 1000 \
    --regress allocated:iters --json "/tmp/owners-$run.json" +RTS -T -N1
done
# Repeat both orders with --time-limit 10 --resamples 10000 and fresh output paths.
# Repeat the four-run sequence with /event-replay-crem/ for retained events alone.
```

## Current cluster comparison

This compares all Crem/Program input workflows and composed state owners with
PR #2927 revision `f0a169f48a9aa09ec189af1f75c34a1a8323e62c`. Both sides enforce
the repaired capacity policy. The measured node binaries use the repository's
Nix build; the same GHC 9.6.7 `-O1` cluster runner drives both.

Each workload uses two pairs in baseline/current/current/baseline order, three
nodes, seed 42, CPU 0 affinity and `-N1 -T` for every node. No builds or tests
run alongside them. Dataset hashes must match across all four runs. The workload
must confirm every submitted transaction, report no invalid or refused
transactions, and complete fanout before its throughput is accepted.
The variable burst case is repeated with two further pairs in the same order.
All 16 runs pass: **60,000 transactions confirmed**, zero invalid or refused
submissions, and all expected outputs fanned out.

The [recorded results](measurements/head-logic-owners-e2e-v1.json) retain every
run, including the repeats, commands, source and binary hashes, dataset hashes,
timestamps, confirmation times, snapshot batches and per-node RTS counters.

- **Small:** 1,500 transactions per client, three outputs in the Head, each
  client waiting for its previous confirmation.
- **Large:** 500 transactions per client; the first 111 transactions split each
  client's funds from one to 1,000 outputs, then hold that size. The measured
  interval includes this growth to 3,000 outputs. Clients wait for confirmation.
- **Burst:** the same small dataset, submitted without waiting for confirmation.

TPS covers first submission through final signed confirmation, with real Cardano
transactions, transport, API serialization and asynchronous persistence. It
excludes Head setup and fanout, which are checked separately. The closed-loop
cases have at most three outstanding transactions. The burst is a finite load,
not a measurement of maximum sustained throughput. Neither mode waits for a
durable disk flush before counting confirmation.

| Workload / order | Baseline TPS | Current TPS | TPS change | Baseline p50 / p95 ms | Current p50 / p95 ms |
| --- | ---: | ---: | ---: | ---: | ---: |
| Small / baseline first | 163.39 | 150.61 | −7.82% | 17.76 / 22.58 | 19.07 / 24.98 |
| Small / current first | 156.06 | 150.54 | −3.54% | 18.45 / 24.35 | 19.21 / 25.11 |
| Large / baseline first | 7.89 | 7.67 | −2.86% | 408.35 / 440.74 | 419.38 / 464.81 |
| Large / current first | 7.73 | 7.69 | −0.47% | 417.11 / 454.86 | 418.73 / 468.38 |

The small runs allocate 3.586–3.625 MiB per confirmed transaction in the baseline
and 4.069–4.108 MiB in the current node. These counters sum the three nodes.
Mutator CPU rises by 3.81–8.25%, while GC adds 0.020–0.028 CPU seconds per 1,000
transactions. Most of the extra measured CPU time is outside GC. Mean live heap
at major collections stays near 27.8 MiB for both versions; short runs cannot
establish long-term retention behavior.

Large-Head allocation is 130.85–130.91 MiB per transaction in the baseline and
131.16–131.73 MiB in the current node. Mutator CPU increases by 0.53–2.91%.
GC time changes direction between pairs, and mean live heap stays near 29.1 MiB.
These results cover payment chains, not pending decommit readiness or other
incremental actions during load.

This investigation establishes an allocation cost and a workload-dependent time
cost. It does not assign them to a particular function. The local benchmark
uses 100 or more outputs and synthesises the other parties' signatures; it is
not a substitute for this three-output cluster workload. Scheduling also changes
batch sizes slightly, even with identical transaction datasets.

### Burst repeats

| Run / order | Baseline TPS | Current TPS | TPS change | Allocation change per confirmed tx |
| --- | ---: | ---: | ---: | ---: |
| Initial / baseline first | 884.64 | 907.34 | +2.57% | +0.83% |
| Initial / current first | 939.65 | 833.09 | −11.34% | +20.44% |
| Repeat / baseline first | 912.41 | 839.66 | −7.97% | +12.19% |
| Repeat / current first | 920.71 | 781.25 | −15.15% | +22.98% |

The burst is especially sensitive to batching. In the first reverse-order pair,
the baseline confirms batches of `[1, 4000, 499]` transactions; the current node
confirms `[1, 747, 3752]`. This changes snapshot validation and pruning work.
Each run confirms all 4,500 transactions in only three or four snapshots, with
a transaction window of roughly five seconds. Repeating the workload tests that
variation; it does not turn a short burst into a sustained-capacity measurement.

The repeated slowdowns exceed the 5% investigation threshold. The recorded work
differs as well as its elapsed time: allocation rises by 0.83–22.98%, and mutator
CPU changes by −2.65% to +18.46%. This is a cost of the complete execution under
this load, including its effect on batching. A fixed schedule or an allocation
profile is needed to separate instruction overhead from the changed work. The
timing variation is not grounds for discarding the slower runs.

### Reproduce the cluster comparison

Build the two nodes and the runner before measuring. The baseline archive leaves
the working branch unchanged:

```sh
git fetch https://github.com/cardano-scaling/hydra.git f0a169f48a9aa09ec189af1f75c34a1a8323e62c
capacity_baseline=$(mktemp -d)
git archive f0a169f48a9aa09ec189af1f75c34a1a8323e62c | tar -x -C "$capacity_baseline"
nix build "path:$capacity_baseline#hydra-node" --out-link /tmp/capacity-baseline-node
nix build .#hydra-node --out-link /tmp/owners-node
cabal build hydra-cluster:bench-e2e --builddir=dist-newstyle-lint
owners_runner=$(cabal list-bin hydra-cluster:bench-e2e --builddir=dist-newstyle-lint)

taskset -c 0 python3 scripts/head-logic-e2e.py \
  --baseline /tmp/capacity-baseline-node/bin/hydra-node \
  --program /tmp/owners-node/bin/hydra-node \
  --bench "$owners_runner" --output /tmp/owners-small \
  --transactions 1500 --shapes Constant --wait-for-confirmation
# Large: --transactions 500 --shapes 'Plateau 1000' and a fresh output directory.
# Burst: the small command without --wait-for-confirmation, again with a fresh directory.
# Repeat the burst command in another fresh directory for the second set of pairs.
```
