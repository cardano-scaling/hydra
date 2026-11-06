# Head logic performance

The current design runs input rules as Programs inside crem and applies retained
events through composed state owners. The [review guide](../../../../head-logic-review.md)
links to that code. Measurements below cover the complete design, including the
decommit fixes and [capacity policy](capacity.md), followed by the earlier
experiments. Different baselines answer different questions; their percentages
must not be added together.

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

Production runs compiled Haskell with tracing disabled. The explorer, debugger,
Mermaid renderer and experimental resolved replay do not run in the node.

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
frozen free programs. Both sides therefore contain Program. Earlier `input`
measurements held aggregation constant. This comparison includes the ownership
refactor and new capacity checks, but neither fixture has a decommit or deposit.
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

[`runRound`](../../../../../hydra-node/bench/HeadLogicProgram.hs) executes one node's
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

## The resolved-replay experiment is a separate cost

The production interpreter emits the existing `StateChanged` events. It does
not construct the experimental UTxO deltas. The roughly **396 µs** delta cost
at 30,000 outputs, slower experimental recovery and larger v2 records therefore
do not reduce production TPS in this spike. Those results argue against adopting
that representation; they are not an additional charge for interpreting the
head logic.

## Earlier measurements

The sections below record earlier stages of the spike. Their baselines and
implementations differ from the current results above. Keep them as the record
of each experiment, not as measurements of the final design.

## Original ReqTx measurements

Measured on 2026-11-03 with GHC 9.6.7, the repository's default Nix build,
Criterion 1.6.5.0, and an Intel Core Ultra 7 258V. The process was pinned to CPU 0
with one RTS capability. Builds and tests were stopped during the run.
The baseline source revision is `7941ca4f8`; the program is this branch's spike.
Both implementations are measured in the same binary using identical warmed
fixtures. Every fixture first checks complete outcome and replay agreement.

The 40 comparisons cover two ledgers, two UTxO sizes, two backlog sizes, and five
paths. UTxO sizes are base sizes; collision fixtures add one output, making their
local UTxOs 101 or 10,001 entries. The largest mean increase in this initial grid is **1.07%**; the largest reduction is
**7.65%**. Allocation estimates decrease in every comparison. These results cover only these `ReqTx` cases; they do not measure node throughput. Longer follow-ups
below reach a **4.11%** mean increase in one case; the intervals do not certify
a strict 5% bound.

The timed result forces structural state projections and outgoing snapshot ID
bytes after handler execution and replay. It does not deeply force or serialize
the complete state. Pending deposits, networking, persistence, cold inputs, and
other handlers are outside the grid. The frozen handler is in a separate test
module, which can affect inlining relative to its original location.

Reproduce on Linux with `taskset` available:

```sh
nix build .#head-logic-program --out-link /tmp/head-logic-program
taskset -c 0 /tmp/head-logic-program/bin/head-logic-program \
  --time-limit 0.5 --resamples 1000 \
  --json /tmp/head-logic-program.json --regress allocated:iters +RTS -T -N1
```

Times below are Criterion means in microseconds, followed by their 95% confidence
intervals. Allocation is the fitted `allocated:iters` slope, rounded to bytes per
iteration. Delta is `(program / legacy - 1) * 100` for the mean time. Negative
values favor the program; small timing differences can reflect measurement noise.

| Ledger / UTxO / backlog / path | Legacy µs (95% CI) | Program µs (95% CI) | Time delta | Legacy B/op | Program B/op |
| --- | ---: | ---: | ---: | ---: | ---: |
| simple / 100 / 0 / leader | 1.07151 (1.06857–1.07435) | 1.01663 (1.01328–1.02084) | -5.12% | 10080 | 9752 |
| simple / 100 / 0 / follower | 0.795101 (0.793241–0.797339) | 0.791116 (0.7845–0.801251) | -0.50% | 4560 | 4440 |
| simple / 100 / 0 / snapshot-in-flight | 0.729567 (0.727822–0.732015) | 0.70837 (0.70646–0.71256) | -2.91% | 4240 | 4152 |
| simple / 100 / 0 / collision-wait | 0.142525 (0.142148–0.143041) | 0.134643 (0.134293–0.135066) | -5.53% | 1088 | 808 |
| simple / 100 / 0 / collision-timeout | 0.205336 (0.204952–0.205842) | 0.189636 (0.18913–0.190175) | -7.65% | 1384 | 1128 |
| cardano / 100 / 0 / leader | 106.563 (106.34–106.861) | 106.262 (106.075–106.521) | -0.28% | 219587 | 219259 |
| cardano / 100 / 0 / follower | 105.878 (105.687–106.143) | 105.73 (105.418–106.075) | -0.14% | 218211 | 218091 |
| cardano / 100 / 0 / snapshot-in-flight | 105.735 (105.547–106.039) | 104.797 (104.573–105.061) | -0.89% | 217891 | 217803 |
| cardano / 100 / 0 / collision-wait | 0.608123 (0.606969–0.609955) | 0.594385 (0.592549–0.597003) | -2.26% | 4952 | 4672 |
| cardano / 100 / 0 / collision-timeout | 0.713691 (0.711997–0.715893) | 0.697785 (0.696623–0.699139) | -2.23% | 5704 | 5448 |
| simple / 100 / 1000 / leader | 229.124 (227.817–230.037) | 226.73 (226.293–227.177) | -1.05% | 5321328 | 5320999 |
| simple / 100 / 1000 / follower | 0.914095 (0.911256–0.917268) | 0.904071 (0.901938–0.906464) | -1.10% | 5224 | 5104 |
| simple / 100 / 1000 / snapshot-in-flight | 0.841506 (0.839164–0.844902) | 0.811292 (0.809557–0.813011) | -3.59% | 4904 | 4816 |
| simple / 100 / 1000 / collision-wait | 0.288626 (0.287691–0.289946) | 0.275463 (0.274419–0.277846) | -4.56% | 1616 | 1336 |
| simple / 100 / 1000 / collision-timeout | 0.421278 (0.420189–0.423017) | 0.403087 (0.40185–0.404821) | -4.32% | 2344 | 2088 |
| cardano / 100 / 1000 / leader | 905.44 (903.479–907.996) | 908.867 (905.798–912.434) | +0.38% | 4867443 | 4867113 |
| cardano / 100 / 1000 / follower | 104.138 (103.886–104.459) | 104.364 (104.121–104.663) | +0.22% | 214267 | 214147 |
| cardano / 100 / 1000 / snapshot-in-flight | 104.796 (104.504–105.136) | 104.202 (103.951–104.424) | -0.57% | 213947 | 213859 |
| cardano / 100 / 1000 / collision-wait | 0.744782 (0.74271–0.746932) | 0.733947 (0.731626–0.737622) | -1.45% | 5432 | 5152 |
| cardano / 100 / 1000 / collision-timeout | 1.00775 (1.00082–1.01857) | 0.974555 (0.970318–0.980016) | -3.29% | 6664 | 6408 |
| simple / 10000 / 0 / leader | 1.37058 (1.36828–1.37298) | 1.33293 (1.33077–1.33504) | -2.75% | 11280 | 10952 |
| simple / 10000 / 0 / follower | 1.081 (1.07882–1.08393) | 1.07503 (1.07173–1.0789) | -0.55% | 5760 | 5640 |
| simple / 10000 / 0 / snapshot-in-flight | 1.01793 (1.01595–1.02123) | 0.98481 (0.982608–0.988589) | -3.25% | 5440 | 5352 |
| simple / 10000 / 0 / collision-wait | 0.203903 (0.203165–0.204656) | 0.199426 (0.198612–0.200205) | -2.20% | 1328 | 1048 |
| simple / 10000 / 0 / collision-timeout | 0.270974 (0.270156–0.272266) | 0.253723 (0.252501–0.254923) | -6.37% | 1624 | 1368 |
| cardano / 10000 / 0 / leader | 1171.69 (1120.18–1370.91) | 1145.58 (1114.68–1261.52) | -2.23% | 4935468 | 4935141 |
| cardano / 10000 / 0 / follower | 1142.12 (1112.4–1281.11) | 1148.6 (1118.25–1262.41) | +0.57% | 4934093 | 4933973 |
| cardano / 10000 / 0 / snapshot-in-flight | 1149.02 (1117.23–1304.82) | 1161.35 (1128.54–1284.35) | +1.07% | 4933773 | 4933684 |
| cardano / 10000 / 0 / collision-wait | 0.71682 (0.715145–0.718566) | 0.703821 (0.702356–0.705097) | -1.81% | 5384 | 5104 |
| cardano / 10000 / 0 / collision-timeout | 0.830246 (0.828707–0.83231) | 0.808486 (0.80723–0.810443) | -2.62% | 6136 | 5880 |
| simple / 10000 / 1000 / leader | 299.366 (298.494–300.16) | 298.149 (297.343–299.339) | -0.41% | 5562531 | 5562204 |
| simple / 10000 / 1000 / follower | 1.21522 (1.21258–1.219) | 1.21324 (1.21016–1.21688) | -0.16% | 6424 | 6304 |
| simple / 10000 / 1000 / snapshot-in-flight | 1.13433 (1.13228–1.13671) | 1.12057 (1.11718–1.12701) | -1.21% | 6104 | 6016 |
| simple / 10000 / 1000 / collision-wait | 0.342453 (0.341238–0.344277) | 0.331693 (0.330884–0.332686) | -3.14% | 1856 | 1576 |
| simple / 10000 / 1000 / collision-timeout | 0.48447 (0.483131–0.48585) | 0.466851 (0.465791–0.468789) | -3.64% | 2584 | 2328 |
| cardano / 10000 / 1000 / leader | 2216.8 (2207.53–2227.03) | 2225.23 (2209.91–2246.8) | +0.38% | 9895962 | 9895660 |
| cardano / 10000 / 1000 / follower | 1152.46 (1117.26–1231.8) | 1158.8 (1119.94–1251.01) | +0.55% | 4908484 | 4908370 |
| cardano / 10000 / 1000 / snapshot-in-flight | 1170.94 (1134.74–1246.05) | 1162.03 (1125.92–1253.46) | -0.76% | 4908170 | 4908082 |
| cardano / 10000 / 1000 / collision-wait | 0.879703 (0.87717–0.883843) | 0.863653 (0.861701–0.866193) | -1.82% | 6056 | 5776 |
| cardano / 10000 / 1000 / collision-timeout | 1.12008 (1.11756–1.12352) | 1.10334 (1.10063–1.1063) | -1.49% | 7384 | 7128 |

See the [refactor sketch](original-sketch.md) for interpretation semantics,
coverage, and the remaining migration work.

## Longer measurements of the large Cardano cases

The half-second grid has wide confidence intervals in the large Cardano cases,
so those six comparisons were repeated with `--time-limit 5 --resamples 10000`
on the same CPU, again with builds and tests stopped. Mean changes range from
-2.79% to +1.42%; allocation estimates remain lower. This improves precision,
but the individual intervals still do not establish a strict 5% upper bound for
every workload.

To select these cases, append `--match prefix` and these six prefixes to the
benchmark options before `+RTS`:

```text
cardano/utxo-10000/backlog-0/leader
cardano/utxo-10000/backlog-0/follower
cardano/utxo-10000/backlog-0/snapshot-in-flight
cardano/utxo-10000/backlog-1000/leader
cardano/utxo-10000/backlog-1000/follower
cardano/utxo-10000/backlog-1000/snapshot-in-flight
```

| Ledger / UTxO / backlog / path | Legacy µs (95% CI) | Program µs (95% CI) | Time delta | Legacy B/op | Program B/op |
| --- | ---: | ---: | ---: | ---: | ---: |
| cardano / 10000 / 0 / leader | 1118.4 (1099.43–1158.25) | 1123.69 (1104.19–1165.49) | +0.47% | 4935470 | 4935142 |
| cardano / 10000 / 0 / follower | 1185.6 (1158.33–1228.09) | 1152.48 (1133.38–1194.93) | -2.79% | 4934093 | 4933974 |
| cardano / 10000 / 0 / snapshot-in-flight | 1160.55 (1138–1202.06) | 1177.01 (1156.54–1219.61) | +1.42% | 4933774 | 4933685 |
| cardano / 10000 / 1000 / leader | 2255.15 (2240.07–2282.32) | 2250.34 (2235.46–2279.94) | -0.21% | 9895988 | 9895658 |
| cardano / 10000 / 1000 / follower | 1178.01 (1158.78–1210.2) | 1165.69 (1148.16–1196.97) | -1.05% | 4908491 | 4908372 |
| cardano / 10000 / 1000 / snapshot-in-flight | 1166.54 (1148.78–1195.66) | 1174.87 (1154.88–1206.55) | +0.71% | 4908172 | 4908084 |

## Final long samples

The backlog-zero leader and in-flight cases retained the widest uncertainty, so
they were repeated with `--time-limit 30 --resamples 10000`, selecting just their
two prefixes. Their mean changes are +4.11% and -0.62%. These results are included below. The **largest observed mean
increase across these runs is 4.11%**, while
allocation estimates remain lower. Confidence intervals and variation between
runs prevent treating this as a 5% upper bound. Assessing the cost in production needs representative node
workloads on controlled hardware.

| Ledger / UTxO / backlog / path | Legacy µs (95% CI) | Program µs (95% CI) | Time delta | Legacy B/op | Program B/op |
| --- | ---: | ---: | ---: | ---: | ---: |
| cardano / 10000 / 0 / leader | 1171.8 (1159.06–1192.54) | 1219.98 (1208.09–1243.46) | +4.11% | 4935469 | 4935141 |
| cardano / 10000 / 0 / snapshot-in-flight | 1162.02 (1150.77–1185.07) | 1154.77 (1142.94–1177.53) | -0.62% | 4933773 | 4933685 |

## Complete snapshot rounds

The snapshot extension is measured separately on the same CPU model and toolchain,
using the final Nix components, CPU 1 and one RTS capability. Builds and tests are
stopped during measurement. These are in-process rounds for one node, not
end-to-end cluster measurements. Each round executes ReqTx, ReqSn and three AckSn
handlers with state aggregation, local signing and final multisignature verification.
Both implementations sign the other two parties' replies within timing. Complete
outcomes and resulting states are compared before timing; the timed checksum forces
confirmation and structural state projections. Tracing is disabled.

Runs A and B use `--time-limit 0.5 --resamples 1000`, with legacy first and program
first respectively. Run C repeats the small Simple backlog and both large Cardano
cases with `--time-limit 5 --resamples 10000`, legacy first. The longest operation
exceeds the nominal sampling window; its small sample count limits the evidence.
Short Criterion regressions sometimes have unavailable allocation interval bounds.
The [versioned measurements](measurements/head-logic-measurements-v1.json) retain those nulls
and the individual mean and allocation estimates.

| Ledger / UTxO / backlog | Run | Legacy ms (95% CI) | Program ms (95% CI) | Mean delta |
| --- | --- | ---: | ---: | ---: |
| simple / 100 / 0 | A | 3.1111 (3.1052–3.1169) | 3.1267 (3.121–3.1311) | +0.50% |
| cardano / 100 / 0 | A | 3.4508 (3.4477–3.4552) | 3.4877 (3.4653–3.5333) | +1.07% |
| simple / 100 / 1000 | A | 4.026 (4.0184–4.0347) | 4.1873 (4.1788–4.1942) | +4.01% |
| cardano / 100 / 1000 | A | 135.96 (135.78–136.22) | 136.59 (136.51–136.72) | +0.46% |
| simple / 10000 / 0 | A | 207.21 (206.86–207.54) | 208.25 (207.57–208.68) | +0.50% |
| cardano / 10000 / 0 | A | 216.44 (215.72–217.7) | 218.81 (217.57–221.17) | +1.10% |
| simple / 10000 / 1000 | A | 208.54 (208.04–209) | 205.92 (205.5–206.56) | -1.26% |
| cardano / 10000 / 1000 | A | 4591.4 (4533.4–4626.1) | 4425 (4413.3–4442.5) | -3.62% |
| simple / 100 / 0 | B | 2.98 (2.9714–2.9901) | 2.975 (2.9678–2.9832) | -0.17% |
| cardano / 100 / 0 | B | 3.312 (3.2948–3.3301) | 3.2978 (3.2897–3.3069) | -0.43% |
| simple / 100 / 1000 | B | 3.9972 (3.9877–4.0023) | 4.0256 (3.9915–4.0752) | +0.71% |
| cardano / 100 / 1000 | B | 131.36 (128.43–137) | 132.21 (129.93–133.38) | +0.65% |
| simple / 10000 / 0 | B | 194.22 (194.03–194.39) | 194.85 (194.37–195.1) | +0.33% |
| cardano / 10000 / 0 | B | 199.18 (198.75–199.74) | 201.71 (201.55–201.89) | +1.27% |
| simple / 10000 / 1000 | B | 194.26 (193.85–194.58) | 194.72 (194.53–194.83) | +0.23% |
| cardano / 10000 / 1000 | B | 4464.5 (4448.7–4477.1) | 4451.4 (4442.3–4464.5) | -0.30% |
| simple / 100 / 1000 | C | 3.8099 (3.8045–3.8253) | 3.9236 (3.9201–3.9276) | +2.98% |
| cardano / 10000 / 0 | C | 199.35 (198.95–200.18) | 201.28 (200.81–202.03) | +0.97% |
| cardano / 10000 / 1000 | C | 4424.9 (4410.9–4435.3) | 4456.7 (4429.8–4477.4) | +0.72% |

Across these runs the largest mean increase is **4.01%**. Timing variation
between orders and limited samples prevent certifying a hard 5% bound. The 5%
investigation threshold is exceeded by allocations in two workloads: the Simple
100-output/1,000-backlog case grows about 32%, and Cardano 10,000-output/no-backlog
grows about 9.5%. These increases recur in the longer samples. Allocation profiling
of pruning and the ledger/crypto primitive boundary remains follow-up work; this
spike does not establish blanket performance compliance.

Allocation slopes from run A, rounded to bytes per round:

| Ledger / UTxO / backlog | Legacy B/round | Program B/round | Delta |
| --- | ---: | ---: | ---: |
| simple / 100 / 0 | 628544 | 630217 | +0.27% |
| cardano / 100 / 0 | 1188754 | 1243029 | +4.57% |
| simple / 100 / 1000 | 3963245 | 5228891 | +31.93% |
| cardano / 100 / 1000 | 314608134 | 315930782 | +0.42% |
| simple / 10000 / 0 | 43830095 | 43832255 | +0.00% |
| cardano / 10000 / 0 | 59955614 | 65672366 | +9.53% |
| simple / 10000 / 1000 | 48603641 | 49870938 | +2.61% |
| cardano / 10000 / 1000 | 15497334578 | 15504404274 | +0.05% |

Reproduce the balanced runs and longer repeats:

```sh
nix build .#head-logic-program --out-link /tmp/head-logic-program
HEADLOGIC_BENCH_ORDER=legacy-first taskset -c 1 /tmp/head-logic-program/bin/head-logic-program -m prefix round/ --time-limit 0.5 --resamples 1000 --json /tmp/round-a.json --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_ORDER=program-first taskset -c 1 /tmp/head-logic-program/bin/head-logic-program -m prefix round/ --time-limit 0.5 --resamples 1000 --json /tmp/round-b.json --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_ORDER=legacy-first taskset -c 1 /tmp/head-logic-program/bin/head-logic-program -m prefix round/simple/utxo-100/backlog-1000 round/cardano/utxo-10000/backlog-0 round/cardano/utxo-10000/backlog-1000 --time-limit 5 --resamples 10000 --json /tmp/round-c.json --regress allocated:iters +RTS -T -N1
```

## Resolved transaction experiment

These Simple-ledger fixtures use one input and one output with large unrelated
background sets. The actual ReqTx program supplies the validated endpoints.
Complete decoded replay states are compared before timing. Delta construction
forces both set differences; recovery forces UTxO size, registry size and sequence
length. This is an in-memory microbenchmark, excluding disk I/O and full-state
serialization. Recovery starts from an already restored initial state.

| Background outputs / operation | Mean µs (95% CI) | Allocated B/op |
| --- | ---: | ---: |
| background-100/construct-delta | 1.2098 (1.2078–1.2113) | 128 |
| background-100/resolved-recovery | 0.15538 (0.15519–0.15554) | 1232 |
| background-100/compatibility-recovery | 0.13021 (0.13001–0.13049) | 1136 |
| background-100/decode-and-recover | 1.0242 (1.0233–1.0252) | 9504 |
| background-10000/construct-delta | 116.84 (116.7–117) | 127 |
| background-10000/resolved-recovery | 0.23248 (0.23211–0.23278) | 1712 |
| background-10000/compatibility-recovery | 0.19216 (0.19169–0.19258) | 1616 |
| background-10000/decode-and-recover | 1.1185 (1.117–1.1197) | 9984 |
| background-30000/construct-delta | 354.82 (354.52–355.17) | 128 |
| background-30000/resolved-recovery | 0.25283 (0.25256–0.25322) | 1872 |
| background-30000/compatibility-recovery | 0.21422 (0.2139–0.21451) | 1776 |
| background-30000/decode-and-recover | 1.1396 (1.1382–1.1417) | 10144 |

Encoded sizes (one committed event list, including the experimental version prefix):

```text
background=100 resolved-bytes=168 compatibility-bytes=184
background=10000 resolved-bytes=168 compatibility-bytes=184
background=30000 resolved-bytes=168 compatibility-bytes=184
```

Production recovery already skips ledger validation. The experiment also removes
`applyTxTo` reconstruction, but deriving a delta from whole UTxOs adds construction
cost that grows with the background set. At 30,000 background outputs that cost
is about 355 µs, while resolved recovery takes 253 ns versus compatibility replay's
214 ns. Thus this Simple-ledger experiment has a smaller encoded record but no
recovery-time saving, and substantially more decision-time work. It does not
justify changing the production path; the experimental CBOR format remains separate.

```sh
nix build .#head-logic-lab --out-link /tmp/head-logic-lab
taskset -c 1 /tmp/head-logic-lab/bin/head-logic-lab resolved --time-limit 0.5 --resamples 1000 --json /tmp/resolved.json --regress allocated:iters +RTS -T -N1
```

## After the module split and registry fix

Measured on 2026-11-04 with GHC 9.6.7, Criterion 1.6.5.0, Cabal `-O1`, CPU 1
and one RTS capability. These are new measurements, not replacements for the
historical Nix runs above. Builds/tests were stopped during timing; tracing was
disabled. The applied-ID guard is included, so comparison includes that rule
change as well as interpretation. The measured fixtures use fresh IDs.

Runs A/B use both pair orders, 0.5-second windows and 1,000 resamples. C repeats
the Simple backlog for two seconds. D/E repeat the noisy small Cardano case for
two seconds in both orders. This revision remeasures the 100-output grid; the
10,000-output round grid remains historical evidence, not a new result.
[Individual estimates](measurements/head-logic-measurements-v2.json) retain confidence and
allocation information.

| Ledger / backlog | Run | Legacy ms (95% CI) | Program ms (95% CI) | Mean change |
| --- | --- | ---: | ---: | ---: |
| simple / 0 | A | 3.0350 (3.0279–3.0446) | 3.0208 (3.0148–3.0265) | -0.47% |
| cardano / 0 | A | 3.3307 (3.3253–3.3363) | 3.5652 (3.4948–3.7267) | +7.04% |
| simple / 1000 | A | 4.0058 (3.9737–4.0791) | 4.0692 (4.0607–4.0765) | +1.58% |
| cardano / 1000 | A | 131.3721 (130.9676–131.8982) | 133.4048 (132.6413–134.8054) | +1.55% |
| simple / 0 | B | 3.1421 (3.1337–3.1590) | 3.1352 (3.1308–3.1392) | -0.22% |
| cardano / 0 | B | 3.4683 (3.4616–3.4815) | 3.5286 (3.4953–3.6317) | +1.74% |
| simple / 1000 | B | 4.0793 (4.0717–4.0890) | 4.2320 (4.2274–4.2373) | +3.74% |
| cardano / 1000 | B | 134.6704 (133.9568–135.4311) | 135.1481 (134.7064–135.8000) | +0.35% |
| simple / 1000 | C | 3.9903 (3.9867–3.9941) | 4.1166 (4.1102–4.1238) | +3.17% |
| cardano / 0 | D | 3.2876 (3.2725–3.3025) | 3.3547 (3.3181–3.3953) | +2.04% |
| cardano / 0 | E | 3.4965 (3.4834–3.5108) | 3.6361 (3.6161–3.6710) | +3.99% |

The short Cardano run reaches **+7.04%**, crossing the provisional 5% investigation
threshold. Longer repeats give +2.04% and +3.99%; this does not establish a strict
upper bound or erase the noisy result. The Simple backlog remains around
**+31.94% allocations** (3,963,245 versus 5,229,110 bytes in run A), despite smaller
time differences. The module split and capability typing do not resolve that
cost. Profile these allocations before extending the migration.

The stricter resolved-v2 contract is measured separately with 0.5-second windows,
1,000 resamples and the same CPU/RTS setup:

| Background / operation | Mean µs (95% CI) | Allocated B/op |
| --- | ---: | ---: |
| background-100/construct-delta | 1.3541 (1.3505–1.3573) | 256 |
| background-100/resolved-recovery | 0.5189 (0.5174–0.5201) | 2768 |
| background-100/compatibility-recovery | 0.1440 (0.1435–0.1445) | 1136 |
| background-100/decode-and-recover | 1.6423 (1.6391–1.6444) | 11632 |
| background-10000/construct-delta | 129.2619 (128.8620–129.8989) | 256 |
| background-10000/resolved-recovery | 0.6896 (0.6880–0.6910) | 3728 |
| background-10000/compatibility-recovery | 0.2164 (0.2156–0.2174) | 1616 |
| background-10000/decode-and-recover | 1.8510 (1.8437–1.8579) | 12592 |
| background-30000/construct-delta | 396.3382 (394.4248–400.2031) | 255 |
| background-30000/resolved-recovery | 0.7499 (0.7425–0.7588) | 4048 |
| background-30000/compatibility-recovery | 0.2323 (0.2311–0.2334) | 1776 |
| background-30000/decode-and-recover | 1.9213 (1.8828–1.9681) | 12912 |

The v2 record is **211 bytes**, versus **184 bytes** for the compatibility events,
at all three background sizes. Keeping the head identity and invalidation
information removes the apparent size advantage of v1. At 30,000 outputs,
construction costs about **396 µs**; checked recovery costs **750 ns** versus
**232 ns** for compatibility recovery. Delta construction still compares whole
UTxOs. This representation is slower and larger in the measured workload. It remains
an experiment; production persistence and event encodings are unchanged.

Reproduce the new grid after building the two Cabal targets. Pin to an available
CPU using your platform's affinity tool (CPU 1 was used here):

```sh
cabal build hydra-node:bench:head-logic-program hydra-node:exe:head-logic-lab
bench=$(cabal list-bin hydra-node:bench:head-logic-program)
lab=$(cabal list-bin hydra-node:exe:head-logic-lab)
HEADLOGIC_BENCH_MAX_UTXO=100 HEADLOGIC_BENCH_ORDER=legacy-first taskset -c 1 "$bench" -m prefix round/ --time-limit 0.5 --resamples 1000 --json /tmp/round-a.json --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_MAX_UTXO=100 HEADLOGIC_BENCH_ORDER=program-first taskset -c 1 "$bench" -m prefix round/ --time-limit 0.5 --resamples 1000 --json /tmp/round-b.json --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_MAX_UTXO=100 taskset -c 1 "$bench" -m prefix round/simple/utxo-100/backlog-1000 --time-limit 2 --resamples 1000 --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_MAX_UTXO=100 HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER=legacy-first taskset -c 1 "$bench" -m prefix round/cardano/utxo-100/backlog-0 --time-limit 2 --resamples 1000 --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_MAX_UTXO=100 HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER=program-first taskset -c 1 "$bench" -m prefix round/cardano/utxo-100/backlog-0 --time-limit 2 --resamples 1000 --regress allocated:iters +RTS -T -N1
taskset -c 1 "$lab" resolved --time-limit 0.5 --resamples 1000 --json /tmp/resolved-v2.json --regress allocated:iters +RTS -T -N1
```

## End-to-end cluster comparison

The cluster comparison uses two separate `hydra-node` binaries and the same
`bench-e2e` executable. The baseline is the current checkout with only
`Hydra.HeadLogic` restored from `7941ca4f8`, before the program migration. Other
node, transport, API and persistence code stays the same. This measures the
combined program migration and duplicate-ID fix, not interpreter dispatch alone.
There is no production flag for selecting old handlers.

The final comparison uses matching Nix builds with GHC 9.6.7 and
`+RTS -N1 -T`. Three Hydra nodes,
their etcd processes and a Cardano devnet run on one machine. The client uses
JSON over WebSockets; persistence is enabled and node logging is quiet. Snapshot
UTxOs are omitted from API responses, as in the existing benchmark harness.
The measured window starts with transaction submission and ends with confirmation;
Head initialization, initial deposit settlement, closing and fanout are outside it.
Confirmation means the API reports the signed snapshot; it does not wait for a
per-snapshot disk flush. The existing SQLite writer runs asynchronously on both
sides. RTS counters cover the transaction window; peak RSS in the raw report
covers the whole lifecycle, including fanout.

The local devnet harness funds each node with 1,000 ADA for fees and collateral.
A calibration with the former 100 ADA exhausted collateral during partial fanout
of the 3,000-output Head, after all transactions had confirmed. The increased
funding is confined to local datasets; the demo on an existing network keeps its
usual funding. See `fundDatasetWithFuel` in
[HeadLifecycle.hs](../../../../../hydra-cluster/src/Hydra/Cluster/HeadLifecycle.hs)
and its call in [EndToEnd.hs](../../../../../hydra-cluster/bench/Bench/EndToEnd.hs).

Each workload runs in both orders, giving two pairs. The final runs have no
concurrent builds or tests. The constant-UTxO workload has 3,000 payments per
client; the large case has 500 transactions per client and grows to 1,000 outputs
per client before continuing with self-transfers. These are *closed-loop* runs:
each of the three clients waits for confirmation before submitting its next
transaction. They measure end-to-end performance at that concurrency, not maximum
saturated capacity.

This concurrency also leaves deep pending backlogs and repeated duplicate/retry
traffic unmeasured. In `onOpenNetworkReqTxProgram`, a fresh ID needs a map lookup;
an ID already in `allTxs` can also scan `localTxs`. Capacity testing should include
that path before assigning a general cost to the duplicate-ID fix.

### Results on eight CPUs

Measured on 2026-11-04, on an Intel Core Ultra 7 258V with CPUs 0–7 available.
Every run confirms all submitted transactions, has zero invalid/refused
submissions, and completes fanout. Positive TPS changes favor the program.
The order names the binary run first. Latencies are the inclusive p50/p95 of
the recorded confirmation times, in milliseconds.

| Workload / order | Baseline TPS | Program TPS | TPS change | Baseline p50 / p95 | Program p50 / p95 |
| --- | ---: | ---: | ---: | ---: | ---: |
| Small / baseline first | 84.21 | 152.98 | +81.66% | 23.28 / 65.18 | 18.97 / 24.41 |
| Small / program first | 153.67 | 112.41 | −26.85% | 18.89 / 24.09 | 19.73 / 56.06 |
| Large / baseline first | 20.29 | 20.91 | +3.07% | 164.84 / 197.75 | 156.47 / 190.81 |
| Large / program first | 21.26 | 21.02 | −1.09% | 154.73 / 187.60 | 155.39 / 188.59 |

The small-Head baseline varies by 82% between runs of the **same binary**, and
the program varies by 36%. Both comparisons reverse direction. That prevents
attributing the small-Head timing differences to the program. We have not
identified the source of this runtime variation. The large-Head runs are much
closer, but two pairs still cannot certify the provisional 5% bound.
All eight runs average about 1.50 transactions per snapshot.

Allocation is steadier. The following ranges span the two runs of each binary;
they are not confidence intervals. Allocation and CPU figures sum the three
Hydra nodes over the transaction window. MiB means 2²⁰ bytes.
Allocation and live heap are GHC RTS figures; they exclude native allocations.

| Workload / metric | Baseline | Program |
| --- | ---: | ---: |
| Small: allocated MiB / confirmed tx | 3.735–3.755 | 3.732–3.740 |
| Large: allocated MiB / confirmed tx | 126.130–126.179 | 125.983–126.197 |
| Small: mutator CPU seconds / 1,000 tx | 12.73–17.46 | 12.75–15.03 |
| Large: mutator CPU seconds / 1,000 tx | 235.98–245.04 | 237.22–238.68 |
| Small: GC CPU seconds / 1,000 tx | 0.271–0.367 | 0.271–0.321 |
| Large: GC CPU seconds / 1,000 tx | 9.274–9.769 | 9.301–9.334 |

Mean live heap at major GCs, taking the largest node, stays near 27.8 MiB in the
small case and 29.2 MiB in the large case for both versions. These observations
show less than 1% difference in allocated bytes per transaction in each pair
after the lazy-field fix. GC time changes direction between pairs. These runs do
not test long-lived retention or establish saturated capacity.

### One-CPU control

To investigate the small-Head variability, repeat with all processes pinned to
CPU 0 and 1,500 payments per client: 4,500 transactions per run. The same nodes,
harness, ledger, persistence and RTS flags are used. This changes available CPU
resources and run length; it is a diagnostic comparison, not a deployment model.
All four runs confirm every transaction, report no invalid/refused submissions
and complete fanout. The nodes' `-N1 -T` flags and CPU affinity were also checked
on the running processes.

| Order | Baseline TPS | Program TPS | TPS change | Baseline p50 / p95 ms | Program p50 / p95 ms |
| --- | ---: | ---: | ---: | ---: | ---: |
| Baseline first | 107.69 | 104.93 | −2.56% | 27.18 / 33.22 | 27.49 / 36.73 |
| Program first | 110.04 | 110.01 | −0.03% | 26.63 / 32.19 | 26.70 / 32.27 |

GHC allocation is 3.756–3.757 MiB per transaction for the baseline and
3.759–3.760 MiB for the program. Mutator CPU seconds per 1,000 transactions are
5.46–5.56 versus 5.49–5.51; GC CPU time is about 0.17 seconds for both.
Mean live heap remains near 27.8 MiB, with about 1.50 transactions per snapshot.

This control has less timing spread, but does not identify its cause. Its
small TPS decrease is compatible with modest overhead; four runs cannot
establish a portable 5% bound. A capacity comparison still needs a controlled
runner and bounded pipelining that confirms every submitted transaction.

The [raw results](measurements/head-logic-e2e-v1.json) contain all twelve runs: binary/source
hashes, commands, CPU affinity, timestamps, sorted confirmation times, snapshot
series and per-node RTS counters. All timed runs from these three comparisons
are included.
There are 60,000 confirmed transactions across these fresh Heads.

### What the investigation changed

The preliminary cluster runs prompted a check that exposed unnecessary work in the compatibility
interpreter. With `StrictData`, the before/after UTxO fields of
`RecordAppliedTransaction` and `AppliedTransaction` forced the Cardano ledger's
UTxO conversion even though compatibility interpretation discards those fields.
They are now explicitly lazy. The resolved interpreter still demands them to
construct its experimental delta. A regression test supplies undefined endpoints
and requires compatibility interpretation to emit the original event without
evaluating them; it failed before the change and passes afterwards.

See [Program.hs](../../../../../hydra-node/src/Hydra/HeadLogic/Program.hs),
[Interpreter.hs](../../../../../hydra-node/src/Hydra/HeadLogic/Interpreter.hs) and
[ProgramSpec.hs](../../../../../hydra-node/test/Hydra/HeadLogic/ProgramSpec.hs).
This changes evaluation cost, not protocol decisions or persisted events. The
Agda comparison does not model Haskell strictness.

We also tried an open-loop burst of 10,000 dependent transactions per client.
Neither version confirmed all 30,000 transactions; both encountered refused
submissions and invalid transactions. The raw files from these preliminary runs
were lost when the temporary environment was reset, so they are excluded from
the reported comparison. The harness already documents why this workload can
fail: dependents expire while refused parents wait for resubmission. These runs
do not establish either version's usable capacity or a throughput improvement.
The closed-loop runs avoid that workload failure. A saturation comparison needs
bounded pipelining or independent transactions, with every transaction confirmed.

### Reproduction

Build both nodes from the same checkout. This preserves the existing branch and
uses a temporary directory for the baseline. Run inside the development shell:

```sh
nix build .#hydra-node --out-link /tmp/head-logic-program-node
baseline_dir=$(mktemp -d)
git archive 7941ca4f8 | tar -x -C "$baseline_dir"
git diff 7941ca4f8 --binary | git -C "$baseline_dir" apply
git show 7941ca4f8:hydra-node/src/Hydra/HeadLogic.hs > "$baseline_dir/hydra-node/src/Hydra/HeadLogic.hs"
nix build "path:$baseline_dir#hydra-node" --out-link /tmp/head-logic-baseline-node
bench_e2e=$(nix develop .#hydra-cluster-bench --command sh -c 'command -v bench-e2e')

python3 scripts/head-logic-e2e.py \
  --baseline /tmp/head-logic-baseline-node/bin/hydra-node \
  --program /tmp/head-logic-program-node/bin/hydra-node \
  --bench "$bench_e2e" --output /tmp/head-logic-small \
  --transactions 3000 --shapes Constant --wait-for-confirmation
python3 scripts/head-logic-e2e.py \
  --baseline /tmp/head-logic-baseline-node/bin/hydra-node \
  --program /tmp/head-logic-program-node/bin/hydra-node \
  --bench "$bench_e2e" --output /tmp/head-logic-large \
  --transactions 500 --shapes 'Plateau 1000' --wait-for-confirmation
```

Use short, fresh output directories. The [runner](../../../../../scripts/head-logic-e2e.py)
records binary hashes, dataset hashes, commands, exit status and complete benchmark
JSON, including per-node allocation and GC counters. It rejects differing datasets
within a workload and stops on a failed run. It removes HTTP proxy settings only
from its local benchmark subprocesses, so etcd peers using `0.0.0.0` stay local.

Repeat the CPU-affinity control on Linux with `taskset` available:

```sh
taskset -c 0 python3 scripts/head-logic-e2e.py \
  --baseline /tmp/head-logic-baseline-node/bin/hydra-node \
  --program /tmp/head-logic-program-node/bin/hydra-node \
  --bench "$bench_e2e" --output /tmp/head-logic-one-cpu \
  --transactions 1500 --shapes Constant --wait-for-confirmation
```

For a paired report using the existing analysis code:

```sh
python3 scripts/bench-e2e-diff.py \
  /tmp/head-logic-small/shape-0-pair-0-baseline/end-to-end-benchmarks.md \
  /tmp/head-logic-small/shape-0-pair-0-program/end-to-end-benchmarks.md
```

The adjacent JSON files supply the raw observations. Repeat for pair 1 and the
large case. The report's usual highlighting thresholds differ from this spike's
provisional 5% investigation threshold; neither is a statistical guarantee.

## crem event application

**crem adds about 3–4 µs and 36 KiB of allocation to replaying a complete
round's ten events in these fixtures.** That roughly doubles the reducer's
microbenchmark time. In the longer complete-round repeats, elapsed time changed
by **−0.14% to +1.49%**. This is a small local-round cost; it is not a new
measurement of Hydra's TPS.

[Measurement data](measurements/head-logic-crem-measurements-v1.json) records all eight runs,
confidence estimates, allocation regressions, CPU and source fingerprints.
GHC 9.6.7, `-O1`, criterion 1.6.5.0, crem 0.1.1.0; one pinned CPU, `-N1`,
tracing disabled. Each fixture has one incoming transaction and no backlog.

| Ledger / background UTxO | Replay before → crem, µs | Added replay, µs | Complete-round change: forward / reverse |
| --- | ---: | ---: | ---: |
| Simple / 100 | 2.63 → 5.97 | 3.36 | +1.10% / +0.09% |
| Simple / 10,000 | 2.77 → 6.11 | 3.09 | +0.70% / +0.38% |
| Cardano / 100 | 3.74 → 7.92 | 4.17 | +1.14% / approximately 0% |
| Cardano / 10,000 | 4.03 → 7.60 | 3.55 | +1.49% / −0.14% |

Replay figures are medians across six alternating runs with a 0.5-second sampling
budget; added time is the median paired difference. Their added allocation is 35.75 KiB per round in every fixture. The
short complete-round timings were noisy, ranging from apparent improvements to
a 31.8% slowdown. Two further round runs used a 5-second budget and reversed the
measurement order; the table reports those separately. Combining their marginal
interval endpoints gives diagnostic bounds within −0.79% to +3.06%; those bounds
are not a joint confidence interval or a general 5% guarantee. The earlier
noise remains in the data rather than being discarded.

The allocation increase warrants attention for cheap, heavily batched workloads.
It is about 5.8% of the small Simple round's allocation and 3.1% of the small
Cardano round's allocation, falling below 0.1% for the large-UTxO rounds. These
figures isolate an earlier event machine; the current design is measured above.

The additional machine comparison uses the same `ReqTx`, `ReqSn` and `AckSn`
programs on both sides. Only event application changes: the frozen reducer
versus crem composition. Both must produce the same complete outcomes and
final state and confirm the next snapshot before timing starts.

`aggregation` measures a complete local round, including ledger application,
signing and verification, but excluding network and disk I/O. The adjacent
`event-replay-crem` group applies that round's already-computed events. This
separates event-application overhead from the rest of the round. The existing
checksum forces transaction collections, local UTxO size and confirmed snapshot
contents; equivalence checks compare complete states outside timing.

```sh
HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER=legacy-first cabal bench hydra-node:head-logic-program \
  --benchmark-options='-L 5 --resamples 1000 -m glob "round/*/utxo-*/backlog-*/aggregation/*" --regress allocated:iters --json /tmp/crem-forward.json +RTS -T -N1'
HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER=program-first cabal bench hydra-node:head-logic-program \
  --benchmark-options='-L 5 --resamples 1000 -m glob "round/*/utxo-*/backlog-*/aggregation/*" --regress allocated:iters --json /tmp/crem-reverse.json +RTS -T -N1'
# For replay alone, select round/*/utxo-*/backlog-*/event-replay-crem/* and use -L 0.5.
# Repeat each order three times; use the same CPU affinity for all runs.
```

Alternate the order and repeat noisy cases. Keep the 5% investigation threshold
from the earlier measurements; overlapping uncertainty is not evidence of
meeting it. These in-process measurements do not establish a change in Hydra TPS.

## Programs inside crem

The five-handler conversion runs the existing programs as actions in crem.
This comparison isolates that change: frozen free ReqTx/ReqSn/AckSn programs
and production workflows use the same interpreter and event aggregation.
Tracing is disabled. ReqDec and Recover are not part of this timed round.

**The measured allocation increase is about 37–40 KiB per local snapshot round.**
Small Cardano rounds changed by −0.25% to +3.27% across four paired runs. The
large Cardano case ranged from −2.33% to +5.67%; longer repeats gave +2.10% and
−5.32%. The change is sensitive to run order and timing drift. These results do
not establish compliance with the provisional 5% threshold or an end-to-end TPS
cost. Earlier cluster results predate the crem workflows and event machines.

Measured on 2026-11-05: GHC 9.6.7, `-O1`, criterion 1.6.5.0, crem 0.1.1.0,
Intel Core Ultra 7 258V, CPU 0 affinity, one RTS capability. Builds and tests
were stopped during measurement. Four grid passes used a two-second limit and
1,000 resamples, in forward/reverse/reverse/forward order. The table gives the
median mean time of each implementation and the range of within-run changes;
the range is not a confidence interval. All cases have zero backlog and one
incoming transaction.

| Ledger / background UTxO | Free → crem, median ms | Paired time change, four runs | Added allocation per round |
| --- | ---: | ---: | ---: |
| Simple / 100 | 3.102 → 3.128 | −0.32% to +2.25% | 36.6 KiB |
| Cardano / 100 | 3.452 → 3.463 | −0.25% to +3.27% | 39.8 KiB |
| Simple / 10,000 | 214.541 → 207.877 | −4.16% to +1.09% | 37.3 KiB |
| Cardano / 10,000 | 215.291 → 216.504 | −2.33% to +5.67% | 38.8 KiB |

The +5.67% result triggered ten-second repeats with 10,000 resamples in both
orders. Free-first measured 205.19 → 209.51 ms (+2.10%); crem-first measured
224.55 → 212.59 ms (−5.32%). The opposite signs and moving baseline leave a small
runtime effect unresolved. The [measurement data](measurements/head-logic-workflow-measurements-v1.json)
retains each estimate, its confidence interval, allocation regression, outlier
variance and sample count. None of these local rounds measures saturated TPS,
transport, persistence or the API projection.

Reproduce after building, with builds and tests stopped. On Linux, pin the
process to one CPU with `taskset -c 0` or an equivalent affinity setting:

```sh
cabal build hydra-node:head-logic-program
workflow_bench=$(cabal list-bin hydra-node:head-logic-program)
HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER=legacy-first \
  taskset -c 0 "$workflow_bench" \
  --match pattern /workflow/ --time-limit 2 --resamples 1000 \
  --regress allocated:iters --json /tmp/workflow-forward.json +RTS -T -N1
# Repeat with program-first, program-first, legacy-first and separate output files.
# For the noisy case, select --match prefix round/cardano/utxo-10000/backlog-0/workflow/
# and use --time-limit 10 --resamples 10000 in both orders.
```

The benchmark checks complete outcome/state equality and successful confirmation
before timing. The checksum's evaluation boundary is the same in both paths.
The frozen free programs share selection helpers with production and live in a
different module, so this measures the concrete conversion, including its module
boundaries. It is not a general cost model for crem or free monads.

## Complete input workflow

This comparison calls `HeadLogic.update` for every input in the local snapshot
round: ReqTx, ReqSn and three AckSn messages. The baseline calls the frozen
`LegacyHandlers.update`, including its admission and dispatch logic and frozen
network programs. Both apply events through the same production crem machines.
It measures the complete input conversion, including the five workflows measured
above; it does not isolate the cost of the admission machine.

**The conversion adds about 56 KiB per round.** Four balanced grid runs measured
paired time changes from −1.00% to +1.43%. Longer large-UTxO repeats ranged from
−2.48% to +1.44%. The small Simple allocation increase exceeds the provisional
5% investigation threshold. These local figures do not
establish a TPS change or a general runtime bound.

Measured on 2026-11-05 with GHC 9.6.7, `-O1`, Criterion 1.6.5.0, crem 0.1.1.0,
Intel Core Ultra 7 258V, CPU 0 affinity and one RTS capability. Builds and tests
were stopped. Tracing was disabled. Each fixture has one incoming transaction,
zero backlog and 100 or 10,000 background outputs. Complete ordered outcomes,
states and successful confirmation are checked before timing. The timed checksum
forces the same structural projection in both paths.

The grid uses a two-second sampling limit and 1,000 resamples, in
frozen-first/crem-first/crem-first/frozen-first order. Times below are medians of
the four means; paired ranges are observed differences, not confidence intervals.

| Ledger / background UTxO | Frozen → crem, median ms | Paired time change, four runs | Added allocation per round |
| --- | ---: | ---: | ---: |
| Simple / 100 | 3.175 → 3.172 | −0.38% to +1.22% | 55.8 KiB (+8.56%) |
| Cardano / 100 | 3.502 → 3.531 | +0.24% to +1.43% | 55.9 KiB (+4.66%) |
| Simple / 10,000 | 206.264 → 206.744 | −1.00% to +0.80% | 55.7 KiB (+0.13%) |
| Cardano / 10,000 | 211.546 → 212.804 | −0.68% to +0.76% | 56.8 KiB (+0.10%) |

The large fixtures have only four samples per implementation in the grid, so
they were repeated with ten-second limits and 10,000 resamples. Those runs have
eight or nine samples per implementation. Marginal 95% confidence intervals are
shown below; they do not account for drift between runs.

| Ledger / 10,000 outputs | Order | Frozen ms (95% CI) | Crem ms (95% CI) | Paired change |
| --- | --- | ---: | ---: | ---: |
| Simple | crem first | 197.773 (197.035–198.405) | 192.861 (191.946–193.833) | −2.48% |
| Cardano | crem first | 213.598 (212.565–215.146) | 208.617 (207.638–209.658) | −2.33% |
| Simple | frozen first | 212.800 (211.815–214.645) | 215.858 (215.006–217.130) | +1.44% |
| Cardano | frozen first | 225.382 (223.970–228.018) | 222.665 (220.875–224.783) | −1.21% |

The [measurement data](measurements/head-logic-input-measurements-v1.json) retains
all estimates, confidence errors, allocation regressions, outlier variance and
sample counts, with the measured binary's hash. Network, persistence, API
projection, queueing, chain actions and saturated throughput are outside this
benchmark. The frozen code shares primitives and pure helpers with production;
module boundaries and compiler choices are part of the measured change.

```sh
cabal build hydra-node:head-logic-program
input_bench=$(cabal list-bin hydra-node:head-logic-program)
HEADLOGIC_BENCH_MAX_BACKLOG=0 HEADLOGIC_BENCH_ORDER=legacy-first \
  taskset -c 0 "$input_bench" \
  --match pattern /input/ --time-limit 2 --resamples 1000 \
  --regress allocated:iters --json /tmp/input-forward.json +RTS -T -N1
# Repeat with program-first, program-first, legacy-first and separate output files.
# For the large cases, use --match pattern /utxo-10000/backlog-0/input/
# with --time-limit 10 --resamples 10000 in both orders.
```
