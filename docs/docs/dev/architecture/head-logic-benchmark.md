# Head logic program benchmark

The repository guide at `docs/head-logic-review.md` explains the implementation
and links to the measured code. This report covers four workloads:
[the original ReqTx slice](#original-reqtx-measurements),
[complete snapshot rounds](#complete-snapshot-rounds),
[resolved transaction recovery](#resolved-transaction-experiment), and
[a three-node cluster](#end-to-end-cluster-comparison). Each section records its
inputs, measurement method and limits. The in-process results predate the lazy
UTxO-field fix described with the cluster comparison.

## What does this cost in TPS?

**The one-CPU control measured 0.03–2.56% lower TPS for the program. The large-Head
runs ranged from 1.09% lower to 3.07% higher.** The small-Head runs on eight CPUs
were too variable to resolve a 5% effect. These are observed differences across
two pairs per case, not confidence bounds.

The [cluster comparison](#end-to-end-cluster-comparison) measures transaction
submission through signed snapshot confirmation, with real Cardano transactions,
three nodes, transport, API serialization and asynchronous persistence enabled.
Each client waits for confirmation before sending its next transaction, so it
measures end-to-end performance with three outstanding transactions. It does not
measure maximum saturated TPS or durable confirmation after a disk flush.

The earlier in-process Cardano measurements showed about 2–4% extra processing
time for a small snapshot round in the longer repeats, and 0.35–1.55% with 1,000
pending transactions. One short run measured 7%. These are historical measurements
of one node's handlers; they cannot certify a 5% end-to-end bound.

Production executes compiled Haskell with instruction/continuation bookkeeping
and the same ledger and cryptographic operations. Tracing is disabled. The
explorer, debugger and Mermaid renderer are development tools. The comparison
also includes the duplicate-ID guard, so it measures the combined change.

### What an in-process round includes

[`runRound`](../../../../hydra-node/bench/HeadLogicProgram.hs) executes one node's
`ReqTx`, `ReqSn` and three `AckSn` handlers, including ledger application, snapshot
construction, signing, verification and state aggregation. It synthesises the
other parties' signatures. The backlog is already applied locally before timing;
only one new `ReqTx` is processed. Neither `1 / round time` nor
`transactions in the snapshot / round time` is an end-to-end TPS result.

This benchmark excludes the other nodes, network transport, API work,
message/event serialisation, persistence and queueing under load. It forces the
state projection used by the checksum, not every field in the complete state.

### The resolved-replay experiment is a separate cost

The production interpreter emits the existing `StateChanged` events. It does
not construct the experimental UTxO deltas. The roughly **396 µs** delta cost
at 30,000 outputs, slower experimental recovery and larger v2 records therefore
do not reduce production TPS in this spike. Those results argue against adopting
that representation; they are not an additional charge for interpreting the
head logic.

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

See the [refactor sketch](./head-logic-refactor.md) for interpretation semantics,
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
The [versioned measurements](./head-logic-measurements-v1.json) retain those nulls
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
[Individual estimates](./head-logic-measurements-v2.json) retain confidence and
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
[HeadLifecycle.hs](../../../../hydra-cluster/src/Hydra/Cluster/HeadLifecycle.hs)
and its call in [EndToEnd.hs](../../../../hydra-cluster/bench/Bench/EndToEnd.hs).

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

The [raw results](./head-logic-e2e-v1.json) contain all twelve runs: binary/source
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

See [Program.hs](../../../../hydra-node/src/Hydra/HeadLogic/Program.hs),
[Interpreter.hs](../../../../hydra-node/src/Hydra/HeadLogic/Interpreter.hs) and
[ProgramSpec.hs](../../../../hydra-node/test/Hydra/HeadLogic/ProgramSpec.hs).
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

Use short, fresh output directories. The [runner](../../../../scripts/head-logic-e2e.py)
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
