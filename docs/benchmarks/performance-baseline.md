---
sidebar_position: 11
---

# Comparing algorithm optimizations

Pin the baseline to a full commit SHA. Rerun that commit beside each candidate
on the same machine; archived timings alone do not control for machine load,
compiler changes, or different hardware. Keep benchmark-only changes identical
on both sides, and introduce one production optimization per comparison.

## Establish the noise floor

The `bench-e2e-diff.yaml` workflow accepts `head_ref` and `base_ref`. Set both
to the baseline SHA for an A/A comparison: any reported improvement is noise.
Repeat this experiment before interpreting small changes. See the
[comparison methodology](https://github.com/cardano-scaling/hydra/blob/master/hydra-cluster/README.md#pr-comparison-methodology)
for the paired runs, runtime settings, and threshold calibration.

The initial baseline for this series is master commit
`0aa8987dc49e85756e283f042422e98d20777adb`. Use full SHAs rather than moving branch
names when dispatching comparisons:

```sh
gh workflow run bench-e2e-diff.yaml --repo cardano-scaling/hydra --ref master \
  -f head_ref=0aa8987dc49e85756e283f042422e98d20777adb \
  -f base_ref=0aa8987dc49e85756e283f042422e98d20777adb
```

For an optimization, replace only `head_ref` with its published commit SHA.
For policy-specific work, first establish a baseline containing the same policy
functionality; comparing different feature sets confounds the result.

## Measure the affected algorithm

Run these commands in the development shell on both revisions, with the same
benchmark harness and compiler options. Build before timing, use separate
output directories, and alternate baseline/candidate order across repetitions.

```sh
mkdir -p test-reports
BENCH_MAX_UTXO=4000 cabal bench hydra-node:bench:fanout \
  --benchmark-options='--time-limit 1 --json test-reports/fanout.json +RTS -N1 -RTS'
BENCH_MAX_UTXO=4095 cabal bench hydra-node:bench:snapshot \
  --benchmark-options='--time-limit 1 --json test-reports/snapshot.json +RTS -N1 -RTS'
BENCH_MAX_UTXO=4095 cabal bench hydra-tx:bench:accumulator-bench \
  --benchmark-options='--time-limit 1 --json test-reports/accumulator.json +RTS -N1 -RTS'
```

These suites use fixed QuickCheck seeds and size 30. The fanout suite checks
fixture sizes and expected results before timing. It covers original and
renamed references, duplicate outputs, a missing last output, excessive
duplicate multiplicity, and equal sets. It varies selection size independently
of the remaining UTxO size. Raise `BENCH_MAX_UTXO` to `16000` for larger fanout
cases; the baseline's content-based removal can take quadratic time.

Fixture construction is outside the timed fanout operation. The membership
and equality results are forced to `Bool`; accumulator benchmarks force the
cryptographic result and avoid timing a memoized commitment. Preserve these
forcing boundaries when editing the benchmarks.

The snapshot suite also keeps 100 or 1,000 transactions pending after a
one-transaction snapshot. Compare its `update-and-aggregate` cells to measure
pruning and reconstruction of the local UTxO, and `capacity-check` to isolate
the output-count guard. The transaction sequences use the same fixed seeds on
both revisions and are validated before timing.

Compare each named cell across revisions, including its uncertainty, and plot
cost against input size when assessing complexity. A fanout optimization should
also pass the existing partial-fanout tests: matching outputs by content and
preserving duplicate multiplicity are part of the behavior being measured.

## Keep end-to-end coverage and evidence

The paired workflow uses fixed datasets for sustained three-node load,
1,000-UTxO load, and paced submissions. Preserve the generated dataset files
and their checksums. Increase workload sizes if runs have too few snapshot
points to estimate sustained throughput, then use those same files on both
sides. The workflow uses quiet logging and tmpfs; storage and logging changes
need additional scenarios with representative settings.

Archive the following outside expiring CI artifacts:

- Baseline and candidate SHAs, the benchmark harness patch or SHA, and lockfile.
- Dataset files and checksums, exact commands, compiler and RTS settings.
- CPU, architecture, operating system, available memory, and storage mode.
- Raw Criterion JSON and end-to-end JSON, reports, and failure outcomes.

Use throughput and backlog drain time for saturated runs, confirmation
percentiles for paced runs, and allocation per transaction, CPU per 1,000
transactions, and memory for both. Do not interpret a missing metric as zero.
Require correct outcomes and improvements above the measured noise floor;
retain failed runs instead of selecting only successful or fastest samples.
