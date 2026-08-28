# Hydra Cluster Tool

This package contains the same-named `hydra-cluster` executable, which provides
some tools to work with a "cluster" of Hydra nodes running on top of a Cardano
network:
* It can be used for "smoke testing" the `hydra-node` against an
  existing, well-known Cardano network (eg. `preprod` or `preview`
  testnets),
* It can be used to spin-up a "development" Cardano network made up of a single
  cardano-node, with Hydra validators published in a transaction.

## Requirements

The `hydra-cluster` works by spawning processes running the required
executables, namely `hydra-node` and `cardano-node`, which therefore
must be in scope. You could use a special nix shell containing
`hydra-node` and `hydra-cluster` executables:

```sh
nix develop .#exes
```

Or use an alias:

```sh
cabal build hydra-node
alias hydra-node=$(cabal exec which -- hydra-node)
```

The `HYDRA_CONFIG_DIR` environment variable is used to control where the executable will look
for its configuration files. By default those files are resolved using the cabal package's
data structure which is not always convenient.

The `HYDRA_BACKEND` environment variable is used to choose over which backend we will run our
end-to-end tests. Possible values are _devnet_, _preview_, _preproduction_, _mainnet_ or _blockfrost_ (eg. export HYDRA_BACKEND="mainnet").
If this env variable is not set, the tests will default to local _devnet_ backend.

**Note:** When using _mainnet_, the tests will use real ADA from your faucet account. Ensure you understand the costs involved and have sufficient funds available.

To run the e2e tests successfully using blockfrost backend there should be also
a file named `blocfrost-project.txt` in the root of the repository with the
appropriate api key for the network you want to run on.

## Smoke Testing

The `hydra-cluster` executable spins up a `cardano-node` as a network
participant which synchronizes the block chain and then executes a
single scenario (single party, full life cycle) using funds available
to the `config/credentials/faucet.sk` on that network.

Smoke tests can also run using _Blockfrost_ in which case there is no need to
start `cardano-node`.

The Hydra nodes can reference pre-existing contracts living at some well-known
transaction or can post a new transaction to use those contracts. On testnets,
`--publish-hydra-scripts` caches the tx ids in
`<state-directory>/.hydra-scripts-tx-ids` and reuses them on the next run if the
scripts they point at still match the ones compiled in; a script change
republishes. Mainnet always publishes, since that check cannot tell a changed
script from a transient query failure.

On testnets the scenario also runs with shorter periods than the end-to-end
tests use (see `mkSmokeTiming`): most of its wall clock would otherwise be spent
waiting out `depositActivation` and the contestation period. Mainnet keeps the
end-to-end timings -- it runs once per release, so there is nothing to gain, and
a shorter contestation period only narrows the window its close transaction has
to be included.

:warning: do not provide actual funds to this faucet address as the
signing key is publicly available. Shall you want to run the smoke
test with actual funds, you shall override these file to use a secret
signing key safely stored. See how the C.I overrides these files in
.github/workflows/smoke-test.yaml

To run the smoke test against the official cardano testnet using a
local `state-testnet` directory (to re-use the synchronized chain db):

Note: To get the transaction id for `--hydra-scripts-tx-id` parameter you can
consult our [release page](https://github.com/cardano-scaling/hydra/releases)
where you can find pre-published Hydra scripts for different networks.

```sh
hydra-cluster --preview --state-directory state-testnet --hydra-scripts-tx-id <tx-id>
```

> Note: If you want to do it on mainnet
> ```sh
> hydra-cluster --mainnet --state-directory state-mainnet --hydra-scripts-tx-id <tx-id>
> ```

:warning: the C.I. overrides these files for mainnet. On the C.I. the
faucet secrets are base64 encoded. Shall you need to update them, you
could do it with the following commands (if you're not sure, do not
do this, you may loose access to faucet funds):

```sh
#> cat faucet.vk | base64 | gh secret set faucet_vk
#> cat faucet.sk | base64 | gh secret set faucet_sk
```

## Local devnet

`hydra-cluster` can run a local cardano devnet in the form of a single
block producer with a faster production rate than normal. This is useful in situations where
one wants to test Hydra-based DApps, eg. applications and services using Hydra.

Running the following command in the toplevel directory:

```sh
HYDRA_CONFIG_DIR=hydra-cluster/config hydra-cluster --devnet --state-directory test --publish-hydra-scripts
```

will result in a cardano-node running its own network, yielding the following UTxO

```
% CARDANO_NODE_SOCKET_PATH=test/node.socket cardano-cli query utxo --testnet-magic 42 --whole-utxo
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a221c2db8e6f5972e75f9ac14bedf3be20b450cf8e58a58fd277844f36450112     0        900000000000 lovelace + TxOutDatumNone
c878fb55a32295dc940c8167e844d27a8abaa813a05c23c9ca560885a3eb0a1d     0        24321330 lovelace + TxOutDatumNone
c878fb55a32295dc940c8167e844d27a8abaa813a05c23c9ca560885a3eb0a1d     1        15253090 lovelace + TxOutDatumNone
c878fb55a32295dc940c8167e844d27a8abaa813a05c23c9ca560885a3eb0a1d     2        899959869799 lovelace + TxOutDatumNone
```

Adding ` --publish-hydra-scripts` argument will ensure Hydra validator scripts are published
on the network and available as reference inputs for hydra-node.

# Test suite

The `hydra-cluster:test:integration` test suite runs multiple scenarios on a
local cluster of `hydra-node`s connected to a local Cardano "devnet".

This "devnet" is a trivial Cardano network comprised by only a single block
producing node, having all the stake. This is of course not exactly how a proper
Cardano network would work, but the scope of this package and it's tests are to
assert correct hydra-node behavior in a cluster of Hydra nodes.

Run the integration test suite with `cabal test`

# Benchmarks

The benchmark can be run using `cabal bench` or `cabal run bench-e2e` and
produces a `results.csv` file in a work directory. To plot the transaction
confirmation times you can use the `bench/plot.sh` script, passing it the
directory containing the benchmark's results.

For the benchmarks, you can choose between several generated UTxO shapes via
`--utxo-size`:

* `Constant`: each transaction spends one input and creates exactly one new
  output (1-in-1-out), so the size of the UTxO set stays flat over the run.
* `Growing`: each transaction spends one input and creates two outputs,
  gradually increasing the UTxO set as more transactions are processed.
* `Mixed`: grows for the first half of the run, then contracts via 2-in-1-out
  merges for the second half.
* `Plateau N` (quoted, e.g. `--utxo-size 'Plateau 1000'`): splits each
  client's funds into N outputs, then holds that size with full-value
  self-transfers so every snapshot carries a large UTxO set. This is the
  reference workload for large-UTxO head performance, where snapshot signing
  cost dominates.

The number of transactions per client is set with `--number-of-txs`. This
distinction allows you to measure performance under different realistic
UTxO-set scenarios on Cardano.


To generate, run and then plot results of the benchmark:

```sh
cabal run bench-e2e -- datasets --number-of-txs 10 --output-directory out
./hydra-cluster/bench/plot.sh out
```

Which will produce an output like:

```
Writing dataset to: out/dataset.json
Saved dataset in: out/dataset.json
Test logs available in: out/test.log
Starting benchmark
Seeding network
Fund scenario from faucet
Fuel node key "006ba2f18d2e08f1cb96d3a425090768e3b6dc5e7f613a882509a02af668e6d7"
Fuel node key "33184090500d0c26994df825800d169021e6dc32ecf1633d0903c28eecd87830"
Fuel node key "d7f2a66d3f7bc9bdf135ad28b5106ee751aa5725d767336a2aa1ee19a5532c00"
Publishing hydra scripts
Starting hydra cluster in out
Initializing Head
Committing initialUTxO from dataset
HeadIsOpen
Client 1 (node 0): 1/10 (10.00%)
Client 2 (node 1): 1/10 (10.00%)
Client 3 (node 2): 1/10 (10.00%)
All transactions confirmed. Sweet!
All transactions confirmed. Sweet!
All transactions confirmed. Sweet!
Closing the Head
Writing results to: out/results.csv
Finalizing the Head
Confirmed txs/Total expected txs: 30/30 (100.00 %)
Average confirmation time (ms): 59.977068200
P99: 75.43316676ms
P95: 70.41318959999998ms
P50: 60.638328ms
Invalid txs: 0
Fanout outputs: 3
Writing report to: out/end-to-end-benchmarks.md

./hydra-cluster/bench/plot.sh out
Created plot: out/results.png
```

Note that the summary reports the peak resident memory of the scenario's
hydra-node processes (Linux only).

The benchmark can be run in several modes:

* `single`: Runs one or more pre-existing _dataset_ files in sequence and collects their results in a single markdown formatted file. This is useful to track the evolution of hydra-node's performance over well-known datasets and is what CI uses to compare a PR against master.
* `datasets`: Generates a dataset from options (UTxO shape, cluster size, number of txs), saves it, and runs it. `--seed` makes generation reproducible.
* `generate`: Generates and saves a dataset file without running it. `--seed` makes generation reproducible and `--title` names the scenario in reports (scenarios are paired by title when diffing two reports). Feed the resulting file to `single`.
* `matrix`: Runs a scenario matrix over cluster sizes, UTxO shapes and incremental-ops modes, and writes a `scenarios.md` comparison page. With `--seed N`, cell i generates from seed N+i, so the matrix is reproducible across runs (CI pins this).
* `demo`: Generates transactions against an already running network of cardano and hydra nodes. This can serve as a workload when testing network-resilience scenarios, such as packet loss or node failures. See [this CI workflow](https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/network-test.yaml) for how it is used.

## Load modes and reported metrics

Transactions are submitted either open-loop (the default: each client fires
its whole transaction sequence as fast as the queue drains, building a deep
backlog that exercises the head's saturation throughput) or closed-loop
(`--wait-for-tx-valid`: one in-flight transaction per client, so
per-transaction times measure the true round-trip latency of a snapshot
cycle).

The benchmark starts its hydra-nodes with logging disabled (`--quiet`):
tracing serialises large event envelopes on the node's critical path and its
cost differs across compared versions, so leaving it on would distort the
comparison.

Interpret the metrics accordingly:

* In open-loop runs, per-transaction confirmation times are dominated by time
  spent queued behind the backlog; they scale with the dataset size and mostly
  restate throughput. The rate metrics are the signal there.
* In closed-loop runs, the confirmation percentiles are honest latency
  figures.

Reported metrics:

* _End-to-end TPS_: confirmed transactions over the whole run, from first
  submission to last confirmation.
* _Sustained TPS_: confirmation rate over the middle ~80% of the run, trimmed
  on snapshot boundaries so ramp-up and tail effects are excluded. Only
  reported when at least 10 snapshots were observed.
* _Backlog drain time (s)_: last confirmation minus last submission; how long
  the head needed to work through the submitted backlog.
* _Snapshots per second_ and _Avg txs per snapshot_: the decomposition of
  throughput (TPS = snapshots/s x txs/snapshot). Both are neutral measures:
  raising the node's `maxTxsPerSnapshot` moves them in opposite directions
  while improving throughput.
* _Tx validation time p50 (ms)_: median submission-to-TxValid time; a
  responsiveness signal for the node's ingest path under load.
* _Peak node RSS (MB)_: peak resident memory across the scenario's hydra-node
  processes (Linux only); guards against memory regressions under load.
* Confirmation time average and percentiles: see load modes above.

## PR comparison methodology

The CI workflow `.github/workflows/bench-e2e-diff.yaml` compares each PR
against its merge-base (not master HEAD, so re-runs keep their baseline and
other people's merges cannot appear as PR deltas):

* One job generates the three scenarios (3-node sustained load, 1-node
  `Plateau 1000` large-UTxO load, 3-node closed-loop latency) once, with
  fixed seeds, consumed by every benchmark job.
* Four benchmark jobs run in parallel; each measures BOTH sides back to back
  on its own runner (orders alternated), after prefetching both nix closures.
  GitHub's hosted fleet mixes CPU models with a large single-thread spread:
  unpaired cross-machine comparisons see double-digit CV on open-loop TPS
  even for identical code, while machine identity cancels inside a
  same-machine pair.
* `scripts/bench-e2e-diff.py` reports the median of the per-pair percent
  deltas and colors a row only beyond that metric's noise threshold with
  directional agreement across pairs. This is a calibrated heuristic, not a
  significance test, and nothing fails CI on it; strong regressions on the
  headline rates emit a `::warning` annotation.
* Nodes are spawned with `+RTS -N2 -T` (via `HYDRA_NODE_RTS_FLAGS`, guarded
  by a probe so both sides always get identical settings), the bench client
  itself runs with `+RTS -N2` (passed on the command line so both sides'
  clients get it), and the cluster runs on tmpfs, keeping scheduler
  oversubscription and network-disk fsync latency out of the measurements. The `-T` counters feed the alloc/CPU
  rows, the machine-insensitive signal to trust when wall clock wobbles.
* `workflow_dispatch` takes `head_ref`/`base_ref` to compare arbitrary refs;
  an empty `base_ref` makes it an A/A null run of `head_ref`, in which any
  colored row is a false positive. To recalibrate thresholds, download the
  `bench-results-*` artifacts of accumulated A/A runs into one directory per
  run; `scripts/bench-e2e-diff.py --calibrate <dir>` prints suggested
  per-metric values to land in the script. Draft PRs are skipped unless the
  PR carries the `bench` label (applies from the next push).

A new summary metric only shows a difference once both sides emit it, and
must be registered in the script's `METRICS` table.

