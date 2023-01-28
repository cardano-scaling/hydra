## Executable

This package contains the same-named `hydra-cluster` executable, which can be
used for "smoke testing" the `hydra-node` against an existing, well-known
Cardano network.

It would spin up a `cardano-node` as a network participant, synchronize the
block chain and execute a single scenario (single party, full life cycle) using
funds available to the `config/credentials/faucet.sk` on that network.

The `hydra-cluster` requires `hydra-node` and `cardano-node` to be in scope. You could use a
special nix shell containing `hydra-node` and `hydra-cluster` executables:

```sh
nix develop .#exes
```

Or use an alias:

```sh
cabal build hydra-node
alias hydra-node=$(cabal exec which -- hydra-node)
```

Then, to run the smoke test against the official cardano testnet using a local
`state-testnet` directory (to re-use the synchronized chain db):

```sh
hydra-cluster --preview --state-directory state-testnet
```

## Local devnet

It's possible to use `hydra-cluster` to run a local cardano devnet in the form of a single
block producer with a faster production rate than normal. This is useful in situations where
one wants to test Hydra-based DApps, eg. applications and services using Hydra.

```sh
HYDRA_CONFIG_DIR=hydra-cluster/config hydra-cluster --devnet --state-directory my-test
```

The `HYDRA_CONFIG_DIR` environment variable is used to control where the executable will look
for its configuration files. By default those files are resolved usign the cabal package's
data structure which is not always convenient.

Adding ` --publish-hydra-scripts` argument will ensure Hydra validator scripts are published
on the network and available as reference inputs for hydra-node.

## Test suite

The `hydra-cluster:test:integration` test suite runs multiple scenarios on a
local cluster of `hydra-node`s connected to a local Cardano "devnet".

This "devnet" is a trivial Cardano network comprised by only a single block
producing node, having all the stake. This is of course not exactly how a proper
Cardano network would work, but the scope of this package and it's tests are to
assert correct hydra-node behavior in a cluster of Hydra nodes.

Run the integration test suite with `cabal test`

## Benchmarks

The benchmark can be run using `cabal bench` and produces a `results.csv` file. To plot the transaction confirmation times you can use the `bench/plot.sh` script, passing it the directory containing the benchmark's results.

To run and plot results of the benchmark:

```sh
$ cabal bench
Running 1 benchmarks...
Benchmark bench-e2e: RUNNING...
Writing transactions to: /run/user/1000/bench-83d18973f95a554d/txs.json
[...]
Writing results to: /run/user/1000/bench-6b772589d08f82a5/results.csv
Benchmark bench-e2e: FINISH
$ bench/plot.sh /run/user/1000/bench-6b772589d08f82a5/results.csv
Created plot: /run/user/1000/bench-6b772589d08f82a5/results.png
```

The benchmark provides several options for running, check `cabal bench --benchmark-options --help` for details.

See [this README](./bench/README.md) for more details on benchmark run results interpretation.
