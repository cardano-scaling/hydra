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
nix develop .?submodules=1#exes
```

Or use an alias:

```sh
cabal build hydra-node
alias hydra-node=$(cabal exec which -- hydra-node)
```

The `HYDRA_CONFIG_DIR` environment variable is used to control where the executable will look
for its configuration files. By default those files are resolved using the cabal package's
data structure which is not always convenient.

## Smoke Testing

The `hydra-cluster` executable spins up a `cardano-node` as a network
participant which synchronizes the block chain and then executes a
single scenario (single party, full life cycle) using funds available
to the `config/credentials/faucet.sk` on that network. The Hydra nodes
can reference pre-existing contracts living at some well-known
transaction or can post a new transaction to use those contracts.

To run the smoke test against the official cardano testnet using a
local `state-testnet` directory (to re-use the synchronized chain db):

```sh
hydra-cluster --preview --state-directory state-testnet
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
