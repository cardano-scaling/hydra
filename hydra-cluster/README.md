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
end-to-end tests. Possible values are _direct_ and _blockfrost_ (eg. export HYDRA_BACKEND="blockfrost").
If this env variable is not set, the tests will default to _direct_ backend.

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
transaction or can post a new transaction to use those contracts.

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

To run and plot results of the benchmark:

 - Generate the dataset

```sh
cabal run bench-e2e -- dataset --number-of-txs 10 --output-directory 1"
```

 - Run the generated dataset

```sh
cabal run bench-e2e -- standalone 1/dataset.json --output-directory out"
./hydra-cluster/bench/plot.sh out
```

Which will produce an output like:

```
Reading dataset from: 1/dataset.json
Running benchmark with datasets: ["1/dataset.json"]
Test logs available in: out/test.log
Starting benchmark
Seeding network
Fund scenario from faucet
Fuel node key "92caede6c58affa96718ab4f47bb34639c135df3a7428aa118b13f25236c02e9"
Fuel node key "17a705d22d4ee258400067ee7c8c3a314513f24c6271c8524e085049d1fdd449"
Fuel node key "9951c3506f6f56e3d1871c8a2a0e88e61d32593663f9585e10d3da93b9caec87"
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
Average confirmation time (ms): 60.917365233
P99: 74.32681356ms
P95: 72.72738555ms
P50: 62.208124ms
Invalid txs: 0
Fanout outputs: 3
Writing report to: out/end-to-end-benchmarks.md

./hydra-cluster/bench/plot.sh out-standalone
         line 0: warning: Cannot find or open file "out-standalone/system.csv"
Created plot: out-standalone/results.png
```

Note that if it's present in the environment, benchmark executable will gather basic system-level statistics about the RAM, CPU, and network bandwidth used. The `plot.sh` script then displays those alongside tx confirmation time in a single graph.

The benchmark can be run in three modes:

* `standalone`: Benchmark a single or multiple _datasets_.
* `dataset`: Generates a _dataset_. This is useful to track the evolution of hydra-node's performance over some well-known datasets over time and produce a human-readable summary.
* `demo`: Generates transactions against an already running network of cardano and hydra nodes. This can serve as a workload when testing network-resilience scenarios, such as packet loss or node failures. See [this CI workflow](https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/network-test.yaml) for how it is used.

