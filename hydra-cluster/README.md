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

For the benchmarks, you can choose between generating either a constant-size
UTxO set or a growing UTxO set.

Constant UTxO set:
Each transaction spends one input and creates exactly one new output (1-in-1-out), so the total number of
UTxOs in the set remains roughly the same over time.

Growing UTxO set:
Each transaction spends one input but creates two outputs gradually increasing the total number of UTxOs as more
transactions are processed. For this we use the `--number-of-txs` argument.

This distinction allows you to measure performance under different realistic UTxO-set growth scenarios on Cardano.


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
         line 0: warning: Cannot find or open file "out/system.csv"
Created plot: out/results.png
```

Note that if it's present in the environment, benchmark executable will gather basic system-level statistics about the RAM, CPU, and network bandwidth used. The `plot.sh` script then displays those alongside tx confirmation time in a single graph.

The benchmark can be run in three modes:

* `single`: Generate a single _dataset_ and runs the benchmark with it.
* `datasets`: Runs one or more pre-existing _datasets_ in sequence and collect their results in a single markdown formatted file. This is useful to track the evolution of hydra-node's performance over some well-known datasets over time and produce a human-readable summary.
* `demo`: Generates transactions against an already running network of cardano and hydra nodes. This can serve as a workload when testing network-resilience scenarios, such as packet loss or node failures. See [this CI workflow](https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/network-test.yaml) for how it is used.

