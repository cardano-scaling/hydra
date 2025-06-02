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

## Smoke Testing

The `hydra-cluster` executable spins up a `cardano-node` as a network
participant which synchronizes the block chain and then executes a
single scenario (single party, full life cycle) using funds available
to the `config/credentials/faucet.sk` on that network. The Hydra nodes
can reference pre-existing contracts living at some well-known
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

```sh
cabal run bench-e2e -- single --output-directory out"
bench/plot.sh out
```

Which will produce an output like:

```
Generating dataset with scaling factor: 10
Writing dataset to: out/dataset.json
Test logs available in: out/test.log
Starting benchmark
Seeding network
Fund scenario from faucet
Fuel node key "16e61ed92346eb0b0bd1c6d8c0f924b4d1278996a61043a0a42afad193e5f3fb"
Publishing hydra scripts
Starting hydra cluster in out
Initializing Head
Committing initialUTxO from dataset
HeadIsOpen
Client 1 (node 0): 0/300 (0.00%)
Client 1 (node 0): 266/300 (88.67%)
All transactions confirmed. Sweet!
Closing the Head
Finalizing the Head
Writing results to: out/results.csv
Confirmed txs/Total expected txs: 300/300 (100.00 %)
Average confirmation time (ms): 18.747147496
P99: 23.100851369999994ms
P95: 19.81722345ms
P50: 18.532922ms
Invalid txs: 0
Writing report to: out/end-to-end-benchmarks.md
         line 0: warning: Cannot find or open file "out/system.csv"                
Created plot: out/results.png
```

Note that if it's present in the environment, benchmark executable will gather basic system-level statistics about the RAM, CPU, and network bandwidth used. The `plot.sh` script then displays those alongside tx confirmation time in a single graph.

The benchmark can be run in three modes:

* `single`: Generate a single _dataset_ and runs the benchmark with it.
* `datasets`: Runs one or more pre-existing _datasets_ in sequence and collect their results in a single markdown formatted file. This is useful to track the evolution of hydra-node's performance over some well-known datasets over time and produce a human-readable summary.
* `demo`: Generates transactions against an already running network of cardano and hydra nodes. This can serve as a workload when testing network-resilience scenarios, such as packet loss or node failures. See [this CI workflow](https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/network-test.yaml) for how it is used.

