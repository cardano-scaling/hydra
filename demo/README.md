This is our standard demo setup for demonstrating the Hydra Head protocol.

It consists of
- a cluster of three `hydra-node`s, directly connected to each other, each having access to one of three Hydra credentials `alice`, `bob` or `carol`
- a single `cardano-node` block producer which is used as a local devnet
- a `prometheus` server gathering metrics
- ad-hoc `hydra-tui` clients to interact with the individual `hydra-node`s

IMPORTANT: As we are using a ad-hoc private devnets which start from the genesis
block, you need to ensure the devnet configuration is reasonably up-to-date. If
you get `LedgerNoView` errors from the `cardano-node`, that means the start
times are too far in the past and you should update them e.g. using the
`prepare-devnet.sh` script.

# Building Docker Images

The CI automatically pushes docker images built from latest `master` branch to a
[Github packages](https://github.com/orgs/input-output-hk/packages?repo_name=hydra-poc), hence
running the demo only requires pulling those images.

To build the images locally, do:

```sh
docker-compose build
```

# Starting Network

## With Docker

Using [`docker-compose`](https://docs.docker.com/compose/) and the [`run-docker.sh`](./run-docker.sh) script, one can start the demo cluster with:
```sh
docker-compose pull
./run-docker.sh -d
```

NOTE: You can query the `cardano-node` using the same container, e.g.

``` sh
docker-compose exec cardano-node cardano-cli query utxo --testnet-magic 42 --whole-utxo
```

## Without Docker

One needs to prepare a `cardano-node` (devnet) and `hydra-node`s "manually".
These instructions assume you have both built and in scope for `cabal exec`.

First, let's prepare and start an ad-hoc, single `cardano-node` devnet using our
configuration. Note that this will create a `devnet` directory in your current
working directory:

```sh
./prepare-devnet.sh
cd devnet
cabal exec cardano-node -- run \
  --config cardano-node.json \
  --topology topology.json \
  --database-path db \
  --socket-path ipc/node.socket \
  --shelley-operational-certificate credentials/opcert1.cert \
  --shelley-kes-key credentials/delegate1.kes.skey \
  --shelley-vrf-key credentials/delegate1.vrf.skey
```

Then in 3 different terminals, start 3 `hydra-node`s from the `demo/` directory:

Alice's node:
```sh
cabal exec hydra-node -- \
  --node-id 1 --port 5001 --api-port 4001 --monitoring-port 6001 \
  --peer localhost:5002 \
  --peer localhost:5003 \
  --hydra-signing-key alice.sk \
  --hydra-verification-key bob.vk \
  --hydra-verification-key carol.vk \
  --cardano-signing-key devnet/credentials/alice.sk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet 42 \
  --node-socket devnet/ipc/node.socket
```

Bob's node:
```sh
cabal exec hydra-node -- \
  --node-id 2 --port 5002 --api-port 4002 --monitoring-port 6002 \
  --peer localhost:5001 \
  --peer localhost:5003 \
  --hydra-signing-key bob.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key carol.vk \
  --cardano-signing-key devnet/credentials/bob.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet 42 \
  --node-socket devnet/node.socket
```

Carol's node:
```sh
cabal exec hydra-node -- \
  --node-id 3 --port 5003 --api-port 4003 --monitoring-port 6003 \
  --peer localhost:5001 \
  --peer localhost:5002 \
  --hydra-signing-key carol.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key bob.vk \
  --cardano-signing-key devnet/credentials/carol.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet 42 \
  --node-socket devnet/node.socket
```

If things go well, the nodes should start logging once they are connected to the chain.


# Seeding Network

In the current stage of development, Hydra nodes need a specially crafted set of UTXO to drive the Head protocol ("fuel") and of course some UTXO to be committed to the head.

## With Docker

The included script `seed-devnet.sh` uses the `cardano-cli` in the already running `cardano-node` container to give Alice, Bob and Carol some UTXO to commit and some "fuel" UTXO.

NOTE: There is nothing special about those transactions so one could any other Cardano client to create those transactions. This transaction must match the following characteristics:
* It must pay outputs to commit to the key that's used by the Hydra Node's internal wallet, as defined by argument `--cardano-signing-key` of `hydra-node` executable,
* One of the outputs must include datum hash `a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3` as this is our "fuel" marker.

## Without Docker

You can use the `seed-devnet.sh` script by passing it the path to a `cardano-cli` executable to use instead of having it using the docker container, e.g.

``` sh
./seed-devnet.sh $(which cardano-cli)
```

# Running clients

## With Docker

Using [`docker-compose`](https://docs.docker.com/compose/) you can start the demo _Terminal-based User Interface_ aka. `hydra-tui` to interact with Hydra nodes.

There are 3 preconfigured TUIs to be run using `docker-compose`, `hydra-tui-1`, `hydra-tui-2` and `hydra-tui-3`.
To connect to the first Hydra node in a terminal, run the following commands:

``` sh
cd demo
docker-compose --profile tui run hydra-tui-1
```

## Without docker

Connect to the nodes using `hydra-tui`. For example to use Alice's `hydra-node` and her on-chain
credentials:

```sh
cabal exec hydra-tui -- \
  --connect localhost:4001 \
  --cardano-signing-key devnet/credentials/alice.sk \
  --testnet 42 \
  --node-socket devnet/ipc/node.socket
```

Replace port `4001` with `4002` or `4003` to connect to the other 2 nodes and
`alice.sk` with `bob.sk` or `carol.sk` respectively.

You should now be able to `[I]nit`ialise a Head, `[c]ommit` UTXOs, ...
