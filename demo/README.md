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

# With Docker

Using [`docker-compose`](https://docs.docker.com/compose/) you can start the demo cluster with:
```sh
docker-compose pull
./prepare-devnet.sh
docker-compose up -d
```
And attach three `hydra-tui` clients to each `hydra-node` (in three terminals):

``` sh
docker run --rm -it inputoutput/hydra:hydra-tui-latest --connect localhost:4001 # alice's hydra-node
docker run --rm -it inputoutput/hydra:hydra-tui-latest --connect localhost:4002 # bob's hydra-node
docker run --rm -it inputoutput/hydra:hydra-tui-latest --connect localhost:4003 # carol's hydra-node
```

NOTE: You can query the `cardano-node` using the host-mounted socket file in
`devnet/ipc/node.socket` (requires write permissions), e.g.

``` sh
sudo chmod a+w devnet/ipc/node.socket
export CARDANO_NODE_SOCKET_PATH=devnet/ipc/node.socket
cardano-cli query utxo --testnet-magic 42 --whole-utxo
```

# Without Docker

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
  --socket-path node.socket \
  --shelley-operational-certificate credentials/opcert1.cert \
  --shelley-kes-key credentials/delegate1.kes.skey \
  --shelley-vrf-key credentials/delegate1.vrf.skey
```

Then in 3 different terminals, start 3 `hydra-node`s:

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
  --network-magic 42 \
  --node-socket devnet/node.socket
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
  --network-magic 42 \
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
  --network-magic 42 \
  --node-socket devnet/node.socket
```

If things go well, the nodes should start logging once they are connected to the chain.

Then from yet another set of 3 terminals, connect to the nodes using `hydra-tui`:

```sh
# replace 4001 with 4002 and 4003 to connect to the other 2 nodes
cabal exec hydra-tui -- --connect localhost:4001
```

You should now be able to `Init`ialise a Head, commit UTXOs, ...
