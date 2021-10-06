This is our standard demo setup for demonstrating the Hydra Head protocol.

It consists of
- a cluster of three `hydra-node`s, directly connected to each other, each having access to one of three credentials `alice`, `bob` or `carol`
- a `mock-chain` also connecting all nodes
- a `prometheus` server gathering metrics
- ad-hoc `hydra-tui` clients to interact with the individual `hydra-node`s

# With Docker

Using [`docker-compose`](https://docs.docker.com/compose/) you can start the demo cluster with:
```sh
docker-compose pull
docker-compose up -d
```
And attach three `hydra-tui` clients to each `hydra-node` (in three terminals):

``` sh
docker run --rm -it inputoutput/hydra:hydra-tui-latest --connect localhost:4001 # alice's hydra-node
docker run --rm -it inputoutput/hydra:hydra-tui-latest --connect localhost:4002 # bob's hydra-node
docker run --rm -it inputoutput/hydra:hydra-tui-latest --connect localhost:4003 # carol's hydra-node
```

# Without Docker

One needs to start the various services "manually", eg. from the command-line.
In 3 different terminals, start 3 nodes with the following command-lines:

Alice's node:
```sh
$ cabal exec hydra-node -- --node-id 1 --port 5001 --api-port 4001 --monitoring-port 6001 \
 --peer localhost:5002 --peer localhost:5003 \
 --me demo/alice.sk --party demo/bob.vk --party demo/carol.vk \
 --mock-chain-host localhost
```

Bob's node:
```sh
$ cabal exec hydra-node -- --node-id 2 --port 5002 --api-port 4002 --monitoring-port 6002 \
 --peer localhost:5001 --peer localhost:5003 \
 --me demo/bob.sk --party demo/alice.vk --party demo/carol.vk \
 --mock-chain-host localhost
```

Carol's node:
```sh
$ cabal exec hydra-node -- --node-id 3 --port 5003 --api-port 4003 --monitoring-port 6003 \
 --peer localhost:5001 --peer localhost:5002 \
 --me demo/carol.sk --party demo/alice.vk --party demo/bob.vk \
 --mock-chain-host localhost
```

In another terminal, start mock-chain:

```sh
$ cabal exec mock-chain
```

If things go well, the nodes should start logging once they are connected to the chain.

Then from yet another set of 3 terminals, connect to the nodes using `hydra-tui`:

```sh
# replace 4001 with 4002 and 4003 to connect to the other 2 nodes
cabal exec hydra-tui -- --connect localhost:4001
```

You should now be able to `Init`ialise a Head, commit UTXOs, ...
