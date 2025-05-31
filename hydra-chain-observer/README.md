# Hydra Chain Observer

A lightweight executable designed to connect to a blockchain, such as the `hydra-node`, and streams chain observations as traces to `stdout`.

It supports two modes of operation: **Direct** connection to a node via socket, and connection through **Blockfrost** API.

In both modes, reporting observations to a [`hydra-explorer`](https://github.com/cardano-scaling/hydra-explorer) can be enabled.

## Direct Mode

To run the observer directly connected to a `cardano-node`, use the `--node-socket` option and specify the network id via `--mainnet` or `--testnet-magic`. Optionally, you can specify a starting point to observe using `--start-chain-from`:

``` shell
hydra-chain-observer \
  --node-socket testnets/preprod/node.socket \
  --testnet-magic 1 \
  --start-chain-from "41948777.5d34af0f42be9823ebd35c2d83d5d879c5615ac17f7158bb9aa4ef89072455a7"
```

## Blockfrost Mode

To run a chain observer using [blockfrost](blockfrost.io), use the `--blockfrost-project-path` option to point to a file containing your Blockfrost project API token (e.g. testnetA3C2E...). Optionally, you can specify a starting point to observe using `--start-chain-from`:

``` shell
hydra-chain-observer blockfrost \
  --blockfrost-project-path $PROJECT_TOKEN_PATH \
  --start-chain-from "41948777.5d34af0f42be9823ebd35c2d83d5d879c5615ac17f7158bb9aa4ef89072455a7"
```

## Report to hydra-explorer

Using the `--explorer` argument we can specify a hostname / port for a `hydra-explorer` instance to report observations to. For example using a `direct` observer:

``` shell
hydra-chain-observer \
  --node-socket testnets/preview/node.socket \
  --testnet-magic 2 \
  --start-chain-from "49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee" \
  --explorer http://0.0.0.0:8080
```
