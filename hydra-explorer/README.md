# Hydra Explorer

A small executable which connects to a chain like the `hydra-node`, but puts any
observations as traces onto `stdout`.

It supports two modes of operation: **Direct** connection to a node via socket, and connection through **Blockfrost** API.

By definition, hydra-explorer will bind port 9090.

## Direct Mode

To run from the tip, just pass a `--node-socket` and the corresponding network id. For example:

```shell
hydra-explorer direct \
  --node-socket testnets/preprod/node.socket \
  --testnet-magic 1
```

Note: this assumes you are running a cardano-node in preprod.

## Blockfrost Mode

To run from the tip, just pass a `--project-path`. For example:

```shell
hydra-explorer blockfrost \
  --project-path .vscode/blockfrost/project_token_hash
```
