# Hydra Chain Observer

A lightweight executable designed to connect to a blockchain, such as the `hydra-node`, and streams chain observations as traces to `stdout`.
It supports two modes of operation: **Direct** connection to a node via socket, and connection through **Blockfrost** API.

## Direct Mode

To run the observer in Direct Mode, provide the following arguments:
- `--node-socket`: path to the node socket file.
- network id: `--testnet-magic` (with magic number) for the testnet or `--mainnet` for the mainnet.
- (optional) `--start-chain-from`: specify a chain point (SLOT.HEADER_HASH) to start observing from.

For example:

``` shell
hydra-chain-observer direct \
  --node-socket testnets/preprod/node.socket \
  --testnet-magic 1 \
  --start-chain-from "41948777.5d34af0f42be9823ebd35c2d83d5d879c5615ac17f7158bb9aa4ef89072455a7"
```


## Blockfrost Mode

To run the observer in Blockfrost Mode, provide the following arguments:
- `--project-path`: file path to your Blockfrost project API token hash.
> expected to be prefixed with environment (e.g. testnetA3C2E...)
- (optional) `--start-chain-from`: specify a chain point (SLOT.HEADER_HASH) to start observing from.

For example:

``` shell
hydra-chain-observer blockfrost \
  --project-path project_token_hash \
  --start-chain-from "41948777.5d34af0f42be9823ebd35c2d83d5d879c5615ac17f7158bb9aa4ef89072455a7"
```

