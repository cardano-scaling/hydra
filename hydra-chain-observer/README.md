# Hydra Chain Observer

A small executable which connects to a chain like the `hydra-node`, but puts any
observations as traces onto `stdout`.

To run, pass a `--node-socket`, corresponding network id and optionally
`--start-chain-from`. For example:

``` shell
hydra-chain-observer \
  --node-socket testnets/preprod/node.socket \
  --testnet-magic 1 \
  --start-chain-from "41948777.5d34af0f42be9823ebd35c2d83d5d879c5615ac17f7158bb9aa4ef89072455a7"
```
