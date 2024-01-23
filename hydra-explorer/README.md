# Hydra Explorer

A small executable which connects to a chain like the `hydra-node`, but puts any
observations as traces onto `stdout`.

To run, pass a `--node-socket`, corresponding network id. For example:

``` shell
hydra-explorer \
  --node-socket testnets/preprod/node.socket \
  --testnet-magic 1
```
