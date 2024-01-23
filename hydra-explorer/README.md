# Hydra Explorer

A small executable which connects to a chain like the `hydra-node`, but puts any
observations as traces onto `stdout`.

To run, pass a `--node-socket` and the corresponding network id. For example:

``` shell
hydra-explorer \
  --node-socket testnets/preprod/node.socket \
  --testnet-magic 1
```

Note: this assumes you are running a cardano-node in preprod.

By definition, hydra-explorer will bind port 9090.