# Commit funds to an open Head

Assuming we already have an open Head and some funds on the L1 we would like to commit.

### Demo setup

For example a demo setup could use our `hydra-cluster` binary:

```shell
cabal run hydra-cluster -- \
  --devnet \
  --publish-hydra-scripts \
  --state-directory incremental-demo
```

The following commands expect these environment variables:

```shell
export CARDANO_NODE_SOCKET_PATH=incremental-demo/node.socket
export CARDANO_NODE_NETWORK_ID=42
```

We can inspect the L1 utxo with:

```shell
cardano-cli query utxo --whole-utxo
```
and

```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file hydra-cluster/config/credentials/faucet.vk)
```

As well as the L2 and head state:
```shell
cabal run hydra-tui -- --cardano-signing-key incremental-demo/wallet.sk
```
