# Commit funds to an open Head

Assuming we already have an open Head and some funds on the L1 we would like to commit.

### Demo setup

For example a demo setup could use our `hydra-cluster` binary:

:::caution TODO
Could also copy the faucet keys for more convenience.
:::

```shell
cabal run hydra-cluster -- \
  --devnet \
  --publish-hydra-scripts \
  --state-directory incremental-demo
```

The following commands expect these environment variables:

```shell
export CARDANO_NODE_SOCKET_PATH=${PWD}/incremental-demo/node.socket
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


In this setup, we would be using the `faucet` keys to commit everything into the head.

```shell
export WALLET_SK=${PWD}/hydra-cluster/config/credentials/faucet.sk
export WALLET_VK=${PWD}/hydra-cluster/config/credentials/faucet.vk
cd incremental-demo
```

### Deposit UTxO to commit

To observe funds owned on L2 we can use `hydra-tui`

```shell
cabal run hydra-tui -- --cardano-signing-key ${WALLET_SK}
```

The `/commit` endpoint supports two ways of specifying what to commit, one is just by showing the UTxO (which is assumed to be owned by public keys), while the more advanced way would be using [blueprint transactions](./commit-blueprint).

We are using the simple request here and want to commit everything owned by `${WALLET_SK}`. So we can just query the L1 for all UTxO owned by it:
```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${WALLET_VK}) \
  --out-file commit-utxo.json
```

Then a request to the `/commit` endpoint provides us with a transaction:

:::danger FIXME
Should be able to do just this:
```shell
curl -X POST localhost:4001/commit \
  --data @commit-utxo.json
```
:::

```shell
jq '{utxo: .}' commit-utxo.json | \
  curl -X POST localhost:4001/commit --data @- \
  > deposit-tx.json
```

Which we can submit to the cardano network:
```shell
cardano-cli transaction sign \
  --tx-file deposit-tx.json \
  --signing-key-file ${WALLET_SK} \
  --out-file deposit-tx.signed.json

cardano-cli transaction submit \
  --tx-file deposit-tx.signed.json
```

This will result in a deposit being detected by the `hydra-node` and consequently the funds to be committed to the head.
