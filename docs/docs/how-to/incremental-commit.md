---
sidebar_position: 4
---

# Commit funds to an open Head

Assuming we already have an open Head and some funds on the L1 we would like to commit.


:::info
You could run a local [demo](./../getting-started)
:::

The following commands are expected to be run from the `demo` folder and using these environment variables:

```shell
export CARDANO_NODE_SOCKET_PATH=${PWD}/devnet/node.socket
export CARDANO_NODE_NETWORK_ID=42
```

We can inspect the L1 utxo with:

```shell
cardano-cli query utxo --whole-utxo
```
and the state of the faucet public key

```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${PWD}/../hydra-cluster/config/credentials/faucet.vk)
```


In this setup, we would be using the `faucet` keys to commit everything into the head.

```shell
export WALLET_SK=${PWD}/../hydra-cluster/config/credentials/faucet.sk
export WALLET_VK=${PWD}/../hydra-cluster/config/credentials/faucet.vk
```

### Deposit UTxO to commit

The `/commit` endpoint supports two ways of specifying what to commit, one is just by showing the UTxO (which is assumed to be owned by public keys), while the more advanced way would be using [blueprint transactions](./commit-blueprint).

We are using the simple request here and want to commit everything owned by `${WALLET_SK}`. So we can just query the L1 for all UTxO owned by it:
```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${WALLET_VK}) \
  --out-file commit-utxo.json
```

:::info
You can also specify `amount` of lovelace you want to commit together with the `UTxO` and hydra-node would
commit only the specified amount and return any leftover to the user address.
:::

Then a request to the `/commit` endpoint provides us with a transaction:

```shell
curl -X POST localhost:4001/commit \
  --data @commit-utxo.json \
  > deposit-tx.json
```

Which we can submit to the cardano network:
```shell
cardano-cli conway transaction sign \
  --tx-file deposit-tx.json \
  --signing-key-file ${WALLET_SK} \
  --out-file deposit-tx.signed.json

cardano-cli conway transaction submit \
  --tx-file deposit-tx.signed.json
```

This will result in a deposit being detected by the `hydra-node` and consequently the funds to be committed to the head.

### Recover a deposit

Do the same thing as above, **but** with one node stopped, so the deposit is not going to be picked up.

Once we deposited funds we should not see the corresponding UTxO belonging to faucet public key on L1:

```shell

cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${PWD}/../hydra-cluster/config/credentials/faucet.vk)
```


Inspect the pending deposits:

```
curl -X GET localhost:4001/commits

```
and you should see the tx-id of the deposit transaction `["6b51f3787f5482004b258c60fe0c94775164f547d9284b6233bbb4f6f8b9dfa6"]`

To recover, we can use the `/commits` endpoint again using the transaction id of the deposit:

```shell
curl -X DELETE localhost:4001/commits/$(printf "\"6b51f3787f5482004b258c60fe0c94775164f547d9284b6233bbb4f6f8b9dfa6"\" | jq -sRr '@uri')
```

If we inspect the faucet funds again we will see that the locked deposit is now recovered

```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${PWD}/../hydra-cluster/config/credentials/faucet.vk)
```

