---
sidebar_position: 6
---

# Decommit funds

To take out some `UTXO` present in an open head and send it back to layer 1, you need to do a so-called `decommit`.

This how-to assumes that we are in a similar situation as in the [Getting started](../getting-started) or [Testnet tutorial](../tutorial). Depending on who owns something in your head, you might need to update the instructions of this tutorial. In our example, we decommit funds owned by Alice from their address:

```shell
export WALLET_SK=credentials/alice-funds.sk
export WALLET_ADDR=addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z
```

First, we need to find out which `UTXO` we can spend using our address:

```shell
curl localhost:4001/snapshot/utxo \
  | jq "with_entries(select(.value.address == \"${WALLET_ADDR}\"))" \
  > utxo.json
```

<details>
<summary> Example output</summary>

```json
{
  "f6b004be1cf95dbd3d0abc3daceac40ef6401e502972a919e5e52564b9f5740b#0": {
    "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
    "datum": null,
    "datumhash": null,
    "inlineDatum": null,
    "referenceScript": null,
    "value": {
      "lovelace": 50000000
    }
  },
  "f6b004be1cf95dbd3d0abc3daceac40ef6401e502972a919e5e52564b9f5740b#1": {
    "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
    "datum": null,
    "datumhash": null,
    "inlineDatum": null,
    "referenceScript": null,
    "value": {
      "lovelace": 50000000
    }
  }
}
```

</details>

Now, the `decommit` command requires us to build a transaction that proves we can spend what we want to decommit. The outputs of this transaction will be the outputs that are also going to be made available on the main chain.

For example, to spend the first UTXO queried above in a transaction sending the same value to Alice's key (so she can spend it on layer 1 later):

```shell
LOVELACE=$(jq -r 'to_entries[0].value.value.lovelace' < utxo.json)
cardano-cli conway transaction build-raw \
  --tx-in $(jq -r 'to_entries[0].key' < utxo.json) \
  --tx-out ${WALLET_ADDR}+${LOVELACE} \
  --fee 0 \
  --out-file decommit.json
```

You can inspect the transaction with

```shell
cardano-cli conway transaction view --tx-file decommit.json
```

As the transaction spends from Alice's funds in the Hydra head, we also need to
sign it with her key:

```shell
cardano-cli conway transaction sign \
  --tx-file decommit.json \
  --signing-key-file ${WALLET_SK} \
  --out-file alice-decommit-tx-signed.json
```

With the signed decommit transaction, we can submit it to the `/decommit` endpoint:

```shell
curl -X POST 127.0.0.1:4001/decommit \
  --data @alice-decommit-tx-signed.json
```

<details>
<summary>Alternative using websocket</summary>

We can also submit a `Decommit` client input using a WebSocket:
```shell
cat alice-decommit-tx-signed.json \
  | jq -c '{tag: "Decommit", decommitTx: .}' \
  | websocat "ws://127.0.0.1:4001?history=no"
```

</details>

If you haven't already, open a WebSocket session using `websocat ws://0.0.0.0:4001` now.

In the message history, you will see a `DecommitRequested` message which
indicates a decommit is requested.

After some time, a `DecommitFinalized` can be observed, which concludes the decommit process and makes the funds available on layer 1.

To confirm, you can query the funds of the wallet on layer 1 from a `cardano-node`:

```shell
cardano-cli query utxo \
  --address ${WALLET_ADDR} \
  --output-json | jq
```
