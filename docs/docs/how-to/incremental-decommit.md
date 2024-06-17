---
sidebar_position: 3
---

# Decommit funds
> This example assumes you have the hydra-node repo at your disposal together with `hydra-node`, `hydra-tui`, `cardano-cli` and `curl` binaries.

To take out some `UTxO` present in an already open Head and send it back to the layer one, you need to use the `Decommit` command of the WebSocket API.

To do this we need to find out which `UTxO` we can spend.

In this example we will use Alice and her external wallet key so first let's find out the address:
```shell
cardano-cli address build \
  --payment-verification-key-file credentials/alice-funds.vk \
  --testnet-magic 42 \
  --out-file credentials/alice-funds.addr
```

output:
```shell
addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z
```

Next, let's query the state of Alice's UTxO available in the head:

```shell
curl localhost:4001/snapshot/utxo \
  | jq "to_entries \
  | map(select(.value.address == \"$(cat credentials/alice-funds.addr)\")) \
  | from_entries" \
  > utxo.json
```

output:
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

Certainly! Here is your markdown with the shell command included as part of the note:

> We can also do this by querying the state of Alice's funds in the `websocat` session:
> ```shell
> websocat -U "ws://0.0.0.0:4001?history=no" \
>   | jq "select(.tag == \"Greetings\") \
>   | .snapshotUtxo \
>   | with_entries(select(.value.address == \"$(cat credentials/alice-funds.addr)\"))" \
>   > utxo.json
```

Now we need another key to send funds to and construct a decommit
transaction which will be sent to the `hydra-node` to initiate the process of
sending funds back to the layer one.

Let's create new wallet key pair and address:

```shell
cardano-cli address key-gen \
  --verification-key-file credentials/wallet.vk \
  --signing-key-file credentials/wallet.sk

cardano-cli address build \
  --verification-key-file credentials/wallet.vk \
  --testnet-magic 42 \
  --out-file credentials/wallet.addr
```

Let's pick the first `UTxO` which has total of 5 ADA available and use it to decommit.
> We rely on `hydra-node` to balance the transaction.
Using the `UTxO` we queried before, we need to construct and sign a decommit transaction.

```shell
LOVELACE=$(jq -r 'to_entries[0].value.value.lovelace' < utxo.json)
cardano-cli transaction build-raw \
  --tx-in $(jq -r 'to_entries[0].key' < utxo.json) \
  --tx-out $(cat credentials/wallet.addr)+${LOVELACE} \
  --fee 0 \
  --out-file decommit.json

cardano-cli transaction sign \
  --tx-file decommit.json \
  --signing-key-file credentials/alice.sk \
  --signing-key-file credentials/alice-funds.sk \
  --out-file alice-decommit-tx-signed.json
```

With the signed decommit transaction, now we can submit it to the `hydra-node`:

```shell
curl -X POST 127.0.0.1:4001/decommit \
  --data @alice-decommit-tx-signed.json
```

> We can also do this by sending the `Decommit` client input to the `websocat` session:
> ```shell
> cat alice-decommit-tx-signed.json \
>   | jq -c '{tag: "Decommit", decommitTx: .}' \
>   | websocat "ws://127.0.0.1:4001?history=no"
```

If you haven't already open a websocat session (`websocat ws://0.0.0.0:4001`).
There you will see a `DecommitRequested` message which indicates a decommit is requested.
After some time a `DecommitFinalized` can be observed which concludes the decommit process and after which the funds are available on the layer one.

To confirm, you can query the funds of the wallet on the layer one:

```shell
cardano-cli query utxo \
  --socket-path node.socket \
  --address $(cat credentials/wallet.addr) \
  --testnet-magic 42 \
  --output-json \
  | jq
```
