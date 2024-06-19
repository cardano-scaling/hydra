---
sidebar_position: 2
---

# Submit a transaction

This section describes how to submit a transaction to an already open head using the `NewTx` command of the WebSocket API.

First, query the UTXO available in the head:

```
curl localhost:4001/snapshot/utxo | jq
```

Below is an example response:

```json title="Example response of GET /snapshot/utxo"
{
  "8690d7618bb88825d6ec7cfbe2676779b8f4633cb137a1c12cd31b4c53f90f32#0": {
    "address": "addr_test1vrdhewmpp96gv6az4vymy80hlw9082sjz6rylt2srpntsdq6njxxu",
    "datum": null,
    "datumhash": null,
    "inlineDatum": null,
    "referenceScript": null,
    "value": {
      "lovelace": 100000000
    }
  }
}
```

Assuming the single UTXO is owned by `some-payment-key.sk` and you want to send all of it to another address, you can use `cardano-cli` (or your preferred transaction builder) to construct and sign a transaction:

```shell title="Transaction building"
cardano-cli transaction build-raw \
  --babbage-era \
  --tx-in 8690d7618bb88825d6ec7cfbe2676779b8f4633cb137a1c12cd31b4c53f90f32#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
  --fee 0 \
  --out-file tx.json

cardano-cli transaction sign \
  --tx-body-file tx.json \
  --signing-key-file some-payment-key.sk \
  --out-file tx-signed.json

cat tx-signed.json | jq -c '{tag: "NewTx", transaction: .}'
```

This command generates a message suitable for submission to the `hydra-node` via a WebSocket connection. If `hydra-node` operates on the default port `4001`, the message can be submitted using `websocat`:

```shell
cat tx-signed.json | jq -c '{tag: "NewTx", transaction: .}' | websocat "ws://127.0.0.1:4001?history=no"
```

The transaction will be validated by all connected `hydra-node` instances. It will result in either a `TxInvalid` message, providing a reason for rejection, or a `TxValid` message followed by a `SnapshotConfirmed`, updating the UTXO available in the head shortly after that.
