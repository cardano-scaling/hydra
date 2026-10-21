---
sidebar_position: 6
---

# Decommit funds

To take out some `UTXO` present in an open head and send it back to layer 1, you need to do a so-called `decommit`.

This how-to assumes that we are in a similar situation as in the [Getting started](../getting-started) or [Testnet tutorial](../tutorial). Depending on who owns something in your head, you might need to update the instructions of this tutorial. In our example, we decommit funds owned by Alice from their address:

```shell
export WALLET_SK=credentials/alice-funds.sk
export WALLET_ADDR=$(cat credentials/alice-funds.addr)
```

Use the key and address files from the testnet tutorial. For the local demo,
use the corresponding files under `devnet/credentials/` and derive the address
from `alice-funds.vk`. Keep `CARDANO_NODE_SOCKET_PATH` and
`CARDANO_NODE_NETWORK_ID` set for the matching L1 network.

Query the ADA-only UTxO owned by this address:

```shell
curl --fail-with-body -sS http://127.0.0.1:4001/snapshot/utxo \
  | jq --arg address "$WALLET_ADDR" \
    'with_entries(select(.value.address == $address and (.value.value | keys) == ["lovelace"]))' \
  > utxo.json
TX_IN=$(jq -er 'keys[0] // error("No ADA-only UTxO at this address")' utxo.json)
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

For example, spend the selected UTxO and send its full value to Alice's address on L1:

```shell
LOVELACE=$(jq -er --arg input "$TX_IN" '.[$input].value.lovelace' utxo.json)
cardano-cli conway transaction build-raw \
  --tx-in "$TX_IN" \
  --tx-out "$WALLET_ADDR+$LOVELACE" \
  --fee 0 \
  --out-file decommit.json
```

You can inspect the transaction with

```shell
cardano-cli debug transaction view --tx-file decommit.json
```

As the transaction spends from Alice's funds in the Hydra head, we also need to
sign it with her key:

```shell
cardano-cli conway transaction sign \
  --tx-file decommit.json \
  --signing-key-file "$WALLET_SK" \
  --out-file alice-decommit-tx-signed.json
```

Before submitting, open a WebSocket connection in another terminal:

```shell
websocat 'ws://127.0.0.1:4001?history=no'
```

With the signed decommit transaction, we can submit it to the `/decommit` endpoint:

```shell
curl --fail-with-body -sS http://127.0.0.1:4001/decommit \
  -H 'Content-Type: application/json' \
  --data-binary @alice-decommit-tx-signed.json
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

The connection receives `DecommitRequested`, followed by approval and
`DecommitFinalized` as the request settles. If you connect after submission,
use `?history=yes` to replay earlier events; history is opt-in.

The HTTP request waits for a result and can return a pending response (202).
A rejection requires inspecting the response before retrying. In particular,
wait for an in-flight deposit to settle or expire when receiving
`DepositInFlight`; a node that is unsynchronized or has a stalled broadcast
can reject new work with HTTP 503.

A decommit must have at least one output. Its outputs must meet L1 minimum
ADA and size limits, even when L2 fees are zero. This example assumes the
selected input contains only ADA.

After some time, a `DecommitFinalized` can be observed, which concludes the decommit process and makes the funds available on layer 1.

To confirm, you can query the funds of the wallet on layer 1 from a `cardano-node`:

```shell
cardano-cli query utxo \
  --address "$WALLET_ADDR" \
  --output-json | jq
```
