---
sidebar_position: 1
---

# Commit using a blueprint

This guide provides a walkthrough on using `cardano-cli` to assemble the necessary components for committing funds to a `head` using a blueprint transaction.

**Prerequisites**

You should have access to:

- `hydra-node` repository
- `hydra-tui`
- `cardano-cli`, and
- `curl` binaries.

## Step 1
You can use `cardano-cli` to create a _blueprint_ transaction from some `UTXO` you own. First, initiate a Cardano-node on the pre-production network:

 ```shell
 ./testnets/cardano-node.sh ~/code/hydra/testnets/preprod
 ```

## Step 2
Determine which `UTXO` you intend to commit to the `head`. This example uses Alice's external wallet key to identify her address:

 ```shell
 cardano-cli address build \
  --payment-verification-key-file hydra-cluster/config/credentials/alice-funds.vk \
  --testnet-magic 1

addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z
 ```

Next, query to see what `UTXO` Alice has:

```shell
cardano-cli query utxo \
   --socket-path testnets/preprod/node.socket \
   --address addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z \
   --testnet-magic 1 \
   --output-json

{
    "14ab373afb1112d925b0f6a84518ac26d4a8cfcc99231e1f47e6996182e843a9#0": {
        "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
        "datum": null,
        "datumhash": null,
        "inlineDatum": null,
        "referenceScript": null,
        "value": {
            "lovelace": 8000000
        }
    },
    "14ab373afb1112d925b0f6a84518ac26d4a8cfcc99231e1f47e6996182e843a9#1": {
        "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
        "datum": null,
        "datumhash": null,
        "inlineDatum": null,
        "referenceScript": null,
        "value": {
            "lovelace": 1828427
        }
    },
}
```

## Step 3
Select the first `UTXO`, which has eight ada available. Use five ada to commit and rely on `hydra-node` to balance the commit transaction.

```shell
cardano-cli transaction build-raw \
  --babbage-era \
  --tx-in 14ab373afb1112d925b0f6a84518ac26d4a8cfcc99231e1f47e6996182e843a9#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+5000000 \
  --fee 0 \
  --out-file tx.json
```

## Step 4
You should now have the _blueprint_ transaction in the `tx.json` file. For `hydra-node` to provide a draft commit transaction, you need to:

- Obtain the protocol parameters needed to run the `hydra-node`
- Ensure the `hydra-node` is up and running
- Have the `head` in the initializing state
- Submit the HTTP request to the `hydra-node` API server using the _blueprint_ transaction you just created and the `UTXO` used for its input.


Query the protocol parameters:

```shell
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --socket-path testnets/preprod/node.socket \
  --out-file pp-preprod.json

```

## Step 5
Start the `hydra-node` as a _single_ party head instance.

Note: The value `6264cee4d5eab3fb58ab67f3899ecbcc0d7e72732a2d9c1c5d638115db6ca711` comes from the `hydra-node` release [0.16.0](https://github.com/input-output-hk/hydra/releases/tag/0.16.0).

```shell
hydra-node \
  --node-id 1 --port 5001 --api-port 4001 \
  --hydra-signing-key demo/alice.sk \
  --hydra-scripts-tx-id 6264cee4d5eab3fb58ab67f3899ecbcc0d7e72732a2d9c1c5d638115db6ca711 \
  --cardano-signing-key hydra-cluster/config/credentials/alice.sk \
  --ledger-protocol-parameters pp-preprod.json \
  --testnet-magic 1 \
  --node-socket testnets/preprod/node.socket \
  --persistence-dir .
```

Now we can start `hydra-tui` and initialize the `Head`:

```shell
hydra-tui \
  --connect 0.0.0.0:4001 \
  --cardano-signing-key hydra-cluster/config/credentials/alice-funds.sk \
  --testnet-magic 1 \
  --node-socket testnets/preprod/node.socket
```

Press `i` to initialize the `head`.

Once the head is in the `Initializing` state, you can send the HTTP request to the `/commit` API path.

Assemble the request body using the `cborHex` field from the tx-body file `tx.json` and the JSON representation of the `UTXO` you used as input.

This is the valid JSON request:

```shell
{
  "blueprintTx": {
    "cborHex": "84a3008182582014ab373afb1112d925b0f6a84518ac26d4a8cfcc99231e1f47e6996182e843a900018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a007a12000200a0f5f6",
    "description": "",
    "type": "Tx BabbageEra"
  },
  "utxo": {
    "14ab373afb1112d925b0f6a84518ac26d4a8cfcc99231e1f47e6996182e843a9#0": {
      "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
      "datum": null,
      "datumhash": null,
      "inlineDatum": null,
      "referenceScript": null,
      "value": {
        "lovelace": 8000000
      }
    }
  }
}
```

Save this JSON to a `commit-request.json` file.

You can now prompt the running `hydra-node` to draft a commit transaction:


```
curl -X POST 127.0.0.1:4001/commit \
  --data @commit-request.json

```

This yields a large CBOR blob, which you can save to the `commit-tx.json` file.

Next, sign and submit the draft of the commit transaction:

```shell

cardano-cli transaction sign \
  --tx-file commit-tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file signed-tx.json


cardano-cli transaction submit \
  --tx-file signed-tx.json \
  --socket-path testnets/preprod/node.socket \
  --testnet-magic 1
```

If you start the `hydra-tui` and wait until the transaction you just sent is re-observed by the `hydra-node` we should see that the head is now open.