---
sidebar_position: 1
---

# Commit using a blueprint

 This is a small walk-through on how to use `cardano-cli` to assemble everything needed to commit funds to a `Head` using a so-called blueprint transaction.

 Example assumes you have the hydra-node repo at your disposal together with `hydra-node`, `hydra-tui`, `cardano-cli` and `curl` binaries.

 We can use `cardano-cli` to create a _blueprint_ transaction from some `UTxO` we own.

 First we need a running cardano-node so let's spin one on the preprod network:


 ```shell
 ./testnets/cardano-node.sh ~/code/hydra/testnets/preprod
 ```

 Now we need to find the `UTxO` you want to commit to the `Head`

 In this example we will use Alice and her external wallet key so first let's find out the address:

 ```shell
 cardano-cli address build \
  --payment-verification-key-file hydra-cluster/config/credentials/alice-funds.vk \
  --testnet-magic 1

addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z
 ```

and query to see what `UTxO` Alice has:

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

Let's pick the first `UTxO` which has total of 8 ADA available. Let's use 5 ADA to commit and rely on `hydra-node` to balance the commit transaction.

```shell
cardano-cli transaction build-raw \
  --babbage-era \
  --tx-in 14ab373afb1112d925b0f6a84518ac26d4a8cfcc99231e1f47e6996182e843a9#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+5000000 \
  --fee 0 \
  --out-file tx.json
```

So now we have the _blueprint_ transaction present in the `tx.json` file.

In order to have `hydra-node` give us a draft commit transaction we need to:

- Obtain protocol-parameters needed to run the `hydra-node`
- Have the `hydra-node` up and running
- Have the `Head` in the initializing state
- Submit the http request to the `hydra-node` api server using the _blueprint_ transaction we just created and the `UTxO` used for it's input.


Query the preview protocol-parameters:

```shell
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --socket-path testnets/preprod/node.socket \
  --out-file pp-preprod.json

```

Start the `hydra-node` in as a _single_ party Head instance.

Note: The value `6264cee4d5eab3fb58ab67f3899ecbcc0d7e72732a2d9c1c5d638115db6ca711` comes from `hydra-node` release [0.16.0](https://github.com/input-output-hk/hydra/releases/tag/0.16.0)

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

Now press `i` to initialize the `Head`.

Once we see that the head is in the `Initializing` state we are ready to send the HTTP request to the `/commit` API path.

To assemble the request body we will use the `cborHex` field from the tx-body file `tx.json`.

To get the json representation of the `UTxO` we used as the input we can just copy/paste the output we got from cardano-cli when we did a `UTxO` query:

This is the valid json request:

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

Let's save this json to commit-request.json file.

Now, it is time to ask the running `hydra-node` to draft a commit transaction for us:


```
curl -X POST 127.0.0.1:4001/commit \
  --data @commit-request.json

```

This yields a large cbor blob which we can save to `commit-tx.json` file.

Now we need to sign and submit the draft commit transaction.

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

If we start the `hydra-tui` and wait a bit until the transaction we just sent is re-observed by the `hydra-node` we should see that the `Head` is now open.

