---
sidebar_position: 2
---

# Commit a Script UTxO into a Head

This guide provides a walkthrough on how to commit a UTxO from a script into a Hydra Head.

A **Script UTxO** is a special kind of Unspent Transaction Output (UTxO) that isn't controlled by a simple private key. Instead, it's locked by a script—a small program that runs on the blockchain. To spend the funds in a Script UTxO, you must provide data (a "redeemer" and "datum") that satisfies the conditions defined in that script.

Committing a script UTxO to a Hydra Head is a powerful feature that allows for more complex on-chain validation logic to be brought into the Head. This is useful for scenarios where you need to enforce specific rules on how funds can be spent, even within the Head's off-chain environment.

This tutorial will guide you through the entire process, which involves creating a script, locking an on-chain UTxO with it, and then preparing a special "blueprint" transaction to commit this script UTxO into an open Head.

## Prerequisites

This tutorial assumes you are familiar with the process of committing a standard UTxO to a Hydra Head. If you are not, please first read the [Commit using a blueprint](./commit-blueprint.md) tutorial.

You will also need:

- A running `cardano-node` connected to a testnet.
- `cardano-cli` in your `PATH`.
- `hydra-node` and `hydra-tui` in your `PATH`.

## Step 1: Create the script

For this tutorial, we'll use a simple "always true" validator script. This script will always succeed, regardless of the redeemer or datum.

Create a file named `always-true.plutus` with the following content:

```json
{
    "type": "PlutusScriptV2",
    "description": "Always true validator",
    "cborHex": "49480100002221200101"
}
```

## Step 2: Create the script address

Now, we need to create an address for this script. We can do this using `cardano-cli`:

```shell
cardano-cli address build \
  --payment-script-file always-true.plutus \
  --testnet-magic 1 \
  --out-file script.addr
```

This will create a file named `script.addr` containing the script address. You can inspect the address using `cat`:

```shell
cat script.addr
```

## Step 3: Lock funds in the script address

Before we can commit a script UTxO, we need to create one. This is done by sending funds to the script address.

First, find a UTxO in your wallet that you can use. You can query your wallet's UTxOs using `cardano-cli`. For this example, we'll use `alice-funds.sk`:

```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file hydra-cluster/config/credentials/alice-funds.vk --testnet-magic 1) \
  --testnet-magic 1 \
  --socket-path testnets/preprod/node.socket
```

Pick a UTxO from the output and use it to build a transaction that sends funds to the script address. Let's say you picked a UTxO with TxHash `<UTXO_TXIX>` containing `100 ADA`. We'll send `10 ADA` to the script address.

First, create a file named `datum.json` that contains the datum. For this example, we will just use the integer `42`.

```json
{
    "constructor": 0,
    "fields": [
        {
            "int": 42
        }
    ]
}
```

Now, build the transaction:

```shell
cardano-cli conway transaction build \
  --tx-in <UTXO_TXIX> \
  --tx-out $(cat script.addr)+10000000 \
  --tx-out-inline-datum-file datum.json \
  --change-address $(cardano-cli address build --payment-verification-key-file hydra-cluster/config/credentials/alice-funds.vk --testnet-magic 1) \
  --testnet-magic 1 \
  --socket-path testnets/preprod/node.socket \
  --out-file tx.raw
```

Note that we are also providing an inline datum via the `datum.json` file. Even though our script does not use it, a datum is required for all script UTxOs.

Now, sign and submit the transaction:

```shell
cardano-cli conway transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file tx.signed

cardano-cli conway transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1 \
  --socket-path testnets/preprod/node.socket
```

Once the transaction is confirmed, you can query the script address to see the newly created UTxO:

```shell
cardano-cli query utxo \
  --address $(cat script.addr) \
  --testnet-magic 1 \
  --socket-path testnets/preprod/node.socket
```

## Step 4: Prepare the commit

Now we are ready to prepare the commit. We'll create a blueprint transaction that spends the script UTxO. Note that this transaction is not meant to be signed and submitted to the Cardano network. It's just a blueprint that we'll send to the `hydra-node` to get a properly drafted commit transaction.

We use `cardano-cli ... build-raw` to construct this blueprint because it gives us full control over the transaction structure without trying to automatically balance it or calculate fees, which is perfect for a blueprint.

In this transaction, we'll spend the script UTxO and send the funds to our own wallet address.

```shell
cardano-cli conway transaction build-raw \
  --tx-in <SCRIPT_UTXO_TXIX> \
  --tx-in-script-file always-true.plutus \
  --tx-in-datum-value 42 \
  --tx-in-redeemer-value 42 \
  --tx-in-execution-units '(0, 0)' \
  --tx-out $(cardano-cli address build --payment-verification-key-file hydra-cluster/config/credentials/alice-funds.vk --testnet-magic 1)+10000000 \
  --fee 0 \
  --out-file tx.json
```

A real-world script, like one written in [Aiken](https://aiken-lang.org/), would use the datum to carry state and the redeemer to provide input for validation. Our "always-true" script doesn't actually check any of these, but they are still required fields for a valid transaction that spends a script UTxO. Note that we also provide `--tx-in-execution-units`. This is required for any script spend to tell the network how much computational resource to budget for the script's execution. Since our script does nothing, we can use `(0, 0)`.

## Step 5: Commit the script UTxO

This final step is very similar to the standard commit tutorial. We'll start a `hydra-node`, initialize a Head, and then use the blueprint transaction to get a commit transaction from the `hydra-node`.

First, start the `hydra-node` and `hydra-tui` as explained in the [Commit using a blueprint](./commit-blueprint.md#step-5) tutorial and initialize a Head.

Once the Head is in the `Initializing` state, you can send the HTTP request to the `/commit` API path.

Just like in the other tutorial, you need to assemble a JSON request body. The `blueprintTx` is the `tx.json` file we just created, and the `utxo` is the script UTxO you are committing.

```json
{
  "blueprintTx": {
    "type": "Tx BabbageEra",
    "description": "",
    "cborHex": "84a40081825820a5a11c227e92622321b7d526e0b7ade44c084f706e931e80829037cba55365f5000181825839000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f021a000f42400300a0f5f6"
  },
  "utxo": {
    "<SCRIPT_UTXO_TXIX>": {
      "address": "<SCRIPT_ADDRESS>",
      "datum": "42",
      "datumhash": null,
      "inlineDatum": true,
      "referenceScript": null,
      "value": {
        "lovelace": 10000000
      }
    }
  }
}
```

Save this to a `commit-request.json` file and use `curl` to send it to the `hydra-node`:

```shell
curl -X POST 127.0.0.1:4001/commit \
  --data @commit-request.json
```

This will give you the commit transaction, which you can then sign and submit as usual:

```shell
cardano-cli conway transaction sign \
  --tx-file commit-tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file signed-tx.json

cardano-cli conway transaction submit \
  --tx-file signed-tx.json \
  --socket-path testnets/preprod/node.socket \
  --testnet-magic 1
```

And that's it! After the transaction is confirmed on-chain, your script UTxO will be committed to the Head.
