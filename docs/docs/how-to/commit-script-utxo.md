---
sidebar_position: 2
---

# Commit a Script UTxO into a Head

This guide provides a walkthrough on how to commit a UTxO from a script into a Hydra Head.

A **Script UTxO** is a special kind of Unspent Transaction Output (UTxO) that isn't controlled by a simple private key. Instead, it is locked by a script - a small program that runs on the blockchain. To spend the funds in a Script UTxO, you must provide data (a "redeemer" and "datum") that satisfies the conditions defined in that script.

Committing a script UTxO to a Hydra Head is a powerful feature that allows for more complex on-chain validation logic to be brought into the Head. This is useful for scenarios where you need to enforce specific rules on how funds can be spent, even within the Head's off-chain environment.

This tutorial will guide you through the entire process, which involves creating a script, locking an on-chain UTxO with it, and then preparing a special "blueprint" transaction to commit this script UTxO into an open Head.

## Prerequisites

This tutorial assumes you are familiar with the process of committing a standard UTxO to a Hydra Head. If you are not, please first read the [Commit using a blueprint](./commit-blueprint.md) tutorial.

You will also need:

- A running `cardano-node` connected to a testnet.
- `cardano-cli` in your `PATH`.
- `hydra-node` and `hydra-tui` in your `PATH`.

## Step 1: Set Up Your Environment

To avoid specifying the network identifier and the path to the node's socket in every command, you can set the following environment variables in your shell. Make sure to replace the values with the ones that are appropriate for your setup (e.g., for the pre-production testnet, the magic is `1`).

```shell
export CARDANO_NODE_SOCKET_PATH=<path-to>/node.socket
export CARDANO_TESTNET_MAGIC=1
export CREDENTIALS_PATH=hydra-cluster/config/credentials
```

## Step 2: Create the script

For this tutorial, we will use a simple "always true" validator script. This script will always succeed, regardless of the redeemer or datum.

Create a file named `always-true.plutus` with the following content:

```json
{
    "type": "PlutusScriptV2",
    "description": "Always true validator",
    "cborHex": "49480100002221200101"
}
```

## Step 3: Create the script address

Now, we need to create an address for this script. We can do this using `cardano-cli`:

```shell
cardano-cli address build \
  --payment-script-file always-true.plutus \
  --testnet-magic $CARDANO_TESTNET_MAGIC \
  --out-file script.addr
```

This will create a file named `script.addr` containing the script address. You can inspect the address using `cat`:

```shell
cat script.addr
```

## Step 4: Lock funds in the script address

Before we can commit a script UTxO, we need to create one. This is done by sending funds to the script address.

First, we need a UTxO in a wallet to fund the transaction. The following command will find the first available UTxO in a wallet funded by `alice-funds.sk`, and export its transaction input (`TxIn`) to an environment variable.

```shell
export UTXO_TXIX=$(cardano-cli query utxo --address $(cardano-cli address build --payment-verification-key-file ${CREDENTIALS_PATH}/alice-funds.vk --testnet-magic $CARDANO_TESTNET_MAGIC) --testnet-magic $CARDANO_TESTNET_MAGIC --output-json | jq -r 'keys[0]')
echo "Captured funding UTxO TxIn: $UTXO_TXIX"
```

Now that we have a funding UTxO, we can build the transaction to lock 10 ADA at the script address.

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
  --tx-in $UTXO_TXIX \
  --tx-out $(cat script.addr)+10000000 \
  --tx-out-inline-datum-file datum.json \
  --change-address $(cardano-cli address build --payment-verification-key-file ${CREDENTIALS_PATH}/alice-funds.vk --testnet-magic $CARDANO_TESTNET_MAGIC) \
  --testnet-magic $CARDANO_TESTNET_MAGIC \
  --out-file tx.raw
```

Note that we are also providing an inline datum via the `datum.json` file. Even though our script does not use it, a datum is required for all script UTxOs.

Now, sign and submit the transaction:

```shell
cardano-cli conway transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file ${CREDENTIALS_PATH}/alice-funds.sk \
  --out-file tx.signed

cardano-cli conway transaction submit \
  --tx-file tx.signed \
  --testnet-magic $CARDANO_TESTNET_MAGIC
```

Once the transaction is confirmed, you can query the script address to find the transaction input (`TxIn`) of the script UTxO we just created.

```shell
export SCRIPT_UTXO_TXIX=$(cardano-cli query utxo --address $(cat script.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --output-json | jq -r 'keys[0]')
echo "Captured script UTxO TxIn: $SCRIPT_UTXO_TXIX"
```

## Step 5: Prepare the Blueprint

Now we are ready to prepare the commit. We will create a blueprint transaction that spends the script UTxO. Note that this transaction is not meant to be signed and submitted to the Cardano network. It is just a blueprint that we will send to the `hydra-node` to get a properly drafted commit transaction.

We use `cardano-cli ... build-raw` to construct this blueprint because it gives us full control over the transaction structure without trying to automatically balance it or calculate fees, which is perfect for a blueprint.

For a script UTxO, the blueprint only needs to declare the script UTxO as an input. The `hydra-node` will use this, along with the UTxO context provided in the next step, to build the full commit transaction.

```shell
cardano-cli conway transaction build-raw \
  --tx-in $SCRIPT_UTXO_TXIX \
  --tx-in-script-file always-true.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-value 42 \
  --tx-in-execution-units '(1000000, 100000)' \
  --tx-out $(cardano-cli address build --payment-verification-key-file ${CREDENTIALS_PATH}/alice-funds.vk --testnet-magic $CARDANO_TESTNET_MAGIC)+10000000 \
  --fee 0 \
  --out-file tx.json
```

A real-world script, like one written in [Aiken](https://aiken-lang.org/), would use the datum to carry state and the redeemer to provide input for validation. Our "always-true" script doesn't actually check any of these, but they are still required fields for a valid transaction that spends a script UTxO. Note that we use `--tx-in-inline-datum-present` because the datum was already included on-chain when we created the script UTxO. We also provide `--tx-in-execution-units`. This is required for any script spend to tell the network how much computational resource to budget for the script's execution. Since our script does nothing, we can use `(0, 0)`.

## Step 6: Prepare the commit 

This final step brings everything together. We will start a `hydra-node`, initialize a Head, and then send our blueprint and UTxO context to the `/commit` endpoint.

**1. Start the Hydra Node and TUI**

In a **new terminal**, start a single-party `hydra-node`.
```shell
hydra-node \
  --node-id 1 \
  --api-port 4001 \
  --hydra-signing-key ${CREDENTIALS_PATH}/../alice.sk \
  --cardano-signing-key ${CREDENTIALS_PATH}/alice.sk \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet-magic $CARDANO_TESTNET_MAGIC \
  --node-socket $CARDANO_NODE_SOCKET_PATH
```

In another **new terminal**, start the `hydra-tui`.
```shell
hydra-tui \
  --connect 0.0.0.0:4001 \
  --cardano-signing-key ${CREDENTIALS_PATH}/alice-funds.sk
```
In the TUI, press `i` to initialize the Head. Once the state is `Initializing`, you can quit the TUI (`q`) and proceed.

**2. Send the Commit Request**

Now, we will build the commit request. This request contains two parts: the `utxo` context (which must include the script) and the `blueprintTx` (which provides the witness information).

```shell
# Set variables
BLUEPRINT_JSON=$(cat tx.json)
UTXO_JSON=$(cardano-cli query utxo --tx-in ${SCRIPT_UTXO_TXIX} --testnet-magic $CARDANO_TESTNET_MAGIC --output-json)

# Create the request body
jq -n \
  --argjson utxo "${UTXO_JSON}" \
  --argjson blueprintTx "${BLUEPRINT_JSON}" \
  '{ "utxo": $utxo, "blueprintTx": $blueprintTx }' \
  > commit-request.json
```

Now, use `curl` to send the request to the `hydra-node`, which will respond with a drafted commit transaction:

```shell
curl -X POST --data @commit-request.json http://127.0.0.1:4001/commit > commit-tx.json
```

**3. Sign and Submit**

This is the final step. The `commit-tx.json` file now contains a valid, balanced transaction drafted by the `hydra-node`. We just need to sign it with both our funds key and our party key, and then submit it.

```shell
cardano-cli conway transaction sign \
  --tx-body-file commit-tx.json \
  --signing-key-file ${CREDENTIALS_PATH}/alice-funds.sk \
  --signing-key-file ${CREDENTIALS_PATH}/alice.sk \
  --out-file signed-tx.json

cardano-cli conway transaction submit \
  --tx-file signed-tx.json \
  --testnet-magic $CARDANO_TESTNET_MAGIC
```

And that's it! After the transaction is confirmed on-chain, your script UTxO will be committed to the Head.
