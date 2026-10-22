---
sidebar_position: 2
---

# Deposit a script UTxO

This example deposits an output while preserving its script address and datum
inside the head. It uses an always-succeeding script on **preprod only**;
anyone can spend funds locked by this example script. For application-specific
checks, see [deposit from a dApp](./best-practise-dapp.md).

## Prerequisites

Complete the [testnet tutorial](../tutorial/index.md) through opening a head.
Keep that node running with fuel available. Use the same working directory,
Alice's funded wallet, `cardano-cli`, `jq`, and `curl`.

```shell
export CARDANO_NODE_SOCKET_PATH="$PWD/node.socket"
export CARDANO_NODE_NETWORK_ID=1
export HYDRA_API=http://127.0.0.1:4001
WALLET_ADDR=$(cat credentials/alice-funds.addr)
cardano-cli query protocol-parameters --out-file l1-protocol-parameters.json
```

## Create and fund the script output

Save this script as `always-true.plutus`:

```json
{
  "type": "PlutusScriptV2",
  "description": "Always succeeds: test funds only",
  "cborHex": "49480100002221200101"
}
```

Create its address and select an ADA-only funding input with enough value for
the 10 ADA output, transaction fees, and change:

```shell
cardano-cli address build \
  --payment-script-file always-true.plutus \
  --testnet-magic 1 --out-file script.addr
SCRIPT_ADDR=$(cat script.addr)
cardano-cli query utxo --address "$WALLET_ADDR" --out-file wallet-utxo.json
FUNDING_INPUT=$(jq -er 'to_entries | map(select((.value.value | keys) == ["lovelace"] and .value.value.lovelace >= 15000000)) | .[0].key // error("Need at least 15 ADA")' wallet-utxo.json)

cardano-cli conway transaction build \
  --tx-in "$FUNDING_INPUT" \
  --tx-out "$SCRIPT_ADDR+10000000" \
  --tx-out-inline-datum-value 42 \
  --change-address "$WALLET_ADDR" \
  --out-file lock.json
cardano-cli conway transaction sign \
  --tx-file lock.json \
  --signing-key-file credentials/alice-funds.sk \
  --out-file lock-signed.json
cardano-cli conway transaction submit --tx-file lock-signed.json
LOCK_TX_ID=$(cardano-cli conway transaction txid --tx-file lock-signed.json | jq -er .txhash)
```

Wait until the following query returns the script output. Selecting its exact
reference avoids accidentally using another person's output at this shared
script address.

```shell
SCRIPT_INPUT="$LOCK_TX_ID#0"
cardano-cli query utxo --tx-in "$SCRIPT_INPUT" --out-file script-utxo.json
jq -e --arg input "$SCRIPT_INPUT" 'has($input)' script-utxo.json
```

## Prepare the blueprint

The blueprint includes the script witness, datum reference, redeemer, and an
execution budget. It has no outputs, so the node records the supplied UTxO
with its existing address, value, datum, and reference.

```shell
cardano-cli conway transaction build-raw \
  --tx-in "$SCRIPT_INPUT" \
  --tx-in-script-file always-true.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-value 42 \
  --tx-in-execution-units '(1000000, 100000)' \
  --protocol-params-file l1-protocol-parameters.json \
  --fee 0 \
  --out-file script-blueprint.json

jq -n \
  --slurpfile blueprint script-blueprint.json \
  --slurpfile utxo script-utxo.json \
  '{blueprintTx: $blueprint[0], utxo: $utxo[0]}' \
  > script-commit-request.json
```

`build-raw` constructs a transaction; it does not evaluate its scripts. The
node evaluates and balances the actual deposit transaction. For a different
script, supply its required datum, redeemer, signers, reference inputs, and
input context. Use explicit blueprint outputs when supplying reference-input
context, because a blueprint without outputs deposits the entire supplied
UTxO map. Even an always-succeeding script consumes execution resources.

If you supply explicit blueprint outputs, those outputs define ownership on
L2. Paying them to a key address releases them from the script. Do not include reference
scripts in the deposited outputs: the node cannot reconstruct them from the
deposit encoding. Publish them separately on L2 if needed.

## Submit the deposit

Open a WebSocket connection before submitting to observe the lifecycle:

```shell
websocat 'ws://127.0.0.1:4001?history=no'
```

In the original terminal, request the draft and add any client witness while
preserving the node's existing witness:

```shell
curl --fail-with-body -sS "$HYDRA_API/commit" \
  -H 'Content-Type: application/json' \
  --data-binary @script-commit-request.json \
  > script-deposit.json
cardano-cli conway transaction sign \
  --tx-file script-deposit.json \
  --signing-key-file credentials/alice-funds.sk \
  --out-file script-deposit-signed.json
cardano-cli conway transaction submit --tx-file script-deposit-signed.json
```

Wait for `CommitFinalized`, then inspect `GET /snapshot/utxo`. The deposited
output should still have the script address and datum `42`. Submission and
`CommitRecorded` precede activation and settlement; they do not make the
output spendable on L2. See [deposit timing](../configuration.md#deposit-activation)
and [recovery](./incremental-commit.md#recover-a-deposit).
