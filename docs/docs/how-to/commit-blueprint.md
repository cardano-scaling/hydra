---
sidebar_position: 3
---

# Deposit using a blueprint

A blueprint describes the inputs to spend and the outputs to create inside
an open head. This example deposits 5 ADA from an ADA-only input worth at
least 8 ADA and returns the remaining input value on L1.

## Prerequisites

Complete the [testnet setup](../tutorial/index.md) through opening the head.
Use its node, matching Hydra scripts, funded node wallet, and
`credentials/alice-funds.{sk,addr}`. Run these commands from that tutorial's
working directory with `cardano-cli`, `jq`, and `curl` available.

```shell
export CARDANO_NODE_SOCKET_PATH="$PWD/node.socket"
export CARDANO_NODE_NETWORK_ID=1
export HYDRA_API=http://127.0.0.1:4001
WALLET_ADDR=$(cat credentials/alice-funds.addr)

cardano-cli query utxo --address "$WALLET_ADDR" --out-file wallet-utxo.json
jq 'to_entries | map(select((.value.value | keys) == ["lovelace"] and .value.value.lovelace >= 8000000)) | .[:1] | from_entries' \
  wallet-utxo.json > deposit-utxo.json
TX_IN=$(jq -er 'keys[0] // error("Need an ADA-only UTxO of at least 8 ADA")' deposit-utxo.json)
```

## Build the blueprint

The 5 ADA output below becomes an L2 output. The blueprint itself is not
submitted to Cardano.

```shell
cardano-cli conway transaction build-raw \
  --tx-in "$TX_IN" \
  --tx-out "$WALLET_ADDR+5000000" \
  --fee 0 \
  --out-file blueprint.json

cardano-cli debug transaction view --tx-file blueprint.json
```

Check that the output contains 5,000,000 lovelace. Build the request from the
files instead of copying an encoded transaction from another example:

```shell
jq -n \
  --slurpfile blueprint blueprint.json \
  --slurpfile utxo deposit-utxo.json \
  --arg changeAddress "$WALLET_ADDR" \
  '{blueprintTx: $blueprint[0], utxo: $utxo[0], changeAddress: $changeAddress}' \
  > commit-request.json

curl --fail-with-body -sS "$HYDRA_API/commit" \
  -H 'Content-Type: application/json' \
  --data-binary @commit-request.json \
  > deposit-tx.json
```

`changeAddress` asks the node to return the remainder on L1. Without it,
provide a balanced blueprint whose entire output value is to be deposited.
The node pays the deposit transaction's fees from its own fuel wallet.

## Sign, submit, and wait

Open a WebSocket connection in another terminal before submitting:

```shell
websocat 'ws://127.0.0.1:4001?history=no'
```

Add Alice's witness while preserving the node's witness and output order:

```shell
cardano-cli conway transaction sign \
  --tx-file deposit-tx.json \
  --signing-key-file credentials/alice-funds.sk \
  --out-file deposit-signed.json

cardano-cli conway transaction submit --tx-file deposit-signed.json
```

The head is already open. `CommitRecorded` means the deposit was observed on
L1; it is not yet spendable on L2. After activation, snapshot approval, and
settlement of the increment, `CommitFinalized` confirms that the deposit has settled.
With default activation settings, maturation takes roughly an hour plus the
transaction-validity offset. See [deposit timing](../configuration.md#deposit-activation).

Verify the output in `GET /snapshot/utxo` and the change at Alice's L1 address.
If the deposit expires without being claimed, follow the
[recovery instructions](./incremental-commit.md#recover-a-deposit).
