#!/usr/bin/env bash
# Seed the delegated-demo devnet: fuel the three mediator nodes (they pay for
# head chain txs but own no head funds), publish Hydra reference scripts, and
# generate + fund the two external users Anna and Elsa, who are not parties.
# Requires cardano-cli, hydra-node and jq on PATH.
set -eo pipefail

HERE=$(dirname "$(realpath "$0")")
# shellcheck source=/dev/null
source "$HERE/lib.sh"

[[ $(jq -n '9223372036854775807') == "9223372036854775807" ]] \
  || { echo "please upgrade jq to version 1.7+" >&2; exit 1; }

# Generate a fresh payment key pair for an external user.
genActor() {
  local name=$1
  echo >&2 "Generating fresh keys for external user $name"
  ccli_ address key-gen \
    --verification-key-file "$CREDS/$name.vk" \
    --signing-key-file "$CREDS/$name.sk"
  chmod 0400 "$CREDS/$name.sk"
}

# Send AMOUNT lovelace from the faucet to a named actor as a single new UTxO.
seedFaucet() {
  local actor=$1 amount=$2
  echo >&2 "Seeding $actor with $amount lovelace"
  local faucet_addr txin txid
  faucet_addr=$(addr_of faucet)
  txin=$(ccli conway query utxo --address "$faucet_addr" --out-file /dev/stdout | jq -r 'keys[0]')
  ccli conway transaction build --cardano-mode \
    --change-address "$faucet_addr" --tx-in "$txin" \
    --tx-out "$(addr_of "$actor")+$amount" \
    --out-file "$DEVNET_DIR/seed-$actor.draft" >&2
  ccli conway transaction sign --tx-body-file "$DEVNET_DIR/seed-$actor.draft" \
    --signing-key-file "$CREDS/faucet.sk" --out-file "$DEVNET_DIR/seed-$actor.signed" >&2
  txid=$(ccli_ conway transaction txid --tx-file "$DEVNET_DIR/seed-$actor.signed" | tr -d '\r' | jq -r '.txhash')
  ccli conway transaction submit --tx-file "$DEVNET_DIR/seed-$actor.signed" >&2
  echo -n >&2 "waiting for $txid#0"
  while [ "$(ccli conway query utxo --tx-in "$txid#0" --out-file /dev/stdout | jq ".\"$txid#0\"")" = null ]; do
    sleep 1; echo -n >&2 .
  done
  echo >&2 " done"
}

echo >&2 "Fueling mediators alice, bob and carol..."
for m in alice bob carol; do seedFaucet "$m" 3000000000; done # 3000 Ada each

echo >&2 "Creating and funding external users Anna and Elsa..."
genActor anna
genActor elsa
# Distinct amounts so Anna and Elsa are easy to tell apart. The larger UTxO is
# what each commits into the head; the smaller stays on L1.
seedFaucet anna 800000000 # 800 Ada (Anna commits this)
seedFaucet anna 200000000 # 200 Ada (stays on L1)
seedFaucet elsa 500000000 # 500 Ada (Elsa commits this)
seedFaucet elsa 300000000 # 300 Ada (stays on L1)

echo >&2 "Writing zero-fee protocol parameters..."
ccli query protocol-parameters --socket-path "$DEVNET_DIR/node.socket" --out-file /dev/stdout \
  | jq '.txFeeFixed=0 | .txFeePerByte=0 | .executionUnitPrices.priceMemory=0 | .executionUnitPrices.priceSteps=0 | .minFeeRefScriptCostPerByte=0 | .maxTxSize=10250' \
  > "$DEVNET_DIR/protocol-parameters.json"

echo >&2 "Publishing Hydra reference scripts..."
echo "HYDRA_SCRIPTS_TX_ID=$(hydra-node publish-scripts \
  --testnet-magic "$NETWORK_MAGIC" \
  --node-socket "$DEVNET_DIR/node.socket" \
  --cardano-signing-key "$CREDS/faucet.sk")" > .env
echo >&2 "Wrote .env: $(cat .env)"
