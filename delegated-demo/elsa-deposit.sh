#!/usr/bin/env bash
# Elsa deposits into the open head WITHOUT the operator's /commit endpoint.
#
# She builds the deposit transaction herself: the Hydra deposit datum is produced
# client-side by delegated-demo/js (a Cardano JS library), and cardano-cli
# assembles, signs and submits the tx straight to the cardano-node. The mediators
# never draft or co-sign it; they only observe it on-chain and increment the head.
#
# Usage: delegated-demo/elsa-deposit.sh [api-port]   (default 4002)
# Requires node + delegated-demo/js dependencies (npm install, run automatically).
set -eo pipefail

HERE=$(dirname "$(realpath "$0")")
# shellcheck source=/dev/null
source "$HERE/lib.sh"

PORT=${1:-$ELSA_PORT}
NAME=elsa
# Must match the mediators' node configuration so the deposit is claimable.
DEPOSIT_ACTIVATION=10
DEPOSIT_PERIOD=500
VALIDITY_MARGIN_SLOTS=300

command -v node >/dev/null || { echo "node is required (nix develop .#delegated-demo)" >&2; exit 1; }
# Resolve a writable copy of the js project (the script may live in the read-only
# nix store when launched from `nix run`).
JSDIR="$HERE/js"
if [ ! -w "$JSDIR" ]; then
  cp -r "$HERE/js" "$DEVNET_DIR/js" 2>/dev/null || true
  JSDIR="$DEVNET_DIR/js"
fi
[ -d "$JSDIR/node_modules" ] || { echo "installing js deps..." >&2; npm --prefix "$JSDIR" install --no-audit --no-fund >/dev/null 2>&1; }

head_is_open "$PORT" || { echo "head is not open on :$PORT; init it first" >&2; exit 1; }

# Public head id, read from the head (HeadIsOpen carries it). This is chain info,
# not an operator-side L1 action.
echo "reading head id from :$PORT ..." >&2
HEAD_ID_LOG=$(mktemp)
timeout 8 websocat -n -U "ws://127.0.0.1:$PORT?history=yes" >"$HEAD_ID_LOG" 2>/dev/null || true
HEAD_ID=$(jq -rs '[.[] | select(.tag=="HeadIsOpen")][0].headId // empty' "$HEAD_ID_LOG" 2>/dev/null)
rm -f "$HEAD_ID_LOG"
[ -n "$HEAD_ID" ] && [ "$HEAD_ID" != null ] || { echo "could not read head id" >&2; exit 1; }

# Derive the deposit script address from the embedded Hydra blueprint.
jq -r '{type:"PlutusScriptV3",description:"",cborHex:.validators[0].compiledCode}' \
  hydra-plutus/plutus.json > "$DEVNET_DIR/deposit.plutus"
DEPOSIT_ADDR=$(ccli_ conway address build --payment-script-file "$DEVNET_DIR/deposit.plutus" --testnet-magic "$NETWORK_MAGIC")

ELSA_ADDR=$(addr_of "$NAME")
ELSA_PKH=$(ccli_ conway address key-hash --payment-verification-key-file "$CREDS/$NAME.vk")

# Pick the largest UTxO to deposit whole, and a second UTxO to cover the L1 fee.
ccli conway query utxo --address "$ELSA_ADDR" --out-file "$DEVNET_DIR/$NAME-l1.json"
mapfile -t KEYS < <(jq -r 'to_entries | sort_by(.value.value.lovelace) | reverse | .[].key' "$DEVNET_DIR/$NAME-l1.json")
[ "${#KEYS[@]}" -ge 2 ] || { echo "$NAME needs at least two L1 UTxOs (one to deposit, one for the fee)" >&2; exit 1; }
DEPOSIT_TXIN=${KEYS[0]}
FEE_TXIN=${KEYS[1]}
AMOUNT=$(jq -r ".[\"$DEPOSIT_TXIN\"].value.lovelace" "$DEVNET_DIR/$NAME-l1.json")
DEP_TXID=${DEPOSIT_TXIN%#*}
DEP_IX=${DEPOSIT_TXIN#*#}

NOW=$(date +%s)
DEADLINE_MS=$(( (NOW + DEPOSIT_ACTIVATION + 2 * DEPOSIT_PERIOD) * 1000 ))
SLOT=$(( $(ccli query tip | jq -r .slot) + VALIDITY_MARGIN_SLOTS ))

echo "$NAME building deposit of ${AMOUNT} lovelace (UTxO $DEPOSIT_TXIN) client-side ..." >&2
node "$JSDIR/build-deposit-datum.mjs" \
  --head-id "$HEAD_ID" --deadline-ms "$DEADLINE_MS" \
  --txid "$DEP_TXID" --ix "$DEP_IX" --pkh "$ELSA_PKH" --amount "$AMOUNT" \
  --out "$DEVNET_DIR/$NAME-deposit-datum.cbor" >/dev/null

# Assemble, sign and submit straight to the cardano-node (no operator involved).
ccli conway transaction build \
  --tx-in "$DEPOSIT_TXIN" \
  --tx-in "$FEE_TXIN" \
  --tx-out "$DEPOSIT_ADDR+$AMOUNT" \
  --tx-out-inline-datum-cbor-file "$DEVNET_DIR/$NAME-deposit-datum.cbor" \
  --change-address "$ELSA_ADDR" \
  --invalid-hereafter "$SLOT" \
  --out-file "$DEVNET_DIR/$NAME-selfdeposit.json" >&2
ccli_ conway transaction sign --tx-file "$DEVNET_DIR/$NAME-selfdeposit.json" \
  --signing-key-file "$CREDS/$NAME.sk" --out-file "$DEVNET_DIR/$NAME-selfdeposit-signed.json"
run_and_wait "$PORT" CommitFinalized 240 \
  ccli conway transaction submit --tx-file "$DEVNET_DIR/$NAME-selfdeposit-signed.json" >/dev/null
echo "$NAME self-built deposit finalized; funds are in the head (operators only observed it)" >&2
