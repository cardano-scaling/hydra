#!/usr/bin/env bash
# Shared helpers for the delegated-demo drivers (actor.sh, scenario.sh, observer).
#
# The model: alice/bob/carol run hydra-nodes as pure mediators (they hold Hydra
# keys and pay for head chain txs, but own none of the head's funds). Two people
# who are not part of the network, Anna and Elsa, bring their own funds in via
# POST /commit, transact on L2 with NewTx, and withdraw via POST /decommit. All
# fund-owner signing uses the owner's own Cardano key; mediators never see it.
#
# Every function expects the working directory to be the repository root, where
# ./demo/prepare-devnet.sh created ./devnet.

: "${DEVNET_DIR:=devnet}"
: "${NETWORK_MAGIC:=42}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-$DEVNET_DIR/node.socket}"
CREDS="$DEVNET_DIR/credentials"

# Fixed topology: Anna talks to mediator alice, Elsa to mediator bob. Carol is a
# pure mediator serving no user. Consumed by scripts that source this file.
# shellcheck disable=SC2034
ANNA_PORT=4001
# shellcheck disable=SC2034
ELSA_PORT=4002

# cardano-cli with, and without, the devnet magic appended.
ccli()  { cardano-cli "$@" --testnet-magic "$NETWORK_MAGIC"; }
ccli_() { cardano-cli "$@"; }

# Bech32 address for a named actor (reads $CREDS/<name>.vk).
addr_of() { ccli conway address build --payment-verification-key-file "$CREDS/$1.vk"; }

# L1 helpers.
l1_utxo()    { ccli conway query utxo --address "$(addr_of "$1")" --out-file /dev/stdout; }
l1_balance() { l1_utxo "$1" | jq '[.[].value.lovelace] | add // 0'; }

# Head (L2) helpers, scoped to a named actor's address, via a mediator API port.
head_utxo()    { curl -s "127.0.0.1:$1/snapshot/utxo" | jq "with_entries(select(.value.address == \"$(addr_of "$2")\"))"; }
head_balance() { head_utxo "$1" "$2" | jq '[.[].value.lovelace] | add // 0'; }

# True when the head reachable on the given API port is open.
head_is_open() { [ "$(curl -s -o /dev/null -w '%{http_code}' "127.0.0.1:$1/snapshot/utxo")" = "200" ]; }

# Format a lovelace amount as ada for display.
ada() { awk -v l="$1" 'BEGIN{ printf "%.6f ada", l/1000000 }'; }

# Send a single ClientInput JSON message to a node's WebSocket and return. The
# trailing sleep keeps the connection open long enough for the frame to flush
# before EOF closes it.
send_client_input() {
  { printf '%s\n' "$2"; sleep 1; } \
    | timeout 6 websocat "ws://127.0.0.1:$1?history=no" >/dev/null 2>&1 || true
}

# Start listening for a server output <tag> on <port>, run the triggering action,
# then block until the tag is observed (or <timeout> seconds elapse). <tag> may be
# an alternation such as "A|B". The matched event is printed on success. Listener
# is started before the action so no event is missed.
run_and_wait() { # port tag timeout action...
  local port="$1" tag="$2" timeout="$3"; shift 3
  local log; log=$(mktemp)
  # -U (receive only) + -n (do not close on stdin EOF) keep this passive listener
  # connected; a plain websocat would close right after Greetings and miss events.
  websocat -n -U -B 16777216 "ws://127.0.0.1:$port?history=no" >"$log" 2>/dev/null &
  local wpid=$!
  sleep 2
  "$@"
  # Match on whole lines only, so a partially-written trailing line never causes
  # a real event to be missed (each server output is one compact JSON line).
  local pat="\"tag\": ?\"($tag)\""
  local waited=0
  while ! grep -Eq "$pat" "$log" 2>/dev/null; do
    sleep 1; waited=$((waited+1))
    if [ "$waited" -ge "$timeout" ]; then
      kill "$wpid" 2>/dev/null || true; wait "$wpid" 2>/dev/null || true
      rm -f "$log"; echo "timed out after ${timeout}s waiting for $tag" >&2; return 1
    fi
  done
  kill "$wpid" 2>/dev/null || true; wait "$wpid" 2>/dev/null || true
  grep -Em1 "$pat" "$log"
  rm -f "$log"
}

# Open the head through a mediator node. Idempotent: skips if already open.
init_head() { # port
  if head_is_open "$1"; then echo "head already open" >&2; return 0; fi
  echo "requesting Init via mediator on :$1 ..." >&2
  run_and_wait "$1" HeadIsOpen 120 send_client_input "$1" '{"tag":"Init"}' >/dev/null || return 1
  echo "head open" >&2
}

# Deposit a fund owner's largest single L1 UTxO into the head (external commit).
# The mediator only drafts and balances the deposit; the owner signs it.
commit() { # port name
  local port="$1" name="$2"
  head_is_open "$port" || { echo "head is not open yet; init first" >&2; return 1; }
  local addr; addr=$(addr_of "$name")
  ccli conway query utxo --address "$addr" --out-file "$DEVNET_DIR/$name-l1.json"
  local txin; txin=$(jq -r 'to_entries | max_by(.value.value.lovelace) | .key' "$DEVNET_DIR/$name-l1.json")
  [ "$txin" != "null" ] || { echo "$name has no L1 UTxO to commit" >&2; return 1; }
  # single-entry UTxO map for just the chosen input
  jq "{\"$txin\": .[\"$txin\"]}" "$DEVNET_DIR/$name-l1.json" > "$DEVNET_DIR/$name-commit-utxo.json"
  local amount; amount=$(jq -r ".[\"$txin\"].value.lovelace" "$DEVNET_DIR/$name-commit-utxo.json")
  echo "$name depositing ${amount} lovelace (UTxO $txin) via :$port ..." >&2
  curl -s -X POST "127.0.0.1:$port/commit" --data @"$DEVNET_DIR/$name-commit-utxo.json" > "$DEVNET_DIR/$name-deposit.json"
  ccli_ conway transaction sign --tx-file "$DEVNET_DIR/$name-deposit.json" \
    --signing-key-file "$CREDS/$name.sk" --out-file "$DEVNET_DIR/$name-deposit-signed.json"
  run_and_wait "$port" CommitFinalized 240 \
    ccli conway transaction submit --tx-file "$DEVNET_DIR/$name-deposit-signed.json" >/dev/null || return 1
  echo "$name commit finalized; funds now in the head" >&2
}

# Send lovelace between fund owners on L2 (NewTx). Builds a raw tx spending the
# sender's in-head UTxO, signs with the sender's key, submits via WebSocket.
send() { # port from to lovelace
  local port="$1" from="$2" to="$3" amount="$4"
  local from_addr to_addr; from_addr=$(addr_of "$from"); to_addr=$(addr_of "$to")
  head_utxo "$port" "$from" > "$DEVNET_DIR/$from-head.json"
  local txin total
  txin=$(jq -r --argjson a "$amount" 'to_entries | map(select(.value.value.lovelace >= $a)) | .[0].key' "$DEVNET_DIR/$from-head.json")
  [ "$txin" != "null" ] && [ -n "$txin" ] || { echo "$from has no in-head UTxO with >= ${amount} lovelace" >&2; return 1; }
  total=$(jq -r ".[\"$txin\"].value.lovelace" "$DEVNET_DIR/$from-head.json")
  local change=$((total - amount))
  echo "$from sending ${amount} lovelace to $to on L2 (change ${change}) ..." >&2
  ccli_ conway transaction build-raw \
    --tx-in "$txin" \
    --tx-out "$to_addr+$amount" \
    --tx-out "$from_addr+$change" \
    --fee 0 \
    --out-file "$DEVNET_DIR/$from-l2.json"
  ccli_ conway transaction sign --tx-body-file "$DEVNET_DIR/$from-l2.json" \
    --signing-key-file "$CREDS/$from.sk" --out-file "$DEVNET_DIR/$from-l2-signed.json"
  local msg; msg=$(jq -c '{tag: "NewTx", transaction: .}' "$DEVNET_DIR/$from-l2-signed.json")
  local out; out=$(run_and_wait "$port" "SnapshotConfirmed|TxInvalid" 60 send_client_input "$port" "$msg") || return 1
  if jq -e '.tag == "TxInvalid"' <<<"$out" >/dev/null; then
    echo "$from -> $to rejected on L2: $(jq -r '.validationError.reason' <<<"$out")" >&2
    return 1
  fi
  echo "$from -> $to confirmed on L2" >&2
}

# Withdraw a fund owner's entire in-head balance back to L1 (decommit).
withdraw() { # port name
  local port="$1" name="$2"
  local addr; addr=$(addr_of "$name")
  head_utxo "$port" "$name" > "$DEVNET_DIR/$name-head.json"
  local total; total=$(jq '[.[].value.lovelace] | add // 0' "$DEVNET_DIR/$name-head.json")
  [ "$total" -gt 0 ] 2>/dev/null || { echo "$name has nothing in the head to withdraw" >&2; return 1; }
  # spend every in-head UTxO the user owns into a single L1 output
  local txins=()
  while IFS= read -r k; do txins+=(--tx-in "$k"); done < <(jq -r 'keys[]' "$DEVNET_DIR/$name-head.json")
  echo "$name withdrawing ${total} lovelace to L1 via :$port ..." >&2
  ccli_ conway transaction build-raw \
    "${txins[@]}" \
    --tx-out "$addr+$total" \
    --fee 0 \
    --out-file "$DEVNET_DIR/$name-decommit.json"
  ccli_ conway transaction sign --tx-file "$DEVNET_DIR/$name-decommit.json" \
    --signing-key-file "$CREDS/$name.sk" --out-file "$DEVNET_DIR/$name-decommit-signed.json"
  run_and_wait "$port" DecommitFinalized 120 \
    curl -s -X POST "127.0.0.1:$port/decommit" --data @"$DEVNET_DIR/$name-decommit-signed.json" >/dev/null || return 1
  echo "$name decommit finalized; funds back on L1" >&2
}
