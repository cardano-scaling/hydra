#!/usr/bin/env bash
# Build a chronological timeline of significant events from a hydra-node log.
#
# Output (TSV): row<TAB>timestamp<TAB>kind<TAB>tag<TAB>summary
#   row is the index in the normalized stream (Nth hydra log entry)
#   kind ∈ {state, effect, input, error}
#   tag is the literal tag from the log (e.g. SnapshotRequested, ReqSn, RequireFailed)
#   summary is a short human-readable detail (snapshot number, party, txid prefix...)
#
# Pipe through `column -t -s $'\t'` for aligned display.
#
# Accepts native hydra JSON, journalctl-json, and journalctl text format —
# see normalize.sh.
#
# Usage: timeline.sh <logfile> [--since-row N] [--head N] [--tail N] [--all-tags]
#
# By default only "significant" tags are emitted. Use --all-tags to dump every
# stateChange/effect/input regardless of tag.

set -euo pipefail

if [[ $# -lt 1 ]]; then
  cat >&2 <<EOF
usage: $0 <logfile> [--since-row N] [--head N] [--tail N] [--all-tags]
  --since-row N    start at row N (e.g. boundary from find_boundary.sh)
  --head N         show only first N timeline rows after filtering
  --tail N         show only last N timeline rows after filtering
  --all-tags       emit every event, not just significant ones
EOF
  exit 2
fi

logfile="$1"; shift
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
since_row=1
head_n=""
tail_n=""
all_tags=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --since-row|--since-line) since_row="$2"; shift 2;;
    --head) head_n="$2"; shift 2;;
    --tail) tail_n="$2"; shift 2;;
    --all-tags) all_tags=1; shift;;
    *) echo "unknown arg: $1" >&2; exit 2;;
  esac
done

if [[ ! -r "$logfile" ]]; then
  echo "error: cannot read $logfile" >&2
  exit 1
fi
if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq not installed" >&2
  exit 1
fi

# Significant tags — see references/log_format.md for the full taxonomy.
SIG_STATE='SnapshotRequested|SnapshotConfirmed|SnapshotRequestAborted|TransactionAppliedToLocalUTxO|CommitFinalized|DecommitFinalized|DepositActivated|DepositExpired|DepositRecorded|HeadOpened|HeadClosed|HeadIsContested|TickObserved|PartyCommitted|HeadInitialized|TransactionReceived'
SIG_EFFECT='ReqSn|AckSn|ReqTx|ReqDec|ConfTx'
SIG_INPUT='NetworkInput|ChainInput|ClientInput'

printf "row\ttimestamp\tkind\ttag\tsummary\n"

# Single-pass approach: normalize then awk to slice by row, then one jq invocation.
# jq's input_line_number reflects position in the sliced stream, so we add the offset back.
"$HERE/normalize.sh" "$logfile" \
| awk -v start="$since_row" 'NR>=start' \
| jq -r \
    --arg sigState "$SIG_STATE" \
    --arg sigEffect "$SIG_EFFECT" \
    --arg sigInput "$SIG_INPUT" \
    --argjson allTags "$all_tags" \
    --argjson offset "$since_row" '

  def short(s): if s == null then "" else (s|tostring)[0:12] end;
  def keep(t; sig): $allTags == 1 or (t|tostring|test("^(" + sig + ")$"));

  (input_line_number + $offset - 1) as $row
  | (.timestamp // "?") as $ts
  | (
      # State changes
      (.message.node.outcome.stateChanges // [])
      | .[]?
      | (.tag // "?") as $tag
      | select(keep($tag; $sigState))
      | [
          $row, $ts, "state", $tag,
          ([
            (if (.snapshot.number // .snapshotNumber) != null
              then "sn=\(.snapshot.number // .snapshotNumber)"
              else empty end),
            (if .party.vkey then "party=\(short(.party.vkey))" else empty end),
            (if .depositTxId then "deposit=\(short(.depositTxId))" else empty end),
            (if .chainTime then "ct=\(.chainTime)" else empty end),
            (if .newLocalUTxO then "utxoΔ" else empty end)
          ] | map(select(. != "")) | join(" "))
        ]
    ),

    (
      # Network/chain effects
      (.message.node.outcome.effects // [])
      | .[]?
      | (.message.tag // .tag // "?") as $tag
      | select(keep($tag; $sigEffect))
      | [
          $row, $ts, "effect", $tag,
          ([
            (if .message.snapshotNumber != null then "sn=\(.message.snapshotNumber)" else empty end),
            (if .message.transactionIds then "txs=\(.message.transactionIds|length)" else empty end),
            (if .message.transaction.txId then "tx=\(short(.message.transaction.txId))" else empty end),
            (if .toParty.vkey then "→\(short(.toParty.vkey))" else empty end)
          ] | map(select(. != "")) | join(" "))
        ]
    ),

    (
      # Inputs
      .message.node.input // empty
      | (.tag // "?") as $tag
      | select(keep($tag; $sigInput))
      | [
          $row, $ts, "input", $tag,
          ([
            (if .message.tag then "msg=\(.message.tag)" else empty end),
            (if .message.snapshotNumber != null then "sn=\(.message.snapshotNumber)" else empty end),
            (if .party.vkey then "from=\(short(.party.vkey))" else empty end)
          ] | map(select(. != "")) | join(" "))
        ]
    ),

    (
      # Errors / failures
      ((.message.node.outcome.error // .message.node.outcome.tag // empty) | tostring) as $err
      | select($err != "" and ($err | test("Error|Failed|Invalid"; "i")))
      | [$row, $ts, "error", $err, ""]
    )
  | @tsv
' \
| { if [[ -n "$head_n" ]]; then head -n "$head_n"; \
    elif [[ -n "$tail_n" ]]; then tail -n "$tail_n"; \
    else cat; fi; }
