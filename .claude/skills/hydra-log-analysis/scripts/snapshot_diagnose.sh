#!/usr/bin/env bash
# Diagnostic battery for hydra-node logs — answers "why did snapshots stop confirming?"
#
# Runs canned checks against the failure patterns documented in
# references/failure_patterns.md. Each check that fires emits one finding line:
#   [PATTERN] short description (evidence)
#
# Exit code: 0 if any pattern matched, 1 if no patterns matched, 2 on bad input.
#
# Usage: snapshot_diagnose.sh <logfile> [--since-line N]

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <logfile> [--since-row N]" >&2
  exit 2
fi

logfile="$1"; shift
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
since_row=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    --since-row|--since-line) since_row="$2"; shift 2;;
    *) echo "unknown arg: $1" >&2; exit 2;;
  esac
done

if [[ ! -r "$logfile" ]]; then
  echo "error: cannot read $logfile" >&2
  exit 2
fi
if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq not installed" >&2
  exit 2
fi

# Normalize once into a tmpfile (handles journalctl), then slice.
# Subsequent jq passes read the slice without re-normalizing.
slice=$(mktemp)
trap 'rm -f "$slice"' EXIT
"$HERE/normalize.sh" "$logfile" | awk -v start="$since_row" 'NR>=start' > "$slice"

findings=0

emit() {
  printf "[%s] %s\n" "$1" "$2"
  findings=$((findings+1))
}

# ---------------------------------------------------------------------------
# Check 1: ReqSn flood / etcd feedback loop
# Pattern: ReqSn for the same snapshot number broadcast many times.
# Reference: references/failure_patterns.md#reqsn-flood
# ---------------------------------------------------------------------------
reqsn_counts=$(jq -r '
  .message.node.outcome.effects // []
  | .[]?
  | select((.message.tag // .tag) == "ReqSn")
  | .message.snapshotNumber // empty
' "$slice" | sort | uniq -c | sort -rn)

if [[ -n "$reqsn_counts" ]]; then
  worst_count=$(echo "$reqsn_counts" | awk 'NR==1 {print $1}')
  worst_sn=$(echo "$reqsn_counts" | awk 'NR==1 {print $2}')
  if [[ "$worst_count" -gt 10 ]]; then
    emit "REQSN_FLOOD" "snapshot $worst_sn had $worst_count ReqSn broadcasts (>10 indicates feedback loop)"
  fi
fi

# ---------------------------------------------------------------------------
# Check 2: Leader stuck in RequestedSnapshot — own-echo RequireFailed
# Pattern: SnapshotRequested with no following SnapshotConfirmed for same sn,
#          plus a RequireFailed event in between.
# Reference: references/failure_patterns.md#leader-stuck
# ---------------------------------------------------------------------------
last_requested=$(jq -r '
  .message.node.outcome.stateChanges // []
  | .[]?
  | select(.tag == "SnapshotRequested")
  | (.snapshot.number // .snapshotNumber // empty)
' "$slice" | tail -n 1)

last_confirmed=$(jq -r '
  .message.node.outcome.stateChanges // []
  | .[]?
  | select(.tag == "SnapshotConfirmed")
  | (.snapshot.number // .snapshotNumber // empty)
' "$slice" | tail -n 1)

if [[ -n "$last_requested" && "$last_requested" != "$last_confirmed" ]]; then
  rf_count=$(jq -r '
    [.. | objects | select(.tag? == "RequireFailed" or (.error? // "" | tostring | test("RequireFailed")))]
    | length
  ' "$slice" | awk '{s+=$1} END{print s+0}')
  if [[ "$rf_count" -gt 0 ]]; then
    emit "LEADER_STUCK" "SnapshotRequested(sn=$last_requested) never confirmed; $rf_count RequireFailed events in window"
  else
    emit "SNAPSHOT_NEVER_CONFIRMED" "SnapshotRequested(sn=$last_requested) never confirmed; no explicit RequireFailed (check AckSn coverage)"
  fi
fi

# ---------------------------------------------------------------------------
# Check 3: AckSn coverage — for last requested snapshot, which parties acked?
# Reference: references/failure_patterns.md#missing-acksn
# ---------------------------------------------------------------------------
if [[ -n "$last_requested" ]]; then
  ack_parties=$(jq -r --arg sn "$last_requested" '
    .message.node.input // empty
    | select(.tag == "NetworkInput")
    | .message
    | select(.tag == "AckSn" and ((.snapshotNumber // -1) | tostring) == $sn)
    | .party.vkey // empty
  ' "$slice" | sort -u)
  ack_count=$(echo "$ack_parties" | grep -c . || true)
  if [[ "$ack_count" -gt 0 ]]; then
    emit "ACKSN_COVERAGE" "sn=$last_requested received AckSn from $ack_count distinct party(ies): $(echo "$ack_parties" | tr '\n' ' ' | head -c 80)..."
  else
    emit "NO_ACKSN" "sn=$last_requested received zero AckSn — peers never acknowledged"
  fi
fi

# ---------------------------------------------------------------------------
# Check 4: Stale decommit — CommitFinalized followed by SnapshotRequested still
#          carrying decommitTx
# Reference: references/failure_patterns.md#stale-decommit
# ---------------------------------------------------------------------------
stale_decommit=$(jq -r '
  .message.node.outcome.stateChanges // []
  | .[]?
  | select(.tag == "SnapshotRequested" and .decommitTx != null)
  | "\(.snapshot.number // .snapshotNumber)"
' "$slice" | tail -n 1)

# Only flag if there was a CommitFinalized recently (cheap heuristic).
if [[ -n "$stale_decommit" ]]; then
  has_commit_finalized=$(jq -r '
    .. | objects | select(.tag? == "CommitFinalized") | "x"
  ' "$slice" | head -n 1)
  if [[ -n "$has_commit_finalized" ]]; then
    emit "STALE_DECOMMIT" "SnapshotRequested(sn=$stale_decommit) carries decommitTx after CommitFinalized — possible stale state"
  fi
fi

# ---------------------------------------------------------------------------
# Check 5: Deposit activation gap — DepositRecorded without DepositActivated
# Reference: references/failure_patterns.md#deposit-activation-race
# ---------------------------------------------------------------------------
recorded=$(jq -r '
  .. | objects | select(.tag? == "DepositRecorded") | .depositTxId // empty
' "$slice" | sort -u)
activated=$(jq -r '
  .. | objects | select(.tag? == "DepositActivated") | .depositTxId // empty
' "$slice" | sort -u)
if [[ -n "$recorded" ]]; then
  unactivated=$(comm -23 <(echo "$recorded") <(echo "$activated") || true)
  if [[ -n "$unactivated" ]]; then
    n=$(echo "$unactivated" | grep -c . || true)
    emit "DEPOSIT_NOT_ACTIVATED" "$n deposit(s) recorded but never activated: $(echo "$unactivated" | head -n 1 | cut -c1-12)..."
  fi
fi

# ---------------------------------------------------------------------------
# Check 6: Repeated chain-tick — same DepositExpired/DepositActivated emitted
#          on multiple ticks for the same deposit (pre-fix bug pattern)
# Reference: references/failure_patterns.md#repeated-chain-tick
# ---------------------------------------------------------------------------
dup_ticks=$(jq -r '
  .. | objects
  | select(.tag? == "DepositExpired" or .tag? == "DepositActivated")
  | "\(.tag) \(.depositTxId // "?")"
' "$slice" | sort | uniq -c | awk '$1 > 1 {print}')
if [[ -n "$dup_ticks" ]]; then
  worst=$(echo "$dup_ticks" | sort -rn | head -n 1)
  emit "REPEATED_CHAIN_TICK" "duplicate deposit transition events: $worst"
fi

# ---------------------------------------------------------------------------
# Check 7: RequireFailed counts by reason
# Reference: references/failure_patterns.md#requirefailed
# ---------------------------------------------------------------------------
rf_by_reason=$(jq -r '
  .. | objects
  | select(.tag? == "RequireFailed")
  | (.reason.tag // .reason // "unknown")
' "$slice" | sort | uniq -c | sort -rn | head -n 5)
if [[ -n "$rf_by_reason" ]]; then
  emit "REQUIREFAILED_SUMMARY" "top RequireFailed reasons: $(echo "$rf_by_reason" | tr '\n' ';' | head -c 200)"
fi

# ---------------------------------------------------------------------------
# Check 8: No new ReqTx but pending localTxs — input queue starved?
# Cheap heuristic only — full diagnosis requires queue depth telemetry.
# ---------------------------------------------------------------------------
last_reqtx_line=$(jq -r '
  input_line_number as $ln
  | (.message.node.outcome.effects // []) | .[]?
  | select((.message.tag // .tag) == "ReqTx")
  | $ln
' "$slice" | tail -n 1)
total_lines=$(wc -l < "$slice")
if [[ -n "$last_reqtx_line" && "$total_lines" -gt 0 ]]; then
  gap=$((total_lines - last_reqtx_line))
  if [[ "$gap" -gt 1000 ]]; then
    emit "REQTX_DROUGHT" "no ReqTx broadcast in last $gap log lines — possible queue starvation or no work"
  fi
fi

# ---------------------------------------------------------------------------
echo ""
if [[ "$findings" -eq 0 ]]; then
  echo "No known patterns matched. Use timeline.sh and references/jq_recipes.md for ad-hoc analysis."
  exit 1
else
  echo "$findings finding(s). Cross-reference with references/failure_patterns.md."
  exit 0
fi
