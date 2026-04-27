#!/usr/bin/env bash
# Cross-node diagnostic battery for a Hydra Head — runs over multiple
# node logs and identifies failures only visible cluster-wide.
#
# Each finding is followed by an EVIDENCE block: rows from the merged
# timeline that prove the finding. This is the "timeline that proves
# the error you are reporting is truthful" requirement.
#
# Usage:
#   cluster_diagnose.sh <label1>:<log1> <label2>:<log2> [<label3>:<log3> ...]
#
# Findings emitted:
#   ACKSN_GAP_CLUSTER  — leader's snapshot N has < quorum AckSn cluster-wide
#   REQSN_NOT_DELIVERED — leader sent ReqSn(N) but ≥1 peer never received it
#   STATE_DIVERGENCE   — first index where state-change tag streams differ
#   CHAIN_SKEW         — same chain event observed at very different times
#                        across nodes (>5s skew)

set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <label1>:<log1> <label2>:<log2> [<label3>:<log3> ...]" >&2
  exit 2
fi

specs=("$@")

# Build the merged timeline once.
merged=$(mktemp)
trap 'rm -f "$merged"' EXIT
"$HERE/merge_timelines.sh" "${specs[@]}" > "$merged"

findings=0

emit_finding() {
  local pattern="$1" headline="$2"
  printf "\n[%s] %s\n" "$pattern" "$headline"
  findings=$((findings+1))
}

emit_evidence_sn() {
  local sn="$1"
  echo "  EVIDENCE (timeline rows for sn=$sn):"
  awk -F'\t' -v sn="$sn" '
    NR == 1 { next }                              # skip header
    $6 ~ ("sn=" sn "([^0-9]|$)") || $5 ~ ("sn=" sn "$") {
      printf "    %s\n", $0
    }
  ' "$merged"
}

# ---------------------------------------------------------------------------
# Set 1: per-snapshot AckSn coverage across the cluster.
# For each snapshot number with a SnapshotRequested anywhere, count how many
# distinct nodes emitted an AckSn effect for that sn.
# ---------------------------------------------------------------------------

# Collect (sn, kind, node) triples from the merged stream.
# kind = req | ack | conf
sn_events=$(awk -F'\t' '
  NR == 1 { next }
  $5 == "SnapshotRequested" {
    if (match($6, /sn=[0-9]+/)) {
      sn = substr($6, RSTART+3, RLENGTH-3)
      print sn "\treq\t" $1
    }
  }
  $5 == "AckSn" {
    if (match($6, /sn=[0-9]+/)) {
      sn = substr($6, RSTART+3, RLENGTH-3)
      print sn "\tack\t" $1
    }
  }
  $5 == "SnapshotConfirmed" {
    if (match($6, /sn=[0-9]+/)) {
      sn = substr($6, RSTART+3, RLENGTH-3)
      print sn "\tconf\t" $1
    }
  }
' "$merged" | sort -u)

n_nodes="${#specs[@]}"
quorum=$n_nodes  # Hydra requires unanimous acknowledgement

# For each (sn, kind), count distinct nodes.
echo "$sn_events" | awk -F'\t' '
  { key = $1 "\t" $2; nodes[key] = nodes[key] " " $3 }
  END {
    for (k in nodes) {
      n = split(nodes[k], a, " "); seen = ""
      count = 0
      for (i = 1; i <= n; i++) {
        if (a[i] == "") continue
        if (seen !~ ("(^| )" a[i] "( |$)")) { seen = seen " " a[i]; count++ }
      }
      print k "\t" count "\t" seen
    }
  }
' | sort -k1,1n -k2,2 > "$merged.cov"

# Find requested snapshots that never reached quorum AckSn.
while IFS=$'\t' read -r sn kind count nodes; do
  [[ "$kind" != "req" ]] && continue
  # AckSn count for same sn
  ack_count=$(awk -F'\t' -v sn="$sn" '$1==sn && $2=="ack" {print $3}' "$merged.cov")
  ack_count=${ack_count:-0}
  conf_count=$(awk -F'\t' -v sn="$sn" '$1==sn && $2=="conf" {print $3}' "$merged.cov")
  conf_count=${conf_count:-0}
  if [[ "$conf_count" -ge 1 ]]; then continue; fi   # snapshot succeeded somewhere; skip
  if [[ "$ack_count" -lt "$quorum" ]]; then
    emit_finding "ACKSN_GAP_CLUSTER" \
      "sn=$sn requested but only $ack_count/$quorum nodes emitted AckSn (no node confirmed)"
    emit_evidence_sn "$sn"
  fi
done < "$merged.cov"
rm -f "$merged.cov"

# ---------------------------------------------------------------------------
# Set 2: ReqSn delivery — for each ReqSn effect, did all peers receive it
# (visible as their own NetworkInput on that sn)?
# ---------------------------------------------------------------------------

# ReqSn effects per sn → set of leaders.
# NetworkInput msg=ReqSn per sn → set of receivers.
delivery=$(awk -F'\t' '
  NR == 1 { next }
  $5 == "ReqSn" && $4 == "effect" {
    if (match($6, /sn=[0-9]+/)) {
      sn = substr($6, RSTART+3, RLENGTH-3)
      print sn "\tsent\t" $1
    }
  }
  $4 == "input" && $5 == "NetworkInput" && $6 ~ /msg=ReqSn/ {
    if (match($6, /sn=[0-9]+/)) {
      sn = substr($6, RSTART+3, RLENGTH-3)
      print sn "\trecv\t" $1
    }
  }
' "$merged" | sort -u)

# Build per-sn sets and compare.
echo "$delivery" | awk -F'\t' -v all_nodes="${specs[*]%%:*}" -v n_nodes="$n_nodes" '
  { rec[$1 "\t" $2] = rec[$1 "\t" $2] " " $3 }
  END {
    # Determine all sns that had any ReqSn at all.
    for (k in rec) {
      split(k, p, "\t"); sns[p[1]] = 1
    }
    for (sn in sns) {
      sent = rec[sn "\tsent"]
      recv = rec[sn "\trecv"]
      # n_recv should equal n_nodes - 1 (everyone except the leader).
      n = split(recv, a, " "); seen = ""; rcount = 0
      for (i = 1; i <= n; i++) {
        if (a[i] == "") continue
        if (seen !~ ("(^| )" a[i] "( |$)")) { seen = seen " " a[i]; rcount++ }
      }
      if (sent != "" && rcount < n_nodes - 1) {
        printf "%s\t%d\t%s\n", sn, rcount, recv
      }
    }
  }
' | sort -n > "$merged.deliv"

while IFS=$'\t' read -r sn rcount recv; do
  emit_finding "REQSN_NOT_DELIVERED" \
    "sn=$sn — leader sent ReqSn but only $rcount peer(s) saw it as NetworkInput (expected $((n_nodes-1)))"
  emit_evidence_sn "$sn"
done < "$merged.deliv"
rm -f "$merged.deliv"

# ---------------------------------------------------------------------------
# Set 3: state divergence — first index where stateChange tag stream differs.
# We compare the per-node state-change tag stream (in their own row order),
# zipped on stream index.
# ---------------------------------------------------------------------------

state_streams_dir=$(mktemp -d)
trap 'rm -rf "$merged" "$state_streams_dir"' EXIT

for spec in "${specs[@]}"; do
  label="${spec%%:*}"
  awk -F'\t' -v l="$label" '$1==l && $4=="state" {print $5}' "$merged" \
    > "$state_streams_dir/$label"
done

# Zip the tag streams. Find first index where any node disagrees.
labels=( )
for spec in "${specs[@]}"; do labels+=("${spec%%:*}"); done

paste "${labels[@]/#/$state_streams_dir/}" \
  | awk -F'\t' -v labels="${labels[*]}" '
    BEGIN { split(labels, lbl, " ") }
    {
      n = NF
      same = 1
      first = $1
      for (i = 2; i <= n; i++) if ($i != "" && $i != first && first != "") { same = 0; break }
      if (!same) {
        printf "  index %d: ", NR
        for (i = 1; i <= n; i++) printf "%s=%s ", lbl[i], ($i ? $i : "(none)")
        print ""
        exit
      }
    }
  ' > "$state_streams_dir/divergence"

if [[ -s "$state_streams_dir/divergence" ]]; then
  emit_finding "STATE_DIVERGENCE" "node state-change streams diverge at:"
  cat "$state_streams_dir/divergence"
fi

# ---------------------------------------------------------------------------
# Set 4: chain skew — same chain-observed event seen at different timestamps
# across nodes (>5s skew = potentially serious, indicates one node lagging).
# ---------------------------------------------------------------------------

# Chain events keyed by their depositTxId or chainTime (already in summary).
# For each chain-related state-change tag, find earliest and latest timestamp.
awk -F'\t' '
  NR == 1 { next }
  $5 == "DepositRecorded" || $5 == "DepositActivated" || $5 == "DepositExpired" \
    || $5 == "CommitFinalized" || $5 == "DecommitFinalized" {
    key = $5 "\t" $6
    if (!(key in first_ts)) { first_ts[key] = $3; first_node[key] = $1 }
    last_ts[key] = $3; last_node[key] = $1
  }
  END {
    for (k in first_ts) print k "\t" first_node[k] "\t" first_ts[k] "\t" last_node[k] "\t" last_ts[k]
  }
' "$merged" \
  | while IFS=$'\t' read -r tag summary first_n first_t last_n last_t; do
      [[ "$first_n" == "$last_n" ]] && continue
      # Skew calculation in seconds (POSIX). Best-effort with `date -d`.
      if first_secs=$(date -d "$first_t" +%s 2>/dev/null) \
         && last_secs=$(date -d "$last_t" +%s 2>/dev/null); then
        skew=$((last_secs - first_secs))
        if [[ "$skew" -gt 5 ]]; then
          emit_finding "CHAIN_SKEW" \
            "$tag ($summary) seen by $first_n at $first_t but $last_n at $last_t (~${skew}s later)"
        fi
      fi
    done

# ---------------------------------------------------------------------------

echo ""
if [[ "$findings" -eq 0 ]]; then
  echo "No cluster-level patterns matched. The merged timeline is in this run's tmpdir; for ad-hoc inspection:"
  echo "  scripts/merge_timelines.sh ${specs[*]} | less"
  exit 1
else
  echo "$findings cross-node finding(s). Cross-reference with references/multi_node.md."
  exit 0
fi
