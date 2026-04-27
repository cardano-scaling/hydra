#!/usr/bin/env bash
# Merge timelines from multiple hydra-node logs (one per node in a Head)
# into a single chronological TSV stream — your evidence ledger.
#
# Output (TSV): node<TAB>row<TAB>timestamp<TAB>kind<TAB>tag<TAB>summary
# Sorted by timestamp ascending. Stable when timestamps tie (preserves per-node row order).
#
# Each input is "label:path" — the label is whatever you tell the user
# (e.g. "leader", "peer-A", "alice"). Pick labels at the conversation
# level by asking the user whose log each file is.
#
# Inputs may be native hydra JSON, journalctl-json, or journalctl text —
# normalize.sh handles all three transparently.
#
# Usage:
#   merge_timelines.sh <label1>:<log1> <label2>:<log2> [<label3>:<log3> ...]
#                      [--all-tags] [--sn N] [--since-ts ISO8601]
#
#   --all-tags        emit every event, not just significant ones
#   --sn N            filter to events touching snapshot N (in summary or tag)
#   --since-ts T      filter to events at or after T (ISO 8601)

set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# Parse args
specs=()
extra_args=()
sn_filter=""
since_ts=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --all-tags) extra_args+=(--all-tags); shift;;
    --sn) sn_filter="$2"; shift 2;;
    --since-ts) since_ts="$2"; shift 2;;
    -h|--help)
      sed -n '2,30p' "$0"
      exit 0;;
    *)
      if [[ "$1" != *:* ]]; then
        echo "error: arg '$1' is not in label:path form" >&2
        exit 2
      fi
      specs+=("$1"); shift;;
  esac
done

if [[ "${#specs[@]}" -lt 2 ]]; then
  echo "usage: $0 <label1>:<log1> <label2>:<log2> [<label3>:<log3> ...] [--all-tags] [--sn N] [--since-ts T]" >&2
  exit 2
fi

# Per-spec, run timeline.sh and prefix every output row with the label.
# Skip the header row from each timeline.sh output.
combined=$(mktemp)
trap 'rm -f "$combined"' EXIT

for spec in "${specs[@]}"; do
  label="${spec%%:*}"
  path="${spec#*:}"
  if [[ ! -r "$path" ]]; then
    echo "error: cannot read '$path'" >&2
    exit 1
  fi
  "$HERE/timeline.sh" "$path" "${extra_args[@]}" \
    | tail -n +2 \
    | awk -v l="$label" 'BEGIN{FS=OFS="\t"} {print l, $0}' \
    >> "$combined"
done

# Header
printf "node\trow\ttimestamp\tkind\ttag\tsummary\n"

# Sort by timestamp (column 3), stable. Apply optional filters.
sort -s -t $'\t' -k3,3 "$combined" \
| { if [[ -n "$since_ts" ]]; then awk -F'\t' -v t="$since_ts" '$3 >= t'; else cat; fi; } \
| { if [[ -n "$sn_filter" ]]; then
      # Match "sn=N" anywhere in the summary, or N alone in summary, or in tag.
      awk -F'\t' -v sn="$sn_filter" '
        $6 ~ ("sn=" sn "([^0-9]|$)") || $5 ~ ("^.*sn=" sn "$") {print}
      '
    else cat; fi; }
