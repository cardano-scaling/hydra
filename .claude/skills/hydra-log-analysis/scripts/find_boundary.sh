#!/usr/bin/env bash
# Find the last SnapshotConfirmed event in a hydra-node JSON log.
# Output (TSV): row<TAB>timestamp<TAB>snapshot<TAB>signatures
#
# `row` is the index in the normalized stream (i.e. the Nth valid hydra log
# entry, ignoring journalctl prefix noise). This is what timeline.sh's
# --since-line expects.
#
# This is the boundary for downstream analysis: everything after this row
# is the "problem window" when investigating a stall.
#
# Accepts native hydra JSON, journalctl-json (.MESSAGE wrapper), and
# journalctl text format — see normalize.sh.
#
# Usage: find_boundary.sh <logfile>

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <logfile>" >&2
  exit 2
fi

logfile="$1"
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

if [[ ! -r "$logfile" ]]; then
  echo "error: cannot read $logfile" >&2
  exit 1
fi
if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq not installed" >&2
  exit 1
fi

# Single jq pass over the normalized stream. SnapshotConfirmed appears in
# message.node.outcome.stateChanges[].tag — match anywhere via recursive descent.
# Emit every match with row number; tail picks the last one.
result=$("$HERE/normalize.sh" "$logfile" \
  | jq -r '
      input_line_number as $row
      | (.timestamp // "?") as $ts
      | (.. | objects | select(.tag? == "SnapshotConfirmed")) as $sc
      | [
          $row, $ts,
          (($sc.snapshot.number // $sc.snapshotNumber // "?") | tostring),
          (($sc.signatures // {}) | length | tostring)
        ] | @tsv
    ' 2>/dev/null | tail -n 1)

if [[ -z "$result" ]]; then
  echo "no SnapshotConfirmed event found in $logfile" >&2
  exit 3
fi

printf "row\ttimestamp\tsnapshot\tsignatures\n"
printf "%s\n" "$result"
