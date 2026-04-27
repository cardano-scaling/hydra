#!/usr/bin/env bash
# Normalize a hydra-node log to plain JSON-lines on stdout.
#
# Handles three input formats transparently:
#   1. Native hydra-node JSON (one JSON object per line)
#   2. journalctl --output=json     (top-level object with a .MESSAGE field
#                                    containing the original hydra JSON)
#   3. journalctl default/short     ("Mar 12 14:23:45 host prog[pid]: {json}")
#
# Lines that cannot be coerced to a hydra log object are silently dropped,
# so this script is safe to chain in front of any downstream jq pipeline.
#
# Usage: normalize.sh <logfile>

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <logfile>" >&2
  exit 2
fi

logfile="$1"

if [[ ! -r "$logfile" ]]; then
  echo "error: cannot read $logfile" >&2
  exit 1
fi
if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq not installed" >&2
  exit 1
fi

# -R: read raw lines (don't try to parse as JSON ourselves)
# -c: compact output
# Strategy per line:
#   - try parsing as JSON
#       - if it has .MESSAGE that looks like JSON, that's journalctl-json — unwrap
#       - else use the parsed object as-is
#   - if it didn't parse, try regex-stripping a "<...>: {json}" prefix
#   - finally, only emit lines that look like a hydra log (.timestamp + .message)
jq -R -c '
  def is_hydra: (.timestamp? != null) and (.message? != null);

  . as $line
  | (try fromjson catch null) as $j
  | (
      if $j == null then
        # journalctl short format: "<date> <host> <prog>[<pid>]: {json}"
        ($line | capture("\\]:\\s+(?<inner>\\{.*\\})\\s*$"; "x")? // null)
        | if . == null then null
          else (.inner | try fromjson catch null)
          end
      elif ($j | type) == "object"
           and ($j.MESSAGE? != null)
           and (($j.MESSAGE | type) == "string")
           and ($j.MESSAGE | ltrimstr(" ") | startswith("{")) then
        ($j.MESSAGE | try fromjson catch null)
      else
        $j
      end
    )
  | select(. != null and (type == "object") and is_hydra)
' "$logfile"
