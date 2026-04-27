# jq Recipes for hydra-node Logs

Copy-paste recipes for ad-hoc questions when the canned scripts don't cover what is needed. Each recipe assumes `LOG=path/to/hydra-node.log` and that the log is JSON-lines.

## Counts and tallies

### Count every state-change tag
```bash
jq -r '.message.node.outcome.stateChanges // [] | .[].tag' "$LOG" \
  | sort | uniq -c | sort -rn
```

### Count every effect tag
```bash
jq -r '.message.node.outcome.effects // [] | .[] | (.message.tag // .tag)' "$LOG" \
  | sort | uniq -c | sort -rn
```

### Count input tags
```bash
jq -r '.message.node.input // empty | .tag' "$LOG" \
  | sort | uniq -c | sort -rn
```

### ReqSn broadcasts per snapshot number (flood detector)
```bash
jq -r '.message.node.outcome.effects // [] | .[]
       | select((.message.tag // .tag) == "ReqSn")
       | .message.snapshotNumber' "$LOG" \
  | sort -n | uniq -c | sort -rn | head
```

### AckSn distinct parties per snapshot number
```bash
jq -r '.message.node.input // empty
       | select(.tag == "NetworkInput")
       | .message
       | select(.tag == "AckSn")
       | "\(.snapshotNumber)\t\(.party.vkey)"' "$LOG" \
  | sort -u \
  | awk '{c[$1]++} END {for (k in c) printf "sn=%s\t%d distinct parties\n", k, c[k]}' \
  | sort -k1.4n
```

## Timing and gaps

### Gap (in seconds) between consecutive SnapshotConfirmed events
```bash
jq -r 'select((.. | objects | .tag? // "") | test("SnapshotConfirmed"))
       | .timestamp' "$LOG" \
  | awk 'NR>1 {
      cmd = "date -u -d \"" $0 "\" +%s.%N";
      cmd | getline t; close(cmd);
      if (prev) printf "%.3f s\n", t - prev;
      prev = t;
      next
    }
    { cmd = "date -u -d \"" $0 "\" +%s.%N"; cmd | getline prev; close(cmd) }'
```

### Time between SnapshotRequested(N) and SnapshotConfirmed(N)
```bash
jq -r 'select(.. | objects | (.tag? == "SnapshotRequested" or .tag? == "SnapshotConfirmed"))
       | [.timestamp, (.. | objects | select(.tag? == "SnapshotRequested" or .tag? == "SnapshotConfirmed") | "\(.tag) sn=\(.snapshot.number // .snapshotNumber)")] | @tsv' "$LOG"
# Then pair adjacent rows for matching sn.
```

## Per-tag extraction

### Pull every SnapshotRequested with its decommit/deposit fields
```bash
jq -r '.message.node.outcome.stateChanges // [] | .[]
       | select(.tag == "SnapshotRequested")
       | [.timestamp, (.snapshot.number // .snapshotNumber),
          (.decommitTx // "null"),
          (.depositTxId // "null")] | @tsv' "$LOG"
```

### Show every RequireFailed with reason
```bash
jq -r '..|objects|select(.tag? == "RequireFailed")
       | "\(.reason.tag // .reason // "unknown")"' "$LOG" \
  | sort | uniq -c | sort -rn
```

### Plutus validation failures with debug traces
```bash
grep -n '"Plutus validation failed"' "$LOG" \
  | head -n 5
# Then on a hit:
sed -n '${LINE}p' "$LOG" | jq '..|.DebugFailure? | select(.)'
```

## Multi-node correlation

### Find where node1 and node2 first diverge in stateChange tags
```bash
diff \
  <(jq -r '.message.node.outcome.stateChanges // [] | .[].tag' node1.log) \
  <(jq -r '.message.node.outcome.stateChanges // [] | .[].tag' node2.log) \
  | head -n 40
```

### All AckSns received from a specific peer
```bash
jq -r --arg pk "abcdef0123" '
  .message.node.input // empty
  | select(.tag == "NetworkInput")
  | .message
  | select(.tag == "AckSn" and (.party.vkey | startswith($pk)))
  | "\(.snapshotNumber)"' "$LOG" \
  | sort -n | uniq
```

## Ad-hoc filters by line range

### View lines 1000–1100 as pretty JSON
```bash
sed -n '1000,1100p' "$LOG" | jq -C
```

### Last N significant events before EOF
```bash
tail -n 5000 "$LOG" \
  | jq -r 'select(.message.node.outcome.stateChanges // [] | length > 0)
           | [.timestamp, (.message.node.outcome.stateChanges[].tag)] | @tsv' \
  | tail -n 30
```

## Performance tips

- Always pre-filter with `grep -n` when looking for a known string — `grep` is 10–100× faster than `jq` over a multi-GB log.
- Use `--stream` mode for huge logs if you only need shallow fields.
- `jq -c` (compact) output is faster than pretty-printing.
- Avoid recursive descent (`..`) on large logs unless you need it; explicit paths are much faster.
