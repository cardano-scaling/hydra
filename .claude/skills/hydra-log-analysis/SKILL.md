---
name: hydra-log-analysis
description: Diagnose hydra-node JSON log files when something has gone wrong on a running Head — most commonly "why did snapshots stop confirming?" Builds a chronological timeline of state changes, network effects, and chain events leading up to the problem, then matches the timeline against known Hydra failure patterns (etcd ReqSn feedback loops, leader stuck in RequestedSnapshot, stale decommit/deposit, version race, deposit activation race). Supports single-node analysis or multi-node Head analysis (correlates leader vs peers, AckSn coverage, state divergence) and accepts native hydra JSON, journalctl-json, and journalctl text formats. Use this skill when the user provides a path to a hydra-node log and asks why a Head stalled, why a transaction wasn't confirmed, why a snapshot wasn't acknowledged, or any similar protocol-stall diagnostic question. Also use it when investigating bench-e2e or devnet runs that produced unexpected throughput or hangs.
---

# Hydra Log Analysis

## Overview

Hydra-node emits structured JSON-lines logs where each entry contains an `input`, an `outcome` with `stateChanges` and `effects`, plus chain and network events. When a Head misbehaves — snapshots stop confirming, a leader gets stuck, a deposit never activates — the answer is usually reconstructable from the log, but only if the right fields are correlated in chronological order.

This skill provides a repeatable workflow: locate the boundary where things went wrong, walk forward emitting a timeline, match the timeline against the catalog of known failure patterns, and **only report a finding when the timeline conclusively proves it**.

## Two non-negotiable rules

These override convenience. Read them before doing anything else.

### 1. Read `hydra-node/src/Hydra/HeadLogic.hs` first

`HeadLogic.hs` is the source of truth for off-chain protocol behavior. It dictates which inputs produce which state changes and effects, what the preconditions of every transition are, and what the timer does in each `seenSnapshot` state. Log analysis without that context is guesswork.

Before opening any log file, read `hydra-node/src/Hydra/HeadLogic.hs` (and the associated `HeadLogic/State.hs`, `HeadLogic/Outcome.hs`, `HeadLogic/Error.hs` as needed). If the user has explicitly told you to skip this step for a particular task, only then skip it.

### 2. Be confident before reporting; "no problem found" is a valid answer

If the timeline does not conclusively support a diagnosis, **report that**. Do not speculate, do not pick the closest-looking pattern, do not invent a story to explain ambiguous evidence. A correct "I cannot tell from this log what is wrong" is far more useful than a confident wrong answer.

The bar: every reported finding must be backed by specific timeline rows that prove it. If you can't produce that evidence, you don't have the finding.

## When to use

Trigger on requests like:

- "Why did snapshots stop confirming in this run?"
- "Why is the Head stuck?"
- "Why didn't this transaction get into a snapshot?"
- "Analyze this hydra-node log."
- "What went wrong here?" (with a log path)
- "The bench run regressed — find out where."

Skip this skill for code-only questions that do not involve a concrete log file.

## Required inputs

Paths to one or more hydra-node log files, supplied by the user. Do not search the filesystem for log files.

When the user provides multiple files (a Head has 2+ nodes), **ask whose log each one is** — leader, peer-A, alice, etc. Use the labels they give you when invoking `merge_timelines.sh` and `cluster_diagnose.sh`. Do not auto-derive labels from filenames; node identity matters and is the user's to assert.

Logs may be in any of three formats — `normalize.sh` handles all transparently:
1. Native hydra-node JSON (one JSON object per line)
2. `journalctl --output=json` (`.MESSAGE` field wraps the original)
3. `journalctl` default/short text format (`Mar 12 ... prog[pid]: {json}`)

If `jq` is not installed, stop and tell the user.

## Workflow

### Step 0 — Read `HeadLogic.hs`

See rule 1. Skip only if the user has already explicitly waived this for the current task.

### Step 1 — Establish the boundary (single log)

```bash
scripts/find_boundary.sh <logfile>
```

Output: row index, timestamp, snapshot number, signature count of the last `SnapshotConfirmed`. Everything after this row is the "problem window."

The `row` index refers to the **normalized** stream (Nth valid hydra log entry, ignoring journalctl prefix noise). It is the value to pass to `--since-row` downstream.

### Step 2 — Build the timeline

**Single log:**

```bash
scripts/timeline.sh <logfile> [--since-row N] [--tail N]
```

**Multi-log (one Head, several nodes):**

```bash
scripts/merge_timelines.sh leader:<log1> peer-A:<log2> peer-B:<log3> [--sn N] [--since-ts T]
```

Output: TSV with `node | row | timestamp | kind | tag | summary`, sorted chronologically. Every later report cites rows from this output.

### Step 3 — Run the diagnostic battery

**Single log:**

```bash
scripts/snapshot_diagnose.sh <logfile>
```

Detects: REQSN_FLOOD, LEADER_STUCK, NO_ACKSN / ACKSN_COVERAGE, STALE_DECOMMIT, DEPOSIT_NOT_ACTIVATED, REPEATED_CHAIN_TICK, REQUIREFAILED_SUMMARY, REQTX_DROUGHT.

**Multi-log:**

```bash
scripts/cluster_diagnose.sh leader:<log1> peer-A:<log2> peer-B:<log3>
```

Detects: ACKSN_GAP_CLUSTER, REQSN_NOT_DELIVERED, STATE_DIVERGENCE, CHAIN_SKEW. Each finding is followed by an inline `EVIDENCE` block of merged-timeline rows — that is the proof.

### Step 4 — Interpret against known patterns

Match findings from Step 3 against `references/failure_patterns.md` (single-node) and `references/multi_node.md` (cluster). Each catalog entry documents signature, mechanism, fix history (commit/PR), and the code paths in `HeadLogic.hs` to verify against.

For cluster findings, **re-read** the relevant `HeadLogic.hs` code path before concluding. Verify the diagnosis against the actual transition logic; do not rely on the catalog entry alone.

If no pattern matches, fall back to `references/log_format.md` and `references/jq_recipes.md` for ad-hoc queries.

### Step 5 — Write the verdict

Produce a short report:

1. **Timeline** — relevant rows from Step 2, verbatim. Use literal tag names (`SnapshotRequested`, not "snapshot was requested").
2. **Diagnosis** — pattern name from the catalog, OR "no known pattern matches; insufficient evidence to identify root cause." For each finding, cite specific timeline rows that prove it. No row, no finding.
3. **Next steps** — what to inspect in `HeadLogic.hs`, what additional logs would disambiguate, or which experiment would confirm.

Keep it tight. Bullets beat paragraphs. The user reads this to decide where to look in code.

## Key references

- `references/log_format.md` — JSON paths, tag taxonomy, field semantics, format detection
- `references/failure_patterns.md` — single-node failure catalog
- `references/multi_node.md` — cluster-level failure catalog
- `references/jq_recipes.md` — ad-hoc one-liners

## Anti-patterns

- Do not skip Step 0. `HeadLogic.hs` first, always.
- Do not invent failure patterns. If nothing matches, say so.
- Do not summarize log lines into prose. Timeline rows are the evidence; show them.
- Do not paraphrase tags. The literal tag is what matches the code.
- Do not auto-label nodes from filenames. Ask the user.
- Do not run the full bench or replay the run. Logs only.
- Do not modify the log file. Read-only analysis.
