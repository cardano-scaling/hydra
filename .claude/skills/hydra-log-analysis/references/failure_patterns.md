# Hydra Failure Patterns — Diagnostic Catalog

Each entry: **signature** (what to grep for), **mechanism** (why it happens), **fix history** (commit/PR if known), **code paths** (where to look).

When `snapshot_diagnose.sh` reports a finding tagged `[PATTERN_NAME]`, look up that anchor here.

---

## REQSN_FLOOD — etcd ReqSn feedback loop {#reqsn-flood}

**Signature**: same `snapshotNumber` appears in `effects[].message.tag == "ReqSn"` more than ~10 times. Often hundreds.

**Mechanism**: prior to the fix, `onOpenTimer` re-broadcast `ReqSn`+`AckSn` in the `SeenSnapshot` case while waiting for the remaining AckSns. Each broadcast went through etcd, which echoed it back as a `NetworkInput`, resetting the input queue's `lastWasTimer=False`, allowing the next timer tick to fire — creating a 200Hz feedback loop that flooded etcd's `PersistentQueue` (capacity 100), back-pressuring `processEffects` and delaying real AckSn delivery.

**Fix history**: removed `SeenSnapshot` case from `onOpenTimer`; timer in `SeenSnapshot` is now a noop. Branch `resilient-snapshots`, around 2026-03-10.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — `onOpenTimer`
- `hydra-node/src/Hydra/Node/InputQueue.hs` — `lastWasTimer` coalescing

**Disambiguation**: if ReqSn flood is present **without** repeated SnapshotRequested entries, it may be a different bug (peer re-broadcasting). Check whether the flood is on this node's effects or in its inputs.

---

## LEADER_STUCK — leader stuck in RequestedSnapshot {#leader-stuck}

**Signature**: a `SnapshotRequested(sn=N)` event with **no** matching `SnapshotConfirmed(sn=N)`, and one or more `RequireFailed` outcomes between them. Often the failing input is the leader's own echoed ReqSn.

**Mechanism**: `snapshotInFlight RequestedSnapshot{requested=sn} sn = False` lets the leader process its own echo, but if the echo fails validation (e.g. signature mismatch from a stale tx set), the leader stays in `RequestedSnapshot` forever — no further timer or ReqTx will reset it.

**Fix history**: introduced `SnapshotRequestAborted` StateChanged + `abortOwnEchoOnFail` wrapper in `onOpenNetworkReqSn`. Branch `resilient-snapshots`, 2026-03-12.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — `onOpenNetworkReqSn`, `abortOwnEchoOnFail`
- `hydra-node/golden/StateChanged/SnapshotRequestAborted.json`

**Disambiguation**: combined with REQSN_FLOOD it usually means the flood **caused** the stuck state (echoes interleave with stale ReqTx). Solo, it suggests a logic bug in own-echo validation.

---

## NO_ACKSN / ACKSN_COVERAGE — missing peer acknowledgements {#missing-acksn}

**Signature**: `SnapshotRequested(sn=N)` followed by zero or fewer-than-quorum `AckSn(sn=N)` inputs from peers.

**Mechanism**: peer never received the ReqSn, peer rejected it (peer's RequireFailed visible only in peer's log), or AckSn was lost in transit.

**Diagnosis**: requires peer logs. Run `find_boundary.sh` and `timeline.sh` on each peer's log; locate ReqSn input with the same `sn` and check what came after.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — `onOpenNetworkReqSn` (peer side)

---

## STALE_DECOMMIT — CommitFinalized leaves stale decommitTx {#stale-decommit}

**Signature**: `SnapshotRequested` where `decommitTx != null` follows a `CommitFinalized` event in which `confirmedSnapshot.utxoToDecommit != null`.

**Mechanism**: prior to fix, `aggregateNodeState` for `CommitFinalized` did not clear `decommitTx`, so the next snapshot still tried to decommit utxo that was already part of the just-finalized commit, causing signature/utxo mismatches.

**Fix history**: `aggregateNodeState` CommitFinalized case now clears `decommitTx` when `confirmedSnapshot.utxoToDecommit = Just _`. Branch `resilient-snapshots`, 2026-03-12.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — `aggregateNodeState`, CommitFinalized case

---

## DEPOSIT_NOT_ACTIVATED — deposit activation race {#deposit-activation-race}

**Signature**: `DepositRecorded(txId=X)` exists, but no `DepositActivated(txId=X)` despite the deposit deadline having passed (compare `chainTime` against deposit deadline).

**Mechanism**: `DepositActivated` aggregate set `currentDepositTxId = Just depositTxId` unconditionally, but a concurrent state path could clear it back to `Nothing` before the timer picked it up.

**Fix history**: `DepositActivated` aggregate now uses `currentDepositTxId = currentDepositTxId <|> Just depositTxId` so timer reliably picks it up. See project memory `project_deposit_bug.md`.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — `aggregateNodeState`, DepositActivated case

---

## REPEATED_CHAIN_TICK — duplicate DepositExpired/DepositActivated {#repeated-chain-tick}

**Signature**: same `(tag, depositTxId)` tuple appears in the log on multiple ticks — `DepositExpired tx=abc` fired 5 times, etc.

**Mechanism**: `onChainTick` re-emitted the transition event on every tick where the predicate held, instead of only on the tick where the status actually changed.

**Fix history**: `onChainTick` now uses `isTransitionTo` filter — only emits the event when status flips. Branch `resilient-snapshots`, 2026-03-12.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — `onChainTick`

---

## VERSION_RACE — version mismatch after CommitFinalized/DecommitFinalized {#version-race}

**Signature**: `RequireFailed` with reason involving `version` mismatch, often after a `CommitFinalized` or `DecommitFinalized`. Snapshots stop confirming until all parties agree on the new version.

**Mechanism**: version was previously updated immediately on the version-bump event, but peers' AckSns for the in-flight snapshot still carried the old version. Solution: defer version update until all parties acknowledge.

**Fix history**: deferred version update with chain-state consensus via AckSn. 2026-02-27.

**Code paths**:
- `hydra-node/src/Hydra/HeadLogic.hs` — version update logic

---

## REQUIREFAILED_SUMMARY — generic precondition violations {#requirefailed}

Not a single pattern but a fingerprint. The top reasons typically are:

- `ReqSvNumberInvalid` / `ReqSnNumberInvalid` — leader proposed wrong snapshot number
- `SnapshotAlreadySigned` — duplicate AckSn from same party
- `SnapshotDoesNotApply` — UTxO state mismatch (often points to a STALE_DECOMMIT or stale localUTxO)
- `InvalidMultisignature` — signature didn't verify (often from version mismatch)
- `RequireFailed` with `ReqVersionInvalid` — see VERSION_RACE

Cross-reference with `hydra-node/src/Hydra/HeadLogic/Error.hs` for the full taxonomy.

---

## REQTX_DROUGHT — no new ReqTx broadcast {#reqtx-drought}

**Signature**: gap of >1000 log lines since the last ReqTx effect.

**Mechanism**: usually benign (no client load) but can indicate input queue starvation if `lastWasTimer` was stuck `True` (rarer post-fix).

**Disambiguation**: if accompanied by REQSN_FLOOD, the queue was flooded with ReqSn echoes and starved real ReqTx. If solo, usually means the workload simply stopped.

---

## SNAPSHOT_NEVER_CONFIRMED — generic stall {#snapshot-never-confirmed}

**Signature**: `SnapshotRequested(sn=N)` at the end of the log with no following `SnapshotConfirmed(sn=N)` and no `RequireFailed`.

**Mechanism**: catch-all for stalls without an explicit error. Almost always reduces to NO_ACKSN once peer logs are inspected.

**Diagnosis**: requires peer logs.

---

## Pattern not in this file?

If `snapshot_diagnose.sh` returned no findings or the timeline shows something unfamiliar, fall back to:

1. `references/jq_recipes.md` for ad-hoc queries
2. `hydra-node/src/Hydra/HeadLogic.hs` source — search for the failing tag
3. The user's memory at `~/.claude/projects/-home-v0d1ch-code-hydra/memory/MEMORY.md` for recent project context

When a novel failure is fully diagnosed, propose adding it to this file.
