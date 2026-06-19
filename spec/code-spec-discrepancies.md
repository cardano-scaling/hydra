# Code vs spec discrepancies (`hydra-node` vs the formal spec)

This file tracks discrepancies found between the **implementation**
(`hydra-node/src/Hydra/HeadLogic.hs` and friends) and the **formal specification**
(the Typst + Agda spec in `spec/`, in particular the normative §6 off-chain figure in
`src/Hydra/Protocol/OffChain.lagda.typ` and the §5 on-chain Agda bundles in
`OnChain.lagda.typ`). It is the implementation-facing counterpart to
[`discrepancies-and-fixes.md`](./discrepancies-and-fixes.md) (which tracks spec-conversion
discrepancies).

The audit keyed off the `-- Spec:` correspondence comments the code already carries, then
verified each suspected gap by tracing the relevant handlers and aggregation.

## Severity legend

- **HIGH** — the code admits something the protocol forbids: fund loss, double-spend, or a
  stuck head.
- **MED** — a missing check that is defended elsewhere today, or a robustness/liveness gap.
- **LOW** — comment staleness / latent invariant worth asserting.

---

## C1 — HIGH (FIXED): deposit/decommit mutual exclusion was only half-enforced

**Spec.** A snapshot must never carry **both** a pending deposit/commit (`tx_α` / `U_α`) and a
pending decommit (`tx_ω`):
- `reqSn` (OffChain figure): `require tx_ω = ⊥ ∨ tx_α = ⊥`.
- `reqDec` (OffChain figure): `wait U_α = ∅ ∧ tx_ω = ⊥ ∧ L̂∘tx≠⊥`.

**Code (before fix).** The invariant was enforced in only one direction:

| Link | Location | Before |
|---|---|---|
| `onOpenNetworkReqDec` / `waitOnApplicableDecommit` | `HeadLogic.hs:~812` | checked `tx_ω=⊥` and `L̂∘tx≠⊥`, but **not** `U_α=∅` |
| `DecommitRecorded` aggregation | `HeadLogic.hs:~2231` | set `decommitTx` unconditionally; no guard on `currentDepositTxId` (cf. `DepositActivated:~2223`, which *does* guard on `decommitTx`) |
| `selectNextDeposit` | `HeadLogic.hs:~1613` | `setExistingDeposit` short-circuits the `<|>`, re-emitting the pending deposit **even when a decommit is pending** |
| `onOpenNetworkReqTx` / `maybeRequestSnapshot` | `HeadLogic.hs:~256` | emits a `ReqSn` carrying both fields |
| `onOpenNetworkReqSn` | `HeadLogic.hs:332` | `-- TODO: this is missing!?` — no `require` rejecting both |
| `onOpenNetworkAckSn` | `HeadLogic.hs:~638,663` | once confirmed, **both** `maybePostIncrementTx` and `maybePostDecrementTx` fire |

**Reachable in an honest run** (no malicious leader required):
1. A deposit goes Active; `DepositActivated` sets `currentDepositTxId` (allowed: no decommit yet).
2. `currentDepositTxId` clears only on the on-chain `CommitFinalized` observation, so there is a
   real window before the Increment is observed.
3. A client `ReqDec` arriving in that window passed `waitOnApplicableDecommit` and recorded
   `decommitTx` (no `U_α=∅` guard). Both fields now set.
4. The leader's next `ReqSn` carried both; `onOpenNetworkReqSn` did not reject it; all honest
   parties signed; the snapshot confirmed with both `utxoToCommit` and `utxoToDecommit`.
5. On confirmation the node posted **both** an `IncrementTx` and a `DecrementTx`. On-chain
   (Agda `increment`/`decrement`, `OnChain.lagda.typ`) each consumes the same `Open … v …` output
   and produces `… (suc v) …` — they are mutual double-spends. One is rejected; the losing
   branch's pending field is never cleared → **the head gets stuck** on the orphaned operation.

**Fix (applied).** Both halves of the spec invariant, plus a regression test for each:
- `onOpenNetworkReqSn`: added `requireDepositOrDecommit` → `Error (RequireFailed ReqSnDepositAndDecommit)`
  when a `ReqSn` carries both (the authoritative receiver-side guard; spec `reqSn` require). This
  is the load-bearing safety fix: no honest party will ever sign a both-pending snapshot.
- `onOpenNetworkReqDec`: added the `U_α = ∅` conjunct to `waitOnApplicableDecommit` — it now
  **waits** (`WaitOnUnresolvedCommit`) while `currentDepositTxId` is set, rather than recording a
  decommit. This preserves liveness: the decommit proceeds once the increment finalises and clears
  `currentDepositTxId` (so the receiver-side `require` is never actually hit in an honest run — the
  deposit and decommit are sequenced, deposit first). This mirrors the existing `DepositActivated`
  guard in the opposite direction.
- New `RequirementFailure` constructor `ReqSnDepositAndDecommit` (`HeadLogic/Error.hs`).
- Tests in `HeadLogicSpec.hs`: "rejects a ReqSn carrying both a deposit and a decommit" and
  "waits on a ReqDec while a commit (deposit) is pending".

**Status: fixed**, library + tests compile, all `HeadLogic` tests pass, `just lint` clean.

---

## C2 — MED (open): rollback does not reset off-chain head state

**Spec.** §6 (OffChain figure, rollback handling) requires a per-chain-point history Ω and a
**full** restore of `(v̂, ŝ, Û, Σ̂, L̂, T̂, S̄)` to the state at the maximal `p ≤ p_rb` on a
rollback.

**Code.** `applyEvent ChainRolledBack` (`HeadLogic.hs:~2338`) only swaps the opaque chain-state
pointer (`setChainState`); `version`, `confirmedSnapshot`, and the Open/Closed discriminator are
**not** reverted. `HeadLogic/State.hs:~34` admits this in a comment. `maybeRepostIncrementTx` /
`maybeRepostDecrementTx` (`HeadLogic.hs:~1086-1140`) only re-submit an *in-flight* L1
increment/decrement and are gated on `currentDepositTxId`/`decommitTx` still being `Just` — which
become `Nothing` once `CommitFinalized`/`DecommitFinalized` fires. So a rollback **past an
already-finalized** increment/decrement/close leaves off-chain state ahead of chain with no
compensation.

**Consequence.** Off-chain/on-chain divergence after deep rollbacks; potentially a stuck head.
Systemic and acknowledged in-code; fixing it is a larger piece of work (the history Ω).

**Status: open** (not fixed in this pass — out of scope for the targeted HIGH fix). Re-confirmed in
the six-direction consistency audit (2026-06-19) and deliberately kept as a deferred follow-up note:
the full per-chain-point history Ω restore is a large, self-contained change and is not bundled into
the spec/Agda consistency work.

---

## C3 — MED/latent (open): unchecked close/contest version-discipline invariant

**Code.** `onOpenChainCloseTx` (`HeadLogic.hs:~1200`) and `onClosedChainContestTx` (`~1321`) carry
`-- XXX: As we use 'version' in the contest here, this implies that our last 'confirmedSnapshot'
must match version or version-1. Assert this fact?`. The `Used`/`Unused` close/contest redeemer
choice (and the on-chain `v-1` signature check, matching Agda `closeSigOK`/`contestSigOK`) is
correct **only if** `confirmedSnapshot.version ∈ {version, version-1}`.

**Assessment.** The invariant **does** hold today (version bumps by exactly 1 per
increment/decrement, and a confirmed snapshot is chained before the next bump), so there is **no
bug now**. But it is unchecked; a future change that broke it would post a contest whose `Used`
signature verifies against the wrong version → rejected by L1 → the head could not be
defended/contested in time. Worth converting the `XXX` into a real `assert`.

**Status: open** (latent; no current bug). Re-confirmed in the six-direction consistency audit
(2026-06-19) and kept as a deferred follow-up note. Recommended fix when taken up: convert the `XXX`
into a runtime assertion (`confirmedSnapshot.version ∈ {version, version-1}`) with a regression test,
in a focused PR rather than bundled into the consistency work.

---

## C4 — LOW (open): stale `-- Spec:` comments on close/contest

`onOpenClientClose` / `onOpenChainCloseTx` / `onClosedChainContestTx`
(`HeadLogic.hs:~1158,1203,1324`) carry `-- Spec: η ← combine(̅S.𝑈)`, while the current spec reads
the **stored** `S̄.(η')#` (the close/contest routines do `(η')# ← S̄.(η')#`). The constructed
transaction uses the snapshot's accumulator correctly; only the comments are stale.

**Status: open** (cosmetic).

---

## Clean bills (verified correct, no issue)

- **Init lifecycle.** The simplified Idle→Open lifecycle matches spec §5 (`mtxInit` produces the
  `Open` output directly). The §5 init checks are present, split across layers: `observeInitTx`
  (`cid = hash(muHead(seed))`, ST-token, mint-count) and `onIdleChainInitTx` (parties = hydra keys,
  participants = cardano-key hashes, contestation period). Mismatch → `IgnoredHeadInitializing`.
- **`S̄.s > sc` contest gate.** `onOpenChainCloseTx`/`onClosedChainContestTx` contest only when
  `confirmedSnapshot.number > observed`, matching the OffChain figure. Equal/greater-observed
  branches correctly do not contest.
- **Fanout UTxO inclusion.** `computeFullFanoutUTxO` / `emitNextFanoutStep` include a pending commit
  iff the increment was applied on-chain (`snapshotVersion ≠ version`) and a pending decommit iff
  the decrement was **not** applied — mutually exclusive, so no UTxO is dropped or double-distributed;
  the closed-datum accumulator always commits to the full snapshot UTxO, with undistributed-but-
  committed members handled as `presettled` via the on-chain value-conservation check.
- **reqSn / ackSn discipline.** snapshot-number/version `require`/`wait` guards, signature
  verification, and all-members-signed aggregation match the OffChain figure.
- **Increment/decrement version bump** is applied in `aggregateNodeState`, not lost.
