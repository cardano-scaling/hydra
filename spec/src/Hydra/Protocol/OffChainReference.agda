-- Executable, decidable reference for the OFF-CHAIN HeadLogic decisions (the §6 figure).
--
-- Off-chain analog of `Reference.agda`: kept self-contained over `Agda.Builtin` types so MAlonzo
-- extracts it to clean Haskell, which an off-chain differential test runs as a second oracle against
-- `Hydra.HeadLogic`. (See `Reference.agda`'s header for why the imports stay minimal: Prelude/OffChain
-- and stdlib do not extract / balloon the generated tree.)
--
-- Decisions covered (off-chain differential), one decidable function per handler guard: the
-- deposit-status transition of the §6 `tick` handler (`depositStatusRef`), the reqSn
-- signing-eligibility and reqDec eligibility checks (`signEligibleRef`/`reqDecEligibleRef`, the
-- latter over the four-case `PendingCommitᶜ` rather than a Bool, so WHICH commits block a decommit
-- is decided here and not by whoever projects the node state), the
-- reqSn incremental-action guards (`reqSnNotBothRef`/`reqSnDecommitOutputsRef`/
-- `reqSnDepositSettledRef`/`reqSnDecommitSettledRef`/`reqSnDepositDisjointRef`), the ackSn
-- no-double-sign and all-signed guards
-- (`notAlreadySignedRef`/`allSignedRef`), the close/contest eligibility gate
-- (`contestEligibleRef`), and round-robin leader selection (`leaderRef`).
-- `reqTxOutcomeRef` additionally models terminal outcomes and ordered change/effect tags for one
-- reqTx attempt in the bounded domain documented beside its definition.
-- The Haskell retry-window search and settlement/rollback sequences are separate
-- integration checks. This reference has no scheduler, chain history or restart
-- semantics; agreement with it does not establish progress or rollback safety.
-- Payload evaluation and Haskell strictness are outside this projection too.
module Hydra.Protocol.OffChainReference where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List

-- ── boundary types, bound to clean Haskell at extraction ──────────────────────────────────

-- Deposit lifecycle status (§6 `depositObj.status` / the 𝒟 registry).
data DepositStatusᶜ : Set where
  inactiveᶜ activeᶜ expiredᶜ : DepositStatusᶜ
{-# FOREIGN GHC data HsDepositStatus = InactiveS | ActiveS | ExpiredS deriving (Eq, Show) #-}
{-# COMPILE GHC DepositStatusᶜ = data HsDepositStatus (InactiveS | ActiveS | ExpiredS) #-}

-- ── small helper ──────────────────────────────────────────────────────────────────────────

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y

-- ── the decidable decision ──────────────────────────────────────────────────────────────────

-- The §6 `tick` deposit-status decision for one deposit object, as a pure function of times (all
-- POSIXTime; Nat truncated subtraction, matching the on-chain Reference's time arithmetic):
--   on (tick, t):  if  t > deadline − T_deposit     then Expired
--                  else if t > created + T_activate then Active
--                  else Inactive
-- T_deposit (expiry) and T_activate (activation) are the node's `depositPeriod` and
-- `depositActivation`, which are configured independently, so they are separate arguments here.
-- (`_<_`/`_-_`/`_+_` are the `Agda.Builtin.Nat` Bool-valued / arithmetic builtins; `t > x` is `x < t`.)
depositStatusRef : Nat → Nat → Nat → Nat → Nat → DepositStatusᶜ
depositStatusRef created deadline tDeposit tActivation t =
  if (deadline - tDeposit) < t then expiredᶜ
  else if (created + tActivation) < t then activeᶜ
  else inactiveᶜ

infixr 6 _&&_
_&&_ : Bool → Bool → Bool
true  && b = b
false && _ = false

-- The §6 reqSn signing-eligibility check (`require v = v̂ ∧ s = ŝ + 1 ∧ leader(s) = j`), as a pure
-- decision over the version/number inputs with the leader test resolved to a Bool by the caller
-- (the leader function needs the party set, supplied Haskell-side). `_==_` is the
-- `Agda.Builtin.Nat` Bool equality.
--
-- `true` means the party goes on to sign, which is what the differential compares. It does NOT say
-- which outcome a `false` produces: the node splits these three conjuncts across two outcome kinds.
-- `requireReqSn` errors out on the number (`ReqSnNumberInvalid`) and on the leader
-- (`ReqSnNotLeader`), but the version conjunct is `waitOnSnapshotVersion`, a WAIT: a follower can see
-- a ReqSn for the bumped version before its own chain handler has processed the increment/decrement
-- that bumps it, and an error would drop the message permanently (see the NOTE in HeadLogic.hs). The
-- spec figure writes all three as `require`; the wait is a deliberate implementation refinement.
signEligibleRef : Nat → Nat → Nat → Nat → Bool → Bool
signEligibleRef v vHat s sHat leaderOk = (v == vHat) && ((s == suc sHat) && leaderOk)

infixr 5 _||_
_||_ : Bool → Bool → Bool
true  || _ = true
false || b = b

not : Bool → Bool
not true  = false
not false = true

-- List membership for the signature-accumulator (Σ̂) checks.
elemᵇ : Nat → List Nat → Bool
elemᵇ _ []       = false
elemᵇ x (y ∷ ys) = (x == y) || elemᵇ x ys

-- The state of the head's recorded pending commit: the node's `currentDepositTxId` together with
-- the status of the deposit it names. Four cases rather than one Bool, because the difference
-- between them is exactly what decides whether a commit holds a decommit back, and a Bool leaves
-- that decision to whoever computes it.
data PendingCommitᶜ : Set where
  noCommitᶜ      : PendingCommitᶜ   -- no deposit id is recorded
  commitPendingᶜ : PendingCommitᶜ   -- recorded, and the deposit is Inactive or Active
  commitExpiredᶜ : PendingCommitᶜ   -- recorded, and the deposit has Expired
  commitGoneᶜ    : PendingCommitᶜ   -- recorded, but no such deposit is registered (recovered)
{-# FOREIGN GHC data HsPendingCommit = NoCommitP | CommitPendingP | CommitExpiredP | CommitGoneP
                   deriving (Eq, Show) #-}
{-# COMPILE GHC PendingCommitᶜ = data HsPendingCommit (NoCommitP | CommitPendingP | CommitExpiredP | CommitGoneP) #-}

-- Whether a recorded pending commit is one the head can still settle, which is the `U_α ≠ ∅` a
-- decommit has to wait behind. Only a registered, unexpired deposit is: an expired one is
-- unclaimable and a recovered one is gone, and neither is reachable by any later increment.
--
-- This distinction is load-bearing rather than pedantic. The node clears `currentDepositTxId` on
-- neither expiry nor recovery (`DepositExpired` keeps the deposit so it can still be recovered,
-- `DepositRecovered` only drops the map entry), so a decision that read that field alone would
-- block every later decommit on a commit that can never settle.
commitBlocksᵇ : PendingCommitᶜ → Bool
commitBlocksᵇ commitPendingᶜ = true
commitBlocksᵇ noCommitᶜ      = false
commitBlocksᵇ commitExpiredᶜ = false
commitBlocksᵇ commitGoneᶜ    = false

-- reqDec eligibility (the §6 `wait U_α = ∅ ∧ tx_ω = ⊥`): neither a settleable commit nor a decommit
-- in flight.
reqDecEligibleRef : PendingCommitᶜ → Bool → Bool   -- (pending commit, decommitInFlight)
reqDecEligibleRef commit decommit = not (commitBlocksᵇ commit) && not decommit

-- reqSn incremental-action exclusivity (the §6 `require tx_ω = ⊥ ∨ tx_α = ⊥`, node
-- `ReqSnBothCommitAndDecommit`): a snapshot request may carry a commit (deposit) or a decommit,
-- never both - close and fanout express a single incremental action, so a snapshot settling both
-- would be unclosable.
reqSnNotBothRef : Bool → Bool → Bool   -- (hasDeposit, hasDecommit)
reqSnNotBothRef hasDep hasDec = not (hasDep && hasDec)

-- reqSn decommit-materializes-outputs (node `ReqSnDecommitNoOutputs`): a requested decommit must
-- produce at least one output - the decrement validator materializes the decommit outputs and
-- requires one, so a no-output decommit could never settle on-chain and would be re-proposed by
-- every later snapshot. (`0 < n` is the Agda.Builtin Bool-valued `_<_`.)
reqSnDecommitOutputsRef : Nat → Bool   -- number of outputs the requested decommit produces
reqSnDecommitOutputsRef n = 0 < n

-- reqSn same-version deposit settlement (node `waitForDeposit`, the `ReqSnCommitNotSettled`
-- require): when the request is at the same version as the confirmed snapshot and that snapshot
-- carries a pending commit, the requested deposit must be the same deposit, matched by identity
-- (deposit tx-id, `confDepositTxId == Just depositTxId`) on top of content: two deposits can record
-- the same UTxO, and only the one bound into the confirmed snapshot's κ# is the pending commit
-- being settled. The tx-ids are supplied as deterministic Integer encodings (as the on-chain
-- reference's `refCodeOf`); `contentOk` is the caller-resolved `confUTxOToCommit == Just deposited`.
reqSnDepositSettledRef : Bool → Nat → Nat → Bool   -- (contentOk, confirmed dep tx-id, requested)
reqSnDepositSettledRef contentOk confDep reqDep = contentOk && (confDep == reqDep)

-- reqSn same-version decommit settlement (node `requireApplicableDecommitTx`, the
-- `ReqSnDecommitNotSettled` require): when the request is at the same version as the confirmed
-- snapshot and that snapshot carries a pending decommit, the request must carry a decommit with the
-- same outputs. A settling decommit is never dropped - its decrement is posted again with every
-- snapshot carrying it, and a leader stops carrying it only at the bumped version - so a request
-- without one is rejected like one with another decommit (or with a deposit in its place, which
-- carries no decommit either). `contentOk` is the caller-resolved
-- `confUTxOToDecommit == Just (utxoFromTx decommitTx)`, false when no decommit is requested.
reqSnDecommitSettledRef : Bool → Bool → Bool   -- (hasDecommit, contentOk)
reqSnDecommitSettledRef hasDec contentOk = hasDec && contentOk

-- reqSn deposit disjointness (the §6 `require U_α ∩ U_active = ∅ ∧ U_α ∩ outputs(Treq) = ∅`,
-- node `absorbable` / `ReqSnDepositOutputsHeld`). Deposit output names are chosen by the depositor,
-- so they must be disjoint from both the active ledger and ALL outputs the request creates,
-- including ones a later requested transaction spends. Checking only the final UTxO set would
-- miss consumed names on either side. The three sets are lists of deterministic Nat encodings
-- of their TxIns.
disjointᵇ : List Nat → List Nat → Bool
disjointᵇ []       _  = true
disjointᵇ (x ∷ xs) ys = not (elemᵇ x ys) && disjointᵇ xs ys
reqSnDepositDisjointRef : List Nat → List Nat → List Nat → Bool   -- (active, created, deposit TxIns)
reqSnDepositDisjointRef active created deposit = disjointᵇ deposit active && disjointᵇ deposit created

-- ackSn-collect (the §6 `require (j,·) ∉ Σ̂`): sender j has not already signed this round.
notAlreadySignedRef : List Nat → Nat → Bool   -- (Σ̂ signer indices, j)
notAlreadySignedRef signers j = not (elemᵇ j signers)

-- ackSn-confirm (the §6 `if ∀ k ∈ [1..n] : (k,·) ∈ Σ̂`): every party index below n has signed (n-of-n).
allBelowᵇ : Nat → List Nat → Bool
allBelowᵇ zero    _       = true
allBelowᵇ (suc k) signers = elemᵇ k signers && allBelowᵇ k signers
allSignedRef : Nat → List Nat → Bool     -- (n, Σ̂ signer indices)
allSignedRef n signers = allBelowᵇ n signers

-- Contest re-post (the §6 `if S̄.s > s_c`): our confirmed snapshot is newer than the closed/contested one.
contestEligibleRef : Nat → Nat → Bool    -- (S̄.s, s_c)
contestEligibleRef sBar sc = sc < sBar

modSuc : Nat → Nat → Nat   -- modSuc a b = a mod (suc b), via the Agda.Builtin `mod-helper` primitive
modSuc a b = mod-helper 0 b a b

-- Round-robin leader, matching the node's `isLeader` (HeadLogic.hs): for `suc m` parties and snapshot
-- number sn, the party at 0-based index i is the leader iff (sn - 1) mod (suc m) == i.
--
-- Written `(sn + m) mod (suc m)` rather than `(sn - 1) mod (suc m)` so it agrees with the node at
-- EVERY sn, including 0. The node computes `(fromIntegral sn - 1) mod length parties` over signed
-- Int, so at sn = 0 it asks for `(-1) mod n`, which Haskell returns as n - 1 = m; Agda's `_-_` on ℕ
-- truncates to 0 instead, which would name party 0 and make the oracle confidently disagree with
-- the implementation on a reachable input (a ReqSn can carry sn = 0). Adding m is the same residue
-- as subtracting 1 for every sn ≥ 1, and yields m at sn = 0.
leaderRef : Nat → Nat → Nat → Bool   -- (m where #parties = suc m, sn, party index i)
leaderRef m sn i = modSuc (sn + m) m == i

-- A complete outcome shape for a bounded reqTx attempt, rather than an acceptance Bool.
-- The domain has no deposits or decommit, no pending local transactions, and a local ledger equal
-- to the settled confirmed ledger. Transaction outputs have fresh names. A successful application
-- therefore supplies a requestable tx.
-- Applicability is the ledger primitive's answer; leader selection remains a decision of this model.
-- Transaction/UTxO payloads, collision checks, and retry scheduling remain outside this projection.
data ReqTxTerminalᶜ : Set where
  reqTxFinishedᶜ reqTxWaitingᶜ : ReqTxTerminalᶜ
{-# FOREIGN GHC data HsReqTxTerminal = ReqTxFinished | ReqTxWaiting deriving (Eq, Show) #-}
{-# COMPILE GHC ReqTxTerminalᶜ = data HsReqTxTerminal (ReqTxFinished | ReqTxWaiting) #-}

data ReqTxChangeᶜ : Set where
  reqTxReceivedᶜ reqTxAppliedᶜ reqTxInvalidᶜ reqTxSnapshotDecidedᶜ : ReqTxChangeᶜ
{-# FOREIGN GHC data HsReqTxChange = ReqTxReceived | ReqTxApplied | ReqTxInvalid | ReqTxSnapshotDecided
                   deriving (Eq, Show) #-}
{-# COMPILE GHC ReqTxChangeᶜ = data HsReqTxChange (ReqTxReceived | ReqTxApplied | ReqTxInvalid | ReqTxSnapshotDecided) #-}

data ReqTxEffectᶜ : Set where
  reqTxRequestSnapshotᶜ : ReqTxEffectᶜ
{-# FOREIGN GHC data HsReqTxEffect = ReqTxRequestSnapshot deriving (Eq, Show) #-}
{-# COMPILE GHC ReqTxEffectᶜ = data HsReqTxEffect (ReqTxRequestSnapshot) #-}

record ReqTxOutcomeᶜ : Set where
  constructor reqTxOutcomeᶜ
  field
    reqTxTerminalᶜ : ReqTxTerminalᶜ
    reqTxChangesᶜ : List ReqTxChangeᶜ
    reqTxEffectsᶜ : List ReqTxEffectᶜ
{-# FOREIGN GHC data HsReqTxOutcome = HsReqTxOutcome
                   { reqTxTerminal :: HsReqTxTerminal
                   , reqTxChanges :: [HsReqTxChange]
                   , reqTxEffects :: [HsReqTxEffect]
                   } deriving (Eq, Show) #-}
{-# COMPILE GHC ReqTxOutcomeᶜ = data HsReqTxOutcome (HsReqTxOutcome) #-}

-- Inputs: ledger applicability, remaining TTL, snapshot in flight, party count minus one,
-- next snapshot number, and this party's zero-based index. No Error outcome is reachable here:
-- exhausted TTL records an invalid transaction and completes normally.
reqTxOutcomeRef : Bool → Nat → Bool → Nat → Nat → Nat → ReqTxOutcomeᶜ
reqTxOutcomeRef false zero _ _ _ _ =
  reqTxOutcomeᶜ reqTxFinishedᶜ (reqTxReceivedᶜ ∷ reqTxInvalidᶜ ∷ []) []
reqTxOutcomeRef false (suc _) _ _ _ _ =
  reqTxOutcomeᶜ reqTxWaitingᶜ (reqTxReceivedᶜ ∷ []) []
reqTxOutcomeRef true _ inFlight m nextSn partyIndex =
  if not inFlight && leaderRef m nextSn partyIndex then
    reqTxOutcomeᶜ reqTxFinishedᶜ
      (reqTxReceivedᶜ ∷ reqTxAppliedᶜ ∷ reqTxSnapshotDecidedᶜ ∷ []) (reqTxRequestSnapshotᶜ ∷ [])
  else
    reqTxOutcomeᶜ reqTxFinishedᶜ (reqTxReceivedᶜ ∷ reqTxAppliedᶜ ∷ []) []

-- ══ extraction surface: the stable Haskell names ═══════════════════════════════════════════════
-- See `Reference.agda`'s section of the same name: MAlonzo's mangled names carry a definition-order
-- index that any additive edit above renumbers, so the shim binds these fixed names instead. Every
-- decision here is already FFI-translatable (Nat/Bool/List/DepositStatusᶜ), hence no wrappers.

{-# COMPILE GHC depositStatusRef        as hsDepositStatusRef        #-}
{-# COMPILE GHC signEligibleRef         as hsSignEligibleRef         #-}
{-# COMPILE GHC reqDecEligibleRef       as hsReqDecEligibleRef       #-}
{-# COMPILE GHC reqSnNotBothRef         as hsReqSnNotBothRef         #-}
{-# COMPILE GHC reqSnDecommitOutputsRef as hsReqSnDecommitOutputsRef #-}
{-# COMPILE GHC reqSnDepositSettledRef  as hsReqSnDepositSettledRef  #-}
{-# COMPILE GHC reqSnDecommitSettledRef as hsReqSnDecommitSettledRef #-}
{-# COMPILE GHC reqSnDepositDisjointRef as hsReqSnDepositDisjointRef #-}
{-# COMPILE GHC notAlreadySignedRef     as hsNotAlreadySignedRef     #-}
{-# COMPILE GHC allSignedRef            as hsAllSignedRef            #-}
{-# COMPILE GHC contestEligibleRef      as hsContestEligibleRef      #-}
{-# COMPILE GHC leaderRef               as hsLeaderRef               #-}

-- Capacity policy for the deployed 32768-power setup. Counts are over output
-- references, after forming each union; overlapping entries count only once.
accumulatorCapacityRef : Nat
accumulatorCapacityRef = 32767

snapshotCapacityRef : Nat → Nat → Bool
snapshotCapacityRef withDecommit withCommit =
  (withDecommit < suc accumulatorCapacityRef) && (withCommit < suc accumulatorCapacityRef)

-- Admission counts include the decommit's outputs before they are excluded
-- from the spendable ledger. Pending local merges cannot provide confirmed space.
decommitCapacityRef : Nat → Nat → Nat → Bool
decommitCapacityRef outputCount localAfter confirmedAfter =
  (0 < outputCount) && snapshotCapacityRef localAfter confirmedAfter

-- A ledger-valid pending transaction can still be pruned for a held-output
-- collision or either fanout set exceeding the accumulator capacity.
pendingTxFitsRef : Bool → Bool → Nat → Nat → Bool
pendingTxFitsRef applicable outputsDisjoint withDecommit withCommit =
  applicable && outputsDisjoint && snapshotCapacityRef withDecommit withCommit

{-# COMPILE GHC accumulatorCapacityRef as hsAccumulatorCapacityRef #-}
{-# COMPILE GHC snapshotCapacityRef    as hsSnapshotCapacityRef    #-}
{-# COMPILE GHC decommitCapacityRef    as hsDecommitCapacityRef    #-}
{-# COMPILE GHC pendingTxFitsRef       as hsPendingTxFitsRef       #-}
{-# COMPILE GHC reqTxOutcomeRef         as hsReqTxOutcomeRef         #-}

-- Bounded snapshot outcomes use named terminals, reasons, changes and effects.
-- Domain: no incremental actions, fresh output names, bounded accumulators and
-- three authenticated parties. Payload validity, reconciliation, chain progress
-- and retry scheduling are checked separately; this model does not prove them.
data SnapshotPhaseᶜ : Set where
  neverSeenᶜ requestPendingᶜ collectingSignaturesᶜ lastConfirmedᶜ : SnapshotPhaseᶜ
{-# FOREIGN GHC data HsSnapshotPhase = NeverSeen | RequestPending | CollectingSignatures | LastConfirmed
                   deriving (Eq, Show, Enum, Bounded) #-}
{-# COMPILE GHC SnapshotPhaseᶜ = data HsSnapshotPhase (NeverSeen | RequestPending | CollectingSignatures | LastConfirmed) #-}

data SnapshotChangeᶜ : Set where
  recordSignatureᶜ confirmSnapshotᶜ decideNextSnapshotᶜ recordSnapshotᶜ : SnapshotChangeᶜ
{-# FOREIGN GHC data HsSnapshotChange = RecordSignature | ConfirmSnapshot | DecideNextSnapshot | RecordSnapshot
                   deriving (Eq, Show) #-}
{-# COMPILE GHC SnapshotChangeᶜ = data HsSnapshotChange (RecordSignature | ConfirmSnapshot | DecideNextSnapshot | RecordSnapshot) #-}

data SnapshotEffectᶜ : Set where
  sendAcknowledgementᶜ sendSnapshotRequestᶜ : SnapshotEffectᶜ
{-# FOREIGN GHC data HsSnapshotEffect = SendAcknowledgement | SendSnapshotRequest deriving (Eq, Show) #-}
{-# COMPILE GHC SnapshotEffectᶜ = data HsSnapshotEffect (SendAcknowledgement | SendSnapshotRequest) #-}

data SnapshotWaitᶜ : Set where
  previousUnconfirmedᶜ versionMismatchᶜ missingTransactionsᶜ snapshotNotSeenᶜ : SnapshotWaitᶜ
{-# FOREIGN GHC data HsSnapshotWait = PreviousUnconfirmed | VersionMismatch | MissingTransactions | SnapshotNotSeen
                   deriving (Eq, Show) #-}
{-# COMPILE GHC SnapshotWaitᶜ = data HsSnapshotWait (PreviousUnconfirmed | VersionMismatch | MissingTransactions | SnapshotNotSeen) #-}

data SnapshotRejectionᶜ : Set where
  invalidNumberᶜ notLeaderᶜ transactionDoesNotApplyᶜ alreadySignedᶜ invalidSignatureSetᶜ : SnapshotRejectionᶜ
{-# FOREIGN GHC data HsSnapshotRejection = InvalidNumber | NotLeader | TransactionDoesNotApply | AlreadySigned | InvalidSignatureSet
                   deriving (Eq, Show) #-}
{-# COMPILE GHC SnapshotRejectionᶜ = data HsSnapshotRejection (InvalidNumber | NotLeader | TransactionDoesNotApply | AlreadySigned | InvalidSignatureSet) #-}

data SnapshotOutcomeᶜ : Set where
  snapshotCompletedᶜ : List SnapshotChangeᶜ → List SnapshotEffectᶜ → SnapshotOutcomeᶜ
  snapshotWaitingᶜ : SnapshotWaitᶜ → SnapshotOutcomeᶜ
  snapshotRejectedᶜ : SnapshotRejectionᶜ → SnapshotOutcomeᶜ
{-# FOREIGN GHC data HsSnapshotOutcome
                   = SnapshotCompleted [HsSnapshotChange] [HsSnapshotEffect]
                   | SnapshotWaiting HsSnapshotWait
                   | SnapshotRejected HsSnapshotRejection
                   deriving (Eq, Show) #-}
{-# COMPILE GHC SnapshotOutcomeᶜ = data HsSnapshotOutcome (SnapshotCompleted | SnapshotWaiting | SnapshotRejected) #-}

reqSnOutcomeRef : Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Bool → SnapshotOutcomeᶜ
reqSnOutcomeRef seen confirmed localVersion requestedVersion sn m sender missing applicable =
  if not (sn == suc seen) then snapshotRejectedᶜ invalidNumberᶜ
  else if not (leaderRef m sn sender) then snapshotRejectedᶜ notLeaderᶜ
  else if not (confirmed == seen) then snapshotWaitingᶜ previousUnconfirmedᶜ
  else if not (localVersion == requestedVersion) then snapshotWaitingᶜ versionMismatchᶜ
  else if 0 < missing then snapshotWaitingᶜ missingTransactionsᶜ
  else if not applicable then snapshotRejectedᶜ transactionDoesNotApplyᶜ
  else snapshotCompletedᶜ (recordSnapshotᶜ ∷ []) (sendAcknowledgementᶜ ∷ [])

isLastConfirmed : SnapshotPhaseᶜ → Bool
isLastConfirmed lastConfirmedᶜ = true
isLastConfirmed _ = false

isCollecting : SnapshotPhaseᶜ → Bool
isCollecting collectingSignaturesᶜ = true
isCollecting _ = false

-- Partial collection retains the compatibility event order. Invalid final
-- verification discards this input's tentative signature, not earlier inputs.
ackSnOutcomeRef : SnapshotPhaseᶜ → Nat → Nat → List Nat → Nat → Nat → Bool → Bool → Nat → SnapshotOutcomeᶜ
ackSnOutcomeRef phase seen sn signers sender m verifies useful localParty =
  if not ((sn == seen) || (sn == suc seen)) then snapshotRejectedᶜ invalidNumberᶜ
  else if isLastConfirmed phase && (sn == seen) then snapshotCompletedᶜ [] []
  else if not (isCollecting phase && (sn == seen)) then snapshotWaitingᶜ snapshotNotSeenᶜ
  else if elemᵇ sender signers then snapshotRejectedᶜ alreadySignedᶜ
  else if not (allSignedRef (suc m) (sender ∷ signers)) then
    snapshotCompletedᶜ (recordSignatureᶜ ∷ recordSignatureᶜ ∷ []) []
  else if not verifies then snapshotRejectedᶜ invalidSignatureSetᶜ
  else if useful && leaderRef m (suc sn) localParty then
    snapshotCompletedᶜ (recordSignatureᶜ ∷ confirmSnapshotᶜ ∷ decideNextSnapshotᶜ ∷ []) (sendSnapshotRequestᶜ ∷ [])
  else snapshotCompletedᶜ (recordSignatureᶜ ∷ confirmSnapshotᶜ ∷ []) []

-- An already-applied ID is a no-op before receipt, validation or timeout. This
-- protects the validated payload against both witness replacement and deletion.
reqTxKnownOutcomeRef : Bool → Bool → Nat → Bool → Nat → Nat → Nat → ReqTxOutcomeᶜ
reqTxKnownOutcomeRef true _ _ _ _ _ _ = reqTxOutcomeᶜ reqTxFinishedᶜ [] []
reqTxKnownOutcomeRef false applicable ttl inFlight m sn party = reqTxOutcomeRef applicable ttl inFlight m sn party

{-# COMPILE GHC reqSnOutcomeRef as hsReqSnOutcomeRef #-}
{-# COMPILE GHC ackSnOutcomeRef as hsAckSnOutcomeRef #-}
{-# COMPILE GHC reqTxKnownOutcomeRef as hsReqTxKnownOutcomeRef #-}

-- Retained-event phase projections for the crem migration. These are replay
-- semantics, not admission rules. Checkpoints replace the complete machine;
-- foreign-head filtering occurs before these functions. Payloads, signature
-- maps, versions and slot history are compared separately against the frozen
-- reducer. The Booleans below are explicit abstraction inputs, not proofs.
data LifecyclePhaseᶜ : Set where
  idleHeadᶜ openHeadᶜ closedHeadᶜ readyHeadᶜ pendingFanoutᶜ fanoutProgressᶜ : LifecyclePhaseᶜ
{-# FOREIGN GHC data HsLifecyclePhase = IdleHead | OpenHead | ClosedHead | ReadyHead | PendingFanout | FanoutProgress deriving (Eq, Show, Enum, Bounded) #-}
{-# COMPILE GHC LifecyclePhaseᶜ = data HsLifecyclePhase (IdleHead | OpenHead | ClosedHead | ReadyHead | PendingFanout | FanoutProgress) #-}

data LifecycleEventᶜ : Set where
  openedᶜ closedᶜ readyᶜ fanoutStartedᶜ partialObservedᶜ fanoutRevertedᶜ fannedOutᶜ unchangedᶜ : LifecycleEventᶜ
{-# FOREIGN GHC data HsLifecycleEvent = Opened | Closed | Ready | FanoutStarted | PartialObserved | FanoutReverted | FannedOut | Unchanged deriving (Eq, Show, Enum, Bounded) #-}
{-# COMPILE GHC LifecycleEventᶜ = data HsLifecycleEvent (Opened | Closed | Ready | FanoutStarted | PartialObserved | FanoutReverted | FannedOut | Unchanged) #-}

-- distributed means the accumulated outputs after this observation are nonempty.
lifecyclePhaseRef : LifecyclePhaseᶜ → LifecycleEventᶜ → Bool → LifecyclePhaseᶜ
lifecyclePhaseRef _ openedᶜ _ = openHeadᶜ
lifecyclePhaseRef openHeadᶜ closedᶜ _ = closedHeadᶜ
lifecyclePhaseRef closedHeadᶜ readyᶜ _ = readyHeadᶜ
lifecyclePhaseRef closedHeadᶜ fanoutStartedᶜ _ = pendingFanoutᶜ
lifecyclePhaseRef readyHeadᶜ fanoutStartedᶜ _ = pendingFanoutᶜ
lifecyclePhaseRef closedHeadᶜ partialObservedᶜ distributed = if distributed then fanoutProgressᶜ else pendingFanoutᶜ
lifecyclePhaseRef readyHeadᶜ partialObservedᶜ distributed = if distributed then fanoutProgressᶜ else pendingFanoutᶜ
lifecyclePhaseRef pendingFanoutᶜ partialObservedᶜ distributed = if distributed then fanoutProgressᶜ else pendingFanoutᶜ
lifecyclePhaseRef fanoutProgressᶜ partialObservedᶜ distributed = if distributed then fanoutProgressᶜ else pendingFanoutᶜ
lifecyclePhaseRef pendingFanoutᶜ fanoutRevertedᶜ _ = readyHeadᶜ
lifecyclePhaseRef fanoutProgressᶜ fanoutRevertedᶜ _ = readyHeadᶜ
lifecyclePhaseRef closedHeadᶜ fannedOutᶜ _ = idleHeadᶜ
lifecyclePhaseRef readyHeadᶜ fannedOutᶜ _ = idleHeadᶜ
lifecyclePhaseRef pendingFanoutᶜ fannedOutᶜ _ = idleHeadᶜ
lifecyclePhaseRef fanoutProgressᶜ fannedOutᶜ _ = idleHeadᶜ
lifecyclePhaseRef phase _ _ = phase

-- Coordination is absent outside Open. A request can replace an in-flight
-- snapshot in raw replay; protocol guards normally prevent that replacement.
data CoordinationPhaseᶜ : Set where
  outsideHeadᶜ noRequestSeenᶜ snapshotIdleᶜ requestSentᶜ collectingᶜ : CoordinationPhaseᶜ
{-# FOREIGN GHC data HsCoordinationPhase = OutsideHead | NoRequestSeen | SnapshotIdle | RequestSent | Collecting deriving (Eq, Show, Enum, Bounded) #-}
{-# COMPILE GHC CoordinationPhaseᶜ = data HsCoordinationPhase (OutsideHead | NoRequestSeen | SnapshotIdle | RequestSent | Collecting) #-}

data CoordinationEventᶜ : Set where
  headOpenedᶜ headClosedᶜ requestDecidedᶜ snapshotRequestedᶜ snapshotConfirmedᶜ localClearedᶜ settlementObservedᶜ otherEventᶜ : CoordinationEventᶜ
{-# FOREIGN GHC data HsCoordinationEvent = HeadOpened | HeadClosed | RequestDecided | SnapshotRequested | SnapshotConfirmed | LocalCleared | SettlementObserved | OtherEvent deriving (Eq, Show, Enum, Bounded) #-}
{-# COMPILE GHC CoordinationEventᶜ = data HsCoordinationEvent (HeadOpened | HeadClosed | RequestDecided | SnapshotRequested | SnapshotConfirmed | LocalCleared | SettlementObserved | OtherEvent) #-}

coordinationPhaseRef : CoordinationPhaseᶜ → CoordinationEventᶜ → Bool → Bool → Bool → CoordinationPhaseᶜ
coordinationPhaseRef _ headOpenedᶜ _ _ _ = noRequestSeenᶜ
coordinationPhaseRef _ headClosedᶜ _ _ _ = outsideHeadᶜ
coordinationPhaseRef outsideHeadᶜ _ _ _ _ = outsideHeadᶜ
coordinationPhaseRef _ requestDecidedᶜ _ _ _ = requestSentᶜ
coordinationPhaseRef _ snapshotRequestedᶜ _ _ _ = collectingᶜ
coordinationPhaseRef collectingᶜ snapshotConfirmedᶜ _ _ _ = snapshotIdleᶜ
coordinationPhaseRef phase snapshotConfirmedᶜ _ explicit _ = if explicit then snapshotIdleᶜ else phase
coordinationPhaseRef _ localClearedᶜ initial _ _ = if initial then noRequestSeenᶜ else snapshotIdleᶜ
coordinationPhaseRef collectingᶜ settlementObservedᶜ _ _ _ = collectingᶜ
coordinationPhaseRef phase settlementObservedᶜ _ _ fresh = if fresh then snapshotIdleᶜ else phase
coordinationPhaseRef phase _ _ _ _ = phase

-- The tracked deposit has a status and a separate consuming observation. A
-- rollback restores the stored status; it does not recompute it from time.
data TrackedPhaseᶜ : Set where
  missingDepositᶜ inactiveDepositᶜ activeDepositᶜ expiredDepositᶜ consumedDepositᶜ : TrackedPhaseᶜ
{-# FOREIGN GHC data HsTrackedPhase = MissingDeposit | InactiveDeposit | ActiveDeposit | ExpiredDeposit | ConsumedDeposit deriving (Eq, Show, Enum, Bounded) #-}
{-# COMPILE GHC TrackedPhaseᶜ = data HsTrackedPhase (MissingDeposit | InactiveDeposit | ActiveDeposit | ExpiredDeposit | ConsumedDeposit) #-}

statusPhase : DepositStatusᶜ → TrackedPhaseᶜ
statusPhase inactiveᶜ = inactiveDepositᶜ
statusPhase activeᶜ = activeDepositᶜ
statusPhase expiredᶜ = expiredDepositᶜ

-- action: 0 observe, 1 update, 2 consume, 3 rollback. status is the new
-- payload status for observe/update, otherwise the retained payload status.
trackedDepositPhaseRef : TrackedPhaseᶜ → Nat → DepositStatusᶜ → Bool → Bool → TrackedPhaseᶜ
trackedDepositPhaseRef _ 0 status _ _ = statusPhase status
trackedDepositPhaseRef missingDepositᶜ _ _ _ _ = missingDepositᶜ
trackedDepositPhaseRef consumedDepositᶜ 1 _ _ _ = consumedDepositᶜ
trackedDepositPhaseRef _ 1 status _ _ = statusPhase status
trackedDepositPhaseRef _ 2 _ _ _ = consumedDepositᶜ
trackedDepositPhaseRef _ 3 _ true _ = missingDepositᶜ
trackedDepositPhaseRef consumedDepositᶜ 3 status false true = statusPhase status
trackedDepositPhaseRef phase _ _ _ _ = phase

-- Pending action projection: outside=0, none=1, deposit=2, decommit=3, both=4.
-- The last case is representable in persisted data; admission prevents it.
pendingIncrementPhaseRef : Bool → Bool → Bool → Nat
pendingIncrementPhaseRef false _ _ = 0
pendingIncrementPhaseRef true false false = 1
pendingIncrementPhaseRef true true false = 2
pendingIncrementPhaseRef true false true = 3
pendingIncrementPhaseRef true true true = 4

{-# COMPILE GHC lifecyclePhaseRef as hsLifecyclePhaseRef #-}
{-# COMPILE GHC coordinationPhaseRef as hsCoordinationPhaseRef #-}
{-# COMPILE GHC trackedDepositPhaseRef as hsTrackedDepositPhaseRef #-}
{-# COMPILE GHC pendingIncrementPhaseRef as hsPendingIncrementPhaseRef #-}

-- ReqTx, ReqSn and AckSn comparisons run against the frozen free programs and
-- the production crem phase machines with program actions. This reference is
-- independent of either control-flow encoding. Phase typing adds no coverage
-- of primitive correctness, payloads or Haskell evaluation to this projection.

-- ReqDec terminal and event/effect projection. The selected-commit flag is
-- supplied after deposit selection. Accepted decommits have confirmed inputs.
-- This excludes deferred work, ledger payloads and requested snapshot contents.
-- Each result names the whole ordered outcome: invalidation has one change and
-- no effects; acceptance records one decommit and optionally sends one ReqSn.
-- In particular ReqDec does not record SnapshotRequestDecided.
data ReqDecOutcomeᶜ : Set where
  decWaitCommitᶜ decInvalidCommitᶜ decWaitExistingᶜ decInvalidExistingᶜ
    decWaitLedgerᶜ decInvalidLedgerᶜ decEmptyOutputsᶜ decRecordedᶜ decRequestedᶜ : ReqDecOutcomeᶜ
{-# FOREIGN GHC data HsReqDecOutcome = ReqDecWaitCommit | ReqDecInvalidCommit
                   | ReqDecWaitExisting | ReqDecInvalidExisting | ReqDecWaitLedger
                   | ReqDecInvalidLedger | ReqDecEmptyOutputs | ReqDecRecorded
                   | ReqDecRequested deriving (Eq, Show) #-}
{-# COMPILE GHC ReqDecOutcomeᶜ = data HsReqDecOutcome
    (ReqDecWaitCommit | ReqDecInvalidCommit | ReqDecWaitExisting | ReqDecInvalidExisting
    | ReqDecWaitLedger | ReqDecInvalidLedger | ReqDecEmptyOutputs | ReqDecRecorded | ReqDecRequested) #-}

-- Selected commit, existing decommit, applicability, nonempty outputs, TTL,
-- snapshot in flight, party count minus one, next number and local party index.
reqDecOutcomeRef : Bool → Bool → Bool → Bool → Nat → Bool → Nat → Nat → Nat → ReqDecOutcomeᶜ
reqDecOutcomeRef true _ _ _ zero _ _ _ _ = decInvalidCommitᶜ
reqDecOutcomeRef true _ _ _ (suc _) _ _ _ _ = decWaitCommitᶜ
reqDecOutcomeRef false true _ _ zero _ _ _ _ = decInvalidExistingᶜ
reqDecOutcomeRef false true _ _ (suc _) _ _ _ _ = decWaitExistingᶜ
reqDecOutcomeRef false false false _ zero _ _ _ _ = decInvalidLedgerᶜ
reqDecOutcomeRef false false false _ (suc _) _ _ _ _ = decWaitLedgerᶜ
reqDecOutcomeRef false false true false _ _ _ _ _ = decEmptyOutputsᶜ
reqDecOutcomeRef false false true true _ inFlight m sn party =
  if not inFlight && leaderRef m sn party then decRequestedᶜ else decRecordedᶜ

-- Recover checks registration before the finalized-increment guard. Deposit
-- status is not an admission guard here; the chain enforces the deadline.
data RecoverOutcomeᶜ : Set where
  recoveryMissingᶜ recoveryBlockedᶜ recoveryPostedᶜ : RecoverOutcomeᶜ
{-# FOREIGN GHC data HsRecoverOutcome = RecoveryMissing | RecoveryBlocked | RecoveryPosted deriving (Eq, Show) #-}
{-# COMPILE GHC RecoverOutcomeᶜ = data HsRecoverOutcome (RecoveryMissing | RecoveryBlocked | RecoveryPosted) #-}
recoverOutcomeRef : Bool → Bool → RecoverOutcomeᶜ
recoverOutcomeRef false _ = recoveryMissingᶜ
recoverOutcomeRef true true = recoveryBlockedᶜ
recoverOutcomeRef true false = recoveryPostedᶜ

{-# COMPILE GHC reqDecOutcomeRef as hsReqDecOutcomeRef #-}
{-# COMPILE GHC recoverOutcomeRef as hsRecoverOutcomeRef #-}

-- Complete-input migration: bounded decisions outside the snapshot round.
-- These projections check admission ordering and scalar guards, not the full
-- routing table, fanout multiset arithmetic, ledger/crypto, or chain validity.
-- Input kind: client=0, network=1, chain=2. Result: dispatch=0, unsafe=1,
-- unsynced client=2, wait for sync=3. Only client inputs have a size guard.
inputAdmissionRef : Nat → Bool → Bool → Nat
inputAdmissionRef 0 false _ = 1
inputAdmissionRef 0 true false = 2
inputAdmissionRef 1 _ false = 3
inputAdmissionRef _ _ _ = 0

-- Existing decommit, ledger applicability, nonempty outputs. Results follow
-- the client's priority: already pending=0, ledger invalid=1, empty=2, send=3.
clientDecommitRef : Bool → Bool → Bool → Nat
clientDecommitRef true _ _ = 0
clientDecommitRef false false _ = 1
clientDecommitRef false true false = 2
clientDecommitRef false true true = 3

-- An observed settlement can adopt a seen snapshot only at that number, with
-- its version exactly one behind the new version, matching incremental action,
-- and valid multisignature. Finalization itself does not depend on adoption.
adoptSettlementRef : Nat → Nat → Nat → Nat → Bool → Bool → Bool
adoptSettlementRef observed seen snapshotVersion newVersion actionMatches signaturesValid =
  (observed == seen) && ((suc snapshotVersion == newVersion) && (actionMatches && signaturesValid))

-- When no still-pending settlement can be reposted, retained finalized work is
-- reposted only for a rollback strictly before its observation slot.
repostFinalizedRef : Bool → Nat → Nat → Bool
repostFinalizedRef pending rolledBack observed = pending || (rolledBack < observed)

-- Remaining set empty; mode auto=0, selection=1, awaiting=2.
-- Action: wait=0, post remainder=1, post selection=2. Empty always burns tokens.
fanoutContinuationRef : Bool → Nat → Nat
fanoutContinuationRef true _ = 1
fanoutContinuationRef false 0 = 1
fanoutContinuationRef false 1 = 2
fanoutContinuationRef false _ = 0

{-# COMPILE GHC inputAdmissionRef as hsInputAdmissionRef #-}
{-# COMPILE GHC clientDecommitRef as hsClientDecommitRef #-}
{-# COMPILE GHC adoptSettlementRef as hsAdoptSettlementRef #-}
{-# COMPILE GHC repostFinalizedRef as hsRepostFinalizedRef #-}
{-# COMPILE GHC fanoutContinuationRef as hsFanoutContinuationRef #-}

-- Independent invariants for the coordinated ownership experiment. These are
-- requirements, not a translation of the current event reducer: the historical
-- decommit schedule violates both. Outputs are natural-number identities, as
-- in SimpleTx. There is no Cardano validation, reference input, script, slot or
-- settlement model here; 'carried' means the same decommit is already signed.
reservationHeldRef : List Nat → List Nat → Bool
reservationHeldRef [] available = true
reservationHeldRef (input ∷ rest) available =
  not (elemᵇ input available) && reservationHeldRef rest available

decommitReadyRef : Bool → List Nat → List Nat → Bool
decommitReadyRef true inputs confirmed = true
decommitReadyRef false [] confirmed = true
decommitReadyRef false (input ∷ rest) confirmed =
  elemᵇ input confirmed && decommitReadyRef false rest confirmed

{-# COMPILE GHC reservationHeldRef as hsReservationHeldRef #-}
{-# COMPILE GHC decommitReadyRef as hsDecommitReadyRef #-}

-- Version and reservation projection of a decrement observation. Chain rollback
-- preserves both; a repeated observation must not release a newer reservation.
settlementVersionRef : Nat → Nat → Nat
settlementVersionRef old observed = if old < observed then observed else old

settlementReservationRef : Bool → Nat → Nat → Bool
settlementReservationRef pending old observed = pending && not (old < observed)

{-# COMPILE GHC settlementVersionRef as hsSettlementVersionRef #-}
{-# COMPILE GHC settlementReservationRef as hsSettlementReservationRef #-}

-- Rebuilding the optimistic ledger must preserve the pending reservation.
reserveRebaseRef : List Nat → List Nat → List Nat
reserveRebaseRef reserved [] = []
reserveRebaseRef reserved (input ∷ rest) =
  if elemᵇ input reserved then reserveRebaseRef reserved rest
  else (input ∷ reserveRebaseRef reserved rest)

-- Newly available inputs warrant a decommit-only round. Carrying an already
-- signed decommit does not, on its own, warrant another round.
newDecommitReadyRef : Bool → List Nat → List Nat → Bool
newDecommitReadyRef carried inputs confirmed =
  not carried && decommitReadyRef false inputs confirmed

{-# COMPILE GHC reserveRebaseRef as hsReserveRebaseRef #-}
{-# COMPILE GHC newDecommitReadyRef as hsNewDecommitReadyRef #-}
