-- | Clean Haskell surface over the MAlonzo-extracted OFF-CHAIN HeadLogic reference decisions
-- (@spec\/src\/Hydra\/Protocol\/OffChainReference.agda@). This is the off-chain analog of
-- "Hydra.Agda.Reference": the extractable half of the figure↔Agda↔Haskell correspondence for the §6
-- off-chain protocol, run as a second oracle against "Hydra.HeadLogic" in an off-chain differential
-- test. Each binding names an export the Agda source fixes with @COMPILE GHC … as …@, not a mangled
-- MAlonzo name, so regeneration after an unrelated Agda edit does not renumber them out from under
-- this shim (see the header of "Hydra.Agda.Reference").
--
-- As there, every 'Integer' stands for an Agda ℕ and callers must pass non-negative values; see that
-- module's header for what a negative one does.
module Hydra.Agda.OffChainReference (
  accumulatorCapacityRef,
  snapshotCapacityRef,
  decommitCapacityRef,
  pendingTxFitsRef,
  capacityFitsRef,
  capacityPrefixRef,
  capacityPruneRef,
  settlementVersionRef,
  settlementReservationRef,
  reservationHeldRef,
  retainReservationRef,
  decommitAdmissionRef,
  carriedDecommitPayloadRef,
  carriedDecommitRequestRef,
  verificationContextRef,
  decommitReadyRef,
  reserveRebaseRef,
  newDecommitReadyRef,
  HsReqDecOutcome (..),
  HsRecoverOutcome (..),
  inputAdmissionRef,
  clientDecommitRef,
  adoptSettlementRef,
  repostFinalizedRef,
  fanoutContinuationRef,
  reqDecOutcomeRef,
  recoverOutcomeRef,

  -- * deposit lifecycle (the @tick@ handler)
  HsLifecyclePhase (..),
  HsLifecycleEvent (..),
  HsCoordinationPhase (..),
  HsCoordinationEvent (..),
  HsTrackedPhase (..),
  HsDepositStatus (..),
  depositStatusRef,

  -- * reqSn signing eligibility
  signEligibleRef,

  -- * reqSn incremental-action guards
  reqSnNotBothRef,
  reqSnDecommitOutputsRef,
  reqSnDepositSettledRef,
  reqSnDecommitSettledRef,
  reqSnDepositDisjointRef,

  -- * reqDec / ackSn / contest guards
  HsPendingCommit (..),
  reqDecEligibleRef,
  notAlreadySignedRef,
  allSignedRef,
  contestEligibleRef,

  -- * round-robin leader (bound to the real 'Hydra.HeadLogic.isLeader')
  leaderRef,

  -- * bounded reqTx outcomes, including committed changes and effects
  HsReqTxTerminal (..),
  HsReqTxChange (..),
  HsReqTxEffect (..),
  HsReqTxOutcome (..),
  reqTxOutcomeRef,
  HsSnapshotPhase (..),
  HsSnapshotChange (..),
  HsSnapshotEffect (..),
  HsSnapshotWait (..),
  HsSnapshotRejection (..),
  HsSnapshotOutcome (..),
  reqTxKnownOutcomeRef,
  reqSnOutcomeRef,
  ackSnOutcomeRef,
  lifecyclePhaseRef,
  coordinationPhaseRef,
  trackedDepositPhaseRef,
  pendingIncrementPhaseRef,
) where

import MAlonzo.Code.Hydra.Protocol.OffChainReference (
  HsCoordinationEvent (..),
  HsCoordinationPhase (..),
  HsDepositStatus (..),
  HsLifecycleEvent (..),
  HsLifecyclePhase (..),
  HsPendingCommit (..),
  HsRecoverOutcome (..),
  HsReqDecOutcome (..),
  HsReqTxChange (..),
  HsReqTxEffect (..),
  HsReqTxOutcome (..),
  HsReqTxTerminal (..),
  HsSnapshotChange (..),
  HsSnapshotEffect (..),
  HsSnapshotOutcome (..),
  HsSnapshotPhase (..),
  HsSnapshotRejection (..),
  HsSnapshotWait (..),
  HsTrackedPhase (..),
 )
import MAlonzo.Code.Hydra.Protocol.OffChainReference qualified as M

-- | Extracted decidable deposit-status decision of the §6 @tick@ handler: given a deposit's
-- @created@ and @deadline@ times, the expiry period @T_deposit@, the activation period
-- @T_activate@ and the current time @t@ (all POSIXTime), it returns the deposit's lifecycle status.
-- Mirrors the figure's @on (tick, t)@ status transition (@t > deadline - T_deposit@ ⇒ Expired; else
-- @t > created + T_activate@ ⇒ Active; else Inactive).
--
-- The two periods are separate arguments because the node configures them separately
-- (@--deposit-period@ bounds expiry, @--deposit-activation@ the Inactive to Active transition), so
-- an oracle taking one period could only agree with the node where the two happen to be equal.
depositStatusRef :: Integer -> Integer -> Integer -> Integer -> Integer -> HsDepositStatus
depositStatusRef = M.hsDepositStatusRef

-- | Extracted decidable reqSn signing-eligibility check (the §6 @require v = v̂ ∧ s = ŝ + 1 ∧
-- leader(s) = j@): given the requested version @v@, the party's seen version @v̂@, the requested
-- snapshot number @s@, the party's seen number @ŝ@, and whether the requested leader is the sender
-- (resolved Haskell-side from the party set), decides whether the party goes on to sign.
--
-- Only that decision, not the outcome a rejection produces. @onOpenNetworkReqSn@ splits the three
-- conjuncts: @requireReqSn@ errors on the number (@ReqSnNumberInvalid@) and the leader
-- (@ReqSnNotLeader@), while the version conjunct is @waitOnSnapshotVersion@, a wait, so that a
-- follower which has not yet processed the version-bumping increment\/decrement retries instead of
-- dropping the message. The §6 figure writes all three as @require@.
signEligibleRef :: Integer -> Integer -> Integer -> Integer -> Bool -> Bool
signEligibleRef = M.hsSignEligibleRef

-- | Extracted reqDec eligibility (§6 @wait U_α = ∅ ∧ tx_ω = ⊥@): given the state of the head's
-- recorded pending commit and whether a decommit is in flight, decides whether a new decommit may
-- start.
--
-- The commit argument is 'HsPendingCommit' rather than a @Bool@ on purpose. Which commits hold a
-- decommit back is a protocol question, so the reference answers it: only a registered, unexpired
-- deposit does. A @Bool@ would have moved that decision into whatever projects the node state, and
-- the node clears its @currentDepositTxId@ on neither expiry nor recovery, so the obvious projection
-- ("is the field set") blocks decommits on commits that can never settle.
reqDecEligibleRef :: HsPendingCommit -> Bool -> Bool
reqDecEligibleRef = M.hsReqDecEligibleRef

-- | Extracted reqSn incremental-action exclusivity (§6 @require tx_ω = ⊥ ∨ tx_α = ⊥@, the node's
-- @ReqSnBothCommitAndDecommit@): given whether the request carries a deposit and whether it carries a
-- decommit, decides whether the combination is admissible (both at once is not).
reqSnNotBothRef :: Bool -> Bool -> Bool
reqSnNotBothRef = M.hsReqSnNotBothRef

-- | Extracted reqSn decommit-materializes-outputs guard (the node's @ReqSnDecommitNoOutputs@): given
-- the number of outputs the requested decommit produces, decides whether it could ever settle
-- on-chain (the decrement validator materializes the decommit outputs and requires at least one).
reqSnDecommitOutputsRef :: Integer -> Bool
reqSnDecommitOutputsRef = M.hsReqSnDecommitOutputsRef

-- | Extracted reqSn same-version deposit-settlement guard (the node's @waitForDeposit@ /
-- @ReqSnCommitNotSettled@ identity check): given whether the requested deposit's content matches the
-- confirmed snapshot's pending commit, and the Integer-encoded deposit tx-ids bound into the
-- confirmed snapshot and carried by the request, decides whether the request settles the same
-- deposit, by identity rather than content alone (two deposits can record the same UTxO).
reqSnDepositSettledRef :: Bool -> Integer -> Integer -> Bool
reqSnDepositSettledRef = M.hsReqSnDepositSettledRef

-- | Extracted reqSn same-version decommit-settlement guard (the node's
-- @requireApplicableDecommitTx@ / @ReqSnDecommitNotSettled@): given whether the request carries a
-- decommit and whether its outputs match the confirmed snapshot's pending decommit, decides whether
-- the request carries the decommit being settled. A settling decommit is never dropped, so a request
-- without one is rejected like one with another decommit.
reqSnDecommitSettledRef :: Bool -> Bool -> Bool
reqSnDecommitSettledRef = M.hsReqSnDecommitSettledRef

-- | Extracted reqSn deposit-disjointness guard (the node's @absorbable@ /
-- @ReqSnDepositOutputsHeld@): given the Integer-encoded TxIns of the active UTxO set, all outputs
-- created by the requested transactions, and the deposit, decides whether the deposit may be
-- included. Both intersections must be empty, even for outputs the request subsequently spends.
reqSnDepositDisjointRef :: [Integer] -> [Integer] -> [Integer] -> Bool
reqSnDepositDisjointRef = M.hsReqSnDepositDisjointRef

-- | Extracted ackSn-collect guard (§6 @require (j,·) ∉ Σ̂@): given the signer indices already in Σ̂ and a
-- sender @j@, decides whether @j@ is a fresh signer.
notAlreadySignedRef :: [Integer] -> Integer -> Bool
notAlreadySignedRef = M.hsNotAlreadySignedRef

-- | Extracted ackSn-confirm guard (§6 @if ∀ k ∈ [1..n] : (k,·) ∈ Σ̂@): given the party count @n@ and the
-- signer indices in Σ̂, decides whether every party (index @< n@) has signed (n-of-n).
allSignedRef :: Integer -> [Integer] -> Bool
allSignedRef = M.hsAllSignedRef

-- | Extracted contest re-post guard (§6 @if S̄.s > s_c@): our confirmed snapshot number vs the
-- closed/contested one.
contestEligibleRef :: Integer -> Integer -> Bool
contestEligibleRef = M.hsContestEligibleRef

-- | Extracted round-robin leader (the §6 @leader(s)@). Arguments: @m@ where the head has @suc m@ parties,
-- the snapshot number, and a 0-based party index. Bound against the real 'Hydra.HeadLogic.isLeader' in
-- the hydra-node off-chain differential.
leaderRef :: Integer -> Integer -> Integer -> Bool
leaderRef = M.hsLeaderRef

-- | Outcome of one reqTx attempt with no deposits, decommit, or previously pending
-- transactions, fresh transaction output names, and a local UTxO equal to the
-- settled confirmed UTxO. Inputs are
-- ledger applicability, TTL, snapshot-in-flight, party count minus one, next
-- snapshot number, and the local party's zero-based index. The model decides
-- leader eligibility itself and retains ordered committed change/effect tags.
-- Payloads, collision checks, and retry scheduling are outside this projection.
-- All Integer arguments represent naturals; the party index must be in range.
reqTxOutcomeRef :: Bool -> Integer -> Bool -> Integer -> Integer -> Integer -> HsReqTxOutcome
reqTxOutcomeRef = M.hsReqTxOutcomeRef

-- | Named bounded snapshot outcomes, documented in OffChainReference.agda.
-- No deposits/decommit, output collisions, oversized accumulators or payloads.
reqSnOutcomeRef :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool -> HsSnapshotOutcome
reqSnOutcomeRef = M.hsReqSnOutcomeRef

-- | Phase, seen/requested numbers, signer indices, sender, party count minus one,
-- verification reply, useful next work, and local party index. Naturals only.
ackSnOutcomeRef :: HsSnapshotPhase -> Integer -> Integer -> [Integer] -> Integer -> Integer -> Bool -> Bool -> Integer -> HsSnapshotOutcome
ackSnOutcomeRef = M.hsAckSnOutcomeRef

-- | Extend the bounded receipt outcome with the already-applied-ID guard.
reqTxKnownOutcomeRef :: Bool -> Bool -> Integer -> Bool -> Integer -> Integer -> Integer -> HsReqTxOutcome
reqTxKnownOutcomeRef = M.hsReqTxKnownOutcomeRef

-- | Phase projections of retained events; guards and payloads are outside these abstractions.
lifecyclePhaseRef :: HsLifecyclePhase -> HsLifecycleEvent -> Bool -> HsLifecyclePhase
lifecyclePhaseRef = M.hsLifecyclePhaseRef

coordinationPhaseRef :: HsCoordinationPhase -> HsCoordinationEvent -> Bool -> Bool -> Bool -> HsCoordinationPhase
coordinationPhaseRef = M.hsCoordinationPhaseRef

trackedDepositPhaseRef :: HsTrackedPhase -> Integer -> HsDepositStatus -> Bool -> Bool -> HsTrackedPhase
trackedDepositPhaseRef = M.hsTrackedDepositPhaseRef

pendingIncrementPhaseRef :: Bool -> Bool -> Bool -> Integer
pendingIncrementPhaseRef = M.hsPendingIncrementPhaseRef

-- | Whole ReqDec outcome tags within the bounded domain of the Agda reference.
reqDecOutcomeRef :: Bool -> Bool -> Bool -> Bool -> Integer -> Bool -> Integer -> Integer -> Integer -> HsReqDecOutcome
reqDecOutcomeRef = M.hsReqDecOutcomeRef

-- | Registration and finalized-increment guards, in their execution order.
recoverOutcomeRef :: Bool -> Bool -> HsRecoverOutcome
recoverOutcomeRef = M.hsRecoverOutcomeRef

-- | Bounded input admission. Kind: client=0, network=1, chain=2. Result:
-- dispatch=0, unsafe=1, unsynced client=2, wait for sync=3.
inputAdmissionRef :: Integer -> Bool -> Bool -> Integer
inputAdmissionRef = M.hsInputAdmissionRef

-- | Existing decommit, applicability, nonempty outputs; failure priority or send.
clientDecommitRef :: Bool -> Bool -> Bool -> Integer
clientDecommitRef = M.hsClientDecommitRef

-- | Observed/seen number, snapshot/new version, action matches, signatures valid.
adoptSettlementRef :: Integer -> Integer -> Integer -> Integer -> Bool -> Bool -> Bool
adoptSettlementRef = M.hsAdoptSettlementRef

-- | Pending work, rollback slot, finalized observation slot. Naturals only.
repostFinalizedRef :: Bool -> Integer -> Integer -> Bool
repostFinalizedRef = M.hsRepostFinalizedRef

-- | Empty remainder and mode (auto=0, selection=1, awaiting=2); wait=0,
-- post remainder=1, post selection=2.
fanoutContinuationRef :: Bool -> Integer -> Integer
fanoutContinuationRef = M.hsFanoutContinuationRef

-- | Pending decommit inputs must be absent from the available local UTxO.
-- Bounded SimpleTx output identities, represented by nonnegative integers.
reservationHeldRef :: [Integer] -> [Integer] -> Bool
reservationHeldRef = M.hsReservationHeldRef

-- | A new decommit needs its inputs in the confirmed base, before ordinary
-- snapshot transactions run. A decommit already carried needs no reapplication.
decommitReadyRef :: Bool -> [Integer] -> [Integer] -> Bool
decommitReadyRef = M.hsDecommitReadyRef

-- | Retained version and decommit reservation after a decrement observation.
settlementVersionRef :: Integer -> Integer -> Integer
settlementVersionRef = M.hsSettlementVersionRef

settlementReservationRef :: Bool -> Integer -> Integer -> Bool
settlementReservationRef = M.hsSettlementReservationRef

-- | Rebuilt local outputs with reserved spending inputs removed.
reserveRebaseRef :: [Integer] -> [Integer] -> [Integer]
reserveRebaseRef = M.hsReserveRebaseRef

-- | Whether a pending decommit warrants a round without ordinary transactions.
newDecommitReadyRef :: Bool -> [Integer] -> [Integer] -> Bool
newDecommitReadyRef = M.hsNewDecommitReadyRef

-- | Deployed accumulator bound and independent count predicates from #2927.
accumulatorCapacityRef :: Integer
accumulatorCapacityRef = M.hsAccumulatorCapacityRef

snapshotCapacityRef :: Integer -> Integer -> Bool
snapshotCapacityRef = M.hsSnapshotCapacityRef

decommitCapacityRef :: Integer -> Integer -> Integer -> Bool
decommitCapacityRef = M.hsDecommitCapacityRef

pendingTxFitsRef :: Bool -> Bool -> Integer -> Integer -> Bool
pendingTxFitsRef = M.hsPendingTxFitsRef

-- | Set-based checks with a small bound. Callers bridge to production by adding
-- untouched background outputs; transactions must not name that background.
capacityFitsRef :: Integer -> [Integer] -> [Integer] -> [Integer] -> Bool
capacityFitsRef = M.hsCapacityFitsRef

capacityPrefixRef :: Integer -> [Integer] -> [Integer] -> [[Integer]] -> [[Integer]] -> Integer
capacityPrefixRef = M.hsCapacityPrefixRef

capacityPruneRef :: Integer -> [Integer] -> [Integer] -> [Integer] -> [[Integer]] -> [[Integer]] -> [Integer]
capacityPruneRef = M.hsCapacityPruneRef

-- | Retain local intent if carried, or if its dependencies survive confirmation.
retainReservationRef :: Bool -> Bool -> [Integer] -> [Integer] -> Bool
retainReservationRef = M.hsRetainReservationRef

-- | Local reservations and unsettled certificates both block a new decommit.
decommitAdmissionRef :: Bool -> Bool -> Bool
decommitAdmissionRef = M.hsDecommitAdmissionRef

-- | Head identity and ordered committee must match the verification context.
verificationContextRef :: Integer -> Integer -> [Integer] -> [Integer] -> Bool
verificationContextRef = M.hsVerificationContextRef

-- | Zero selects the reservation, positive indices select registry entries.
-- Output identities are finite sets; empty outputs identify no transaction.
carriedDecommitPayloadRef :: Bool -> [Integer] -> [Integer] -> [[Integer]] -> Maybe Integer
carriedDecommitPayloadRef = M.hsCarriedDecommitPayloadRef

-- | A same-version certificate requires a matching carried transaction.
carriedDecommitRequestRef :: Bool -> Bool -> Bool -> Bool
carriedDecommitRequestRef = M.hsCarriedDecommitRequestRef
