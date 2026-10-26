{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | The off-chain snapshot protocol: ingesting transactions, requesting,
-- signing and confirming snapshots, and side-loading a confirmed one.
--
-- The functions here decide the messages to send and the 'StateChanged'
-- events to record, as 'Hydra.HeadLogic.update' dispatches them. The folds that
-- apply those events live in "Hydra.HeadLogic.Aggregate".
module Hydra.HeadLogic.Snapshot where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  PostChainTx (..),
 )
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  changes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.Settlement (retainedDeposits)
import Hydra.HeadLogic.Snapshot.Request (
  SettlingCommit (..),
  SnapshotRequest (..),
  absorbable,
  applyTransactionsWithoutCollision,
  buildSnapshotRequest,
  chainedSnapshotRequest,
  isLeader,
  namesNoneOf,
  settledUTxO,
  settlingCommitAt,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (..),
  OpenState (..),
  SeenSnapshot (..),
  Settlement (..),
  seenSnapshotNumber,
  settlementEntries,
  snapshotInFlight,
 )
import Hydra.Ledger (
  Ledger (..),
  ValidationError (..),
 )
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (
  Environment (..),
 )
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  DepositStatus (..),
  PendingDeposits,
 )
import Hydra.Tx (
  IsTx (..),
  combinedUTxO,
 )
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (
  Signature,
  Verified (..),
  aggregateInOrder,
  sign,
  verifyBytes,
  verifyMultiSignature,
  verifyMultiSignatureBytes,
 )
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Settlement (SettlementEvidence (..), snapshotEvidence)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
 )

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenClientNewTx tx =
  cause . NetworkEffect $ ReqTx tx

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  forall tx.
  IsTx tx =>
  Environment ->
  Ledger tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx =
  -- Keep track of transactions by-id
  (newState TransactionReceived{tx} <>) $
    -- Spec: wait L̂ ◦ tx ≠ ⊥
    waitApplyTx $
      -- Spec: T̂ ← T̂ ⋃ {tx}
      --       L̂  ← L̂ ◦ tx
      newState TransactionAppliedToLocalUTxO{headId, tx}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        & maybeRequestSnapshot (confirmedSn + 1)
 where
  waitApplyTx cont =
    case applyTransactionsWithoutCollision ledger currentSlot localUTxO [tx] of
      Right _
        -- Refuse a tx creating an output that a claimable or settling deposit
        -- names: it would collide with that deposit once absorbed, and no node
        -- signs a snapshot carrying both. Expired deposits are never claimed, so
        -- they do not count.
        | namesNoneOf (utxoFromTx tx) namedByDepositsInPlay -> cont
        | otherwise -> notApplicable (ValidationError "transaction creates an output a pending deposit names")
      Left (_, err) -> notApplicable err
   where
    namedByDepositsInPlay :: UTxOType tx
    namedByDepositsInPlay =
      foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
        <> case settlingCommit of
          Just (ContinueCommit (_, Deposit{deposited})) -> deposited
          _ -> mempty

    notApplicable :: ValidationError -> Outcome tx
    notApplicable err
      | ttl > 0 =
          wait (WaitOnNotApplicableTx err)
      | otherwise =
          -- XXX: We are removing invalid txs from allTxs here to
          -- prevent them piling up infinitely. However, this is not really
          -- covered by the spec and this could be problematic in case of
          -- conflicting transactions paired with network latency and/or
          -- message resubmission. For example: Assume tx2 depends on tx1, but
          -- only tx2 is seen by a participant and eventually times out
          -- because of network latency when receiving tx1. The leader,
          -- however, saw both as valid and requests a snapshot including
          -- both. This is a valid request and it could make the head stuck.
          newState TxInvalid{headId, utxo = localUTxO, transaction = tx, validationError = err}

  maybeRequestSnapshot nextSn outcome =
    -- The request includes this tx. It is checked against the same UTxO the
    -- followers use to check it, so they do not reject it.
    case chainedSnapshotRequest confirmedUTxO localTxs' pendingDeposits currentDepositTxId decommitTx settlingCommit of
      Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}
        | not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn ->
            outcome
              -- XXX: This state update has no equivalence in the
              -- spec. Do we really need to store that we have
              -- requested a snapshot? If yes, should update spec.
              <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
              <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)
      _ -> outcome

  Environment{party} = env

  CoordinatedHeadState
    { localTxs
    , localUTxO
    , confirmedSnapshot
    , seenSnapshot
    , decommitTx
    , version
    , currentDepositTxId
    } = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)

  settlingCommit = settlingCommitAt env version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot)

  OpenState{coordinatedHeadState, headId, parameters} = st

  -- NOTE: Order of transactions is important here. See also
  -- 'pruneTransactions'.
  localTxs' = localTxs Seq.|> tx

-- | Process a snapshot request ('ReqSn') from party.
--
-- This checks that s is the next snapshot number and that the party is
-- responsible for leading that snapshot. Then, we potentially wait until the
-- previous snapshot is confirmed (no snapshot is in flight), before we apply
-- (or wait until applicable) the requested transactions to the last confirmed
-- snapshot. Only then, we start tracking this new "seen" snapshot, compute a
-- signature of it and send the corresponding 'AckSn' to all parties. Finally,
-- the pending transaction set gets pruned to only contain still applicable
-- transactions.
--
-- A snapshot round needs every party's signature. Once the leader is collecting
-- signatures, it does not replace that request. Refusing a request that other
-- parties sign can therefore stop the round permanently.
--
-- Base permanent refusals on the request and the agreed snapshot. Local chain
-- observations can differ: one party may have seen a deposit expire, settle,
-- recover, or return after rollback while another has not. Carrying the same
-- signed claim must not depend on these differences.
--
-- When a different deposit could be valid after recovery of the previous claim,
-- wait for that recovery while the request still has retries. Likewise, a
-- locally expired deposit may still be active for the honest leader that chose
-- it. The leader selects new deposits only while active, but receivers do not
-- reject that selection solely because their local expiry tick came first.
--
-- Requests two or more versions behind remain unsupported: the close redeemer
-- can express only the current or preceding version. That limitation can still
-- cause parties with sufficiently different chain views to disagree.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  forall tx.
  IsTx tx =>
  Environment ->
  Ledger tx ->
  PendingDeposits tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot version.
  SnapshotVersion ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  -- | Optional commit of additional funds into the head.
  Maybe (TxIdType tx) ->
  Outcome tx
onOpenNetworkReqSn env ledger pendingDeposits ChainPointTime{currentSlot, currentChainTime} st ttl otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
  -- Spec: require v = v̂ ∧ s = ŝ + 1 ∧ leader(s) = j
  requireReqSn $
    -- Spec: wait ŝ = ̅S.s
    waitNoSnapshotInFlight $
      -- Spec: wait v = v̂
      -- NOTE: must be a Wait, not a require: a follower can receive ReqSn for
      -- the bumped version before its own chain handler has processed the
      -- triggering OnIncrementTx/OnDecrementTx. Erroring here would drop the
      -- message permanently (Error outcomes are not re-enqueued), leaving the
      -- head stuck until the deposit expires. A proposal *behind* our version
      -- is the opposite case, see 'waitOnSnapshotVersion'.
      waitOnSnapshotVersion $
        -- Require any pending utxo to decommit to be consistent
        requireApplicableDecommitTx $ \(activeUTxO, mUtxoToDecommit) ->
          -- Wait for the deposit and require any pending commit to be consistent
          waitForDeposit $ \mUtxoToCommit ->
            -- Resolve transactions by-id
            waitResolvableTxs $ \requestedTxs -> do
              -- Spec: require 𝑈_𝛼 ∩ 𝑈_active = ∅ ∧ 𝑈_𝛼 ∩ outputs(Treq) = ∅
              --       require 𝑈_active ◦ Treq ≠ ⊥
              --       𝑈 ← 𝑈_active ◦ Treq
              requireApplyTxs activeUTxO mUtxoToCommit requestedTxs $ \nextUTxO ->
                -- The predecessor is confirmed at this point (see
                -- requireReqSn and waitNoSnapshotInFlight), so its two
                -- accumulators cover exactly its own two owed sets (see
                -- 'Accumulator.buildFromSnapshotUTxOs') and can be updated by
                -- the UTxO delta instead of re-serializing and re-hashing
                -- every output.
                let prevSnapshot = getSnapshot confirmedSnapshot
                    accumulator =
                      Accumulator.applyUTxODelta
                        prevSnapshot.accumulator
                        (combinedUTxO prevSnapshot.utxo Nothing prevSnapshot.utxoToDecommit)
                        (combinedUTxO nextUTxO Nothing mUtxoToDecommit)
                    appliedAccumulator
                      -- Nothing pending: both accumulators are the same value,
                      -- share the thunk so the commitment is computed once.
                      | isNothing mUtxoToCommit && isNothing mUtxoToDecommit = accumulator
                      | otherwise =
                          Accumulator.applyUTxODelta
                            prevSnapshot.appliedAccumulator
                            (combinedUTxO prevSnapshot.utxo prevSnapshot.utxoToCommit Nothing)
                            (combinedUTxO nextUTxO mUtxoToCommit Nothing)
                 in requireValidAccumulatorSize accumulator $ requireValidAccumulatorSize appliedAccumulator $ do
                      -- Spec: ŝ ← ̅S.s + 1
                      -- NOTE: confSn == seenSn == sn here
                      let nextSnapshot =
                            Snapshot
                              { headId
                              , -- The version proposed, not the local one: for a
                                -- proposal one version behind they differ, and the
                                -- signed bytes must equal those of the parties that
                                -- have not seen the bump ('waitOnSnapshotVersion').
                                version = sv
                              , number = sn
                              , confirmed = requestedTxs
                              , utxo = nextUTxO
                              , utxoToCommit = mUtxoToCommit
                              , utxoToDecommit = mUtxoToDecommit
                              , -- Bound into the signature so the increment can only
                                -- claim this very deposit, see 'Hydra.Tx.Snapshot'.
                                depositTxId = mDepositTxId
                              , accumulator
                              , appliedAccumulator
                              }

                      -- Spec: 𝜂 ← combine(𝑈)
                      --       σᵢ ← MS-Sign(kₕˢⁱᵍ, (cid‖v‖ŝ‖η))
                      let snapshotSignature = sign signingKey nextSnapshot
                      -- Spec: multicast (ackSn, ŝ, σᵢ)
                      (cause (NetworkEffect $ AckSn snapshotSignature sn) <>) $ do
                        -- Spec: ̂Σ ← ∅
                        --       L̂ ← 𝑈
                        --       𝑋 ← T
                        --       T̂ ← ∅
                        --       for tx ∈ 𝑋 : L̂ ◦ tx ≠ ⊥
                        --         T̂ ← T̂ ⋃ {tx}
                        --         L̂ ← L̂ ◦ tx
                        --
                        -- NOTE: 'SnapshotRequested' rebuilds 'localUTxO' from
                        -- 'nextUTxO', so every kept tx applies there.
                        let newLocalTxs = pruneTransactions nextUTxO
                        newState
                          SnapshotRequested
                            { requestedSnapshot = nextSnapshot
                            , newLocalTxs
                            , newCurrentDepositTxId = mDepositTxId
                            }
 where
  requireReqSn continue
    | sn /= seenSn + 1 =
        Error $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    | not (isLeader parameters otherParty sn) =
        Error $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
    | otherwise =
        continue

  waitNoSnapshotInFlight continue
    | confSn == seenSn =
        continue
    | otherwise =
        wait $ WaitOnSnapshotNumber seenSn

  -- Spec: wait v = v̂. Our version only ever goes up, so there are four cases:
  --
  --   * @sv == version@: the normal case.
  --
  --   * @sv + 1 == version && sv == confVersion@: the proposal is one version
  --     behind us and based on our confirmed snapshot. The leader made it
  --     before it saw the settlement land, and the parties that have not seen
  --     it land either sign it at @sv@. We do the same (see @version = sv@ and
  --     'confirmedUTxO'), so everyone signs the same bytes, and the round
  --     confirms one version behind the chain, which 'CloseUsed' handles.
  --     Waiting would never end, since the leader does not propose again while
  --     it collects signatures. We only sign if the proposal carries the action
  --     that settled, see 'reCarriesSettledAction'.
  --
  --   * @sv > version@: the leader saw a settlement land that our chain handler
  --     has not processed yet. A retry fixes this, so we wait.
  --
  --   * Anything else can never be signed: two or more versions behind, or
  --     below the confirmed snapshot's version. Waiting would only use up the
  --     retries and then drop the message in silence, so we fail right away.
  waitOnSnapshotVersion continue
    | sv == version = continue
    | sv + 1 == version
    , sv == confVersion =
        if reCarriesSettledAction
          then continue
          else Error $ RequireFailed ReqSvBehindMustReCarry{requestedSv = sv, requestedDepositTxId = mDepositTxId, requestedDecommitTxId = txId <$> mDecommitTx}
    | sv > version = wait $ WaitOnSnapshotVersion sv
    | otherwise = Error $ RequireFailed ReqSvNumberInvalid{requestedSv = sv, lastSeenSv = version}

  -- A proposal one version behind us must carry exactly what the confirmed
  -- snapshot settled: the deposit it claims, or the decommit it pays out, and
  -- nothing else. We saw that action land, while the parties that have not
  -- seen it sign whatever the leader proposes. A proposal that drops the
  -- action would confirm a snapshot without something the chain has already
  -- applied, and no close redeemer can express that. A proposal that swaps in
  -- a fresh deposit would make us count that deposit as absorbed once we apply
  -- the bump, although no increment ever claimed it. An honest leader always
  -- carries the action again ('selectNextIncrementalAction').
  reCarriesSettledAction =
    mDepositTxId == confDepositTxId
      && (utxoFromTx <$> mDecommitTx) == confUTxOToDecommit

  waitResolvableTxs continue =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> continue $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
      unseen -> wait $ WaitOnTxs unseen

  -- Resolve the requested deposit into the snapshot's 'utxoToCommit'. It is
  -- not added to the active UTxO: a deposit is not spendable on L2 until its
  -- increment lands, otherwise a requested tx could spend it and the snapshot
  -- would count its value twice.
  waitForDeposit cont =
    case mDepositTxId of
      -- A request without a deposit drops the commit the confirmed snapshot is
      -- settling. Allow that only once its increment can no longer land
      -- ('DropCommit'), otherwise the head ends up over-backed. Wait instead of
      -- failing: this node may be behind the leader's chain view ('Hydra.Node'
      -- keeps this wait beyond the ttl).
      Nothing ->
        case settlingCommitAt env version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot) of
          Just (ContinueCommit (_, Deposit{deposited})) -> wait WaitOnUnresolvedCommit{commitUTxO = deposited}
          _ -> cont Nothing
      Just depositTxId
        -- A proposal one version behind us carries the confirmed snapshot's own
        -- commit again ('waitOnSnapshotVersion'). We already saw its increment,
        -- so here the deposit is consumed and retained, while the parties that
        -- have not seen it still hold the deposit pending and sign a snapshot
        -- carrying it. Sign the same bytes, taking the deposited outputs from
        -- the confirmed snapshot.
        --
        -- This holds whether or not a rollback has since erased the increment.
        -- Refusing then would reject the proposal an honest leader that is a
        -- little behind makes, since it carries its own unsettled claim again
        -- ('selectNextDeposit'). The refusal is terminal, so the leader would
        -- never get our signature and, already collecting them, would never
        -- propose that number again: the head would stop confirming, which is
        -- what signing one version behind exists to prevent. Only a claim on
        -- some other retained deposit is a second claim, and the next guard
        -- refuses that.
        | sv == confVersion
        , confDepositTxId == Just depositTxId
        , isJust confUTxOToCommit
        , confirmedCommitSettled ->
            cont confUTxOToCommit
      Just depositTxId
        -- The deposit is already claimed by a signed snapshot whose increment
        -- landed. It is only pending again because a rollback erased that
        -- increment, and an honest leader never proposes it ('eligibleDeposits').
        -- Refuse rather than sign a second snapshot claiming it, see #2741.
        | depositTxId `Set.member` retainedDeposits settlements ->
            Error $ RequireFailed ReqSnDepositBlockedByFinalizedCommit{depositTxId}
      Just depositTxId ->
        case Map.lookup depositTxId pendingDeposits of
          Nothing
            -- NOTE: must be a Wait while ttl remains, not a require: a
            -- follower can receive the ReqSn before its own chain handler has
            -- processed the deposit observation. Erroring would drop the
            -- message permanently and this node would never sign — with the
            -- snapshot then in flight on all other nodes, the head is stuck
            -- for good. Once ttl is exhausted the deposit is genuinely
            -- unknown (e.g. a stale ReqSn referencing an already recovered
            -- deposit) and we error out.
            | ttl > 0 -> wait WaitOnDepositObserved{depositTxId}
            | otherwise -> Error $ RequireFailed RequestedDepositNotFoundLocally{depositTxId}
          Just Deposit{status, deposited}
            -- A signed claim remains the same claim after local status changes.
            | sv == confVersion
            , confDepositTxId == Just depositTxId
            , confUTxOToCommit == Just deposited ->
                cont confUTxOToCommit
            | status == Inactive -> wait WaitOnDepositActivation{depositTxId}
            -- NOTE: this makes the commits sequential in a sense that you can't
            -- commit unless the previous commit is settled.
            --
            -- Only while the claimed deposit is still tracked: once it was
            -- recovered on L1 the claim is dropped (see 'settlingCommitAt') and
            -- the leader may propose a fresh one. That recover is a chain
            -- observation, so a leader can have seen it while we have not, and
            -- refusing outright would be a refusal the other parties do not
            -- make. Wait instead while the ttl lasts: a retry after our own
            -- chain follower catches up takes the arm below. See the note on
            -- unanimous refusals above.
            | sv == confVersion
            , isJust confUTxOToCommit
            , maybe False (`Map.member` pendingDeposits) confDepositTxId ->
                if ttl > 0
                  then wait WaitOnUnsettledCommit{depositTxId}
                  else Error $ RequireFailed ReqSnCommitNotSettled
            | otherwise -> cont (Just deposited)

  requireApplicableDecommitTx cont =
    case mDecommitTx of
      -- A request without a decommit drops the one the confirmed snapshot is
      -- settling. Unlike a settling commit (see 'waitForDeposit'), a decommit
      -- is never dropped: its decrement is re-posted with every snapshot
      -- carrying it, and a leader stops carrying it only once it observed
      -- the decrement, at the bumped version. Signing such a request would
      -- leave the decommitted outputs in no confirmed snapshot once the
      -- decrement lands (a deposit claimed in their place would also race it
      -- for the version bump), so the head could not be closed and fanned
      -- out from any snapshot the honest nodes hold.
      Nothing
        | sv == confVersion && isJust confUTxOToDecommit ->
            Error $ RequireFailed ReqSnDecommitNotSettled
        | otherwise -> cont (confirmedUTxO, Nothing)
      -- Spec: require tx𝜔 = ⊥ ∨ tx𝛼 = ⊥
      --
      -- A snapshot settling both a commit and a decommit cannot be closed:
      -- close and fanout express a single incremental action
      -- ('setIncrementalActionMaybe'). The leader never proposes both (see
      -- 'nextSnapshotRequest'), so this rejects a request that does
      -- anyway rather than confirming an unclosable snapshot.
      Just decommitTx
        | Just depositTxId <- mDepositTxId ->
            Error $ RequireFailed ReqSnBothCommitAndDecommit{depositTxId, decommitTxId = txId decommitTx}
      -- 'Hydra.Contract.Head.checkDecrement' requires at least one decommit
      -- output, so a decommit materializing none could never settle on-chain and
      -- would be re-proposed by every later snapshot.
      Just decommitTx
        | utxoFromTx decommitTx == mempty ->
            Error $ RequireFailed ReqSnDecommitNoOutputs{decommitTxId = txId decommitTx}
      Just decommitTx ->
        -- Spec:
        -- require 𝑣 = 𝑣 ̂ ∧ 𝑠 = 𝑠 ̂ + 1 ∧ leader(𝑠) = 𝑗
        -- wait 𝑠 ̂ = 𝒮.𝑠
        if sv == confVersion && isJust confUTxOToDecommit
          then
            if confUTxOToDecommit == Just (utxoFromTx decommitTx)
              then cont (confirmedUTxO, confUTxOToDecommit)
              else Error $ RequireFailed ReqSnDecommitNotSettled
          else case applyTransactions ledger currentSlot confirmedUTxO [decommitTx] of
            Left (_, err) ->
              Error $ RequireFailed $ SnapshotDoesNotApply sn (txId decommitTx) err
            Right newConfirmedUTxO -> do
              let utxoToDecommit = utxoFromTx decommitTx
              let activeUTxO = newConfirmedUTxO `withoutUTxO` utxoToDecommit
              cont (activeUTxO, Just utxoToDecommit)

  -- The snapshot can contain fewer transactions than the ones we have seen at
  -- this stage, but they all _must_ apply correctly to the latest snapshot's
  -- UTxO set, eg. it's illegal for a snapshot leader to request a snapshot
  -- containing transactions that do not apply cleanly.
  --
  -- We fully apply here, re-running signature and Plutus checks. Transactions
  -- resolved from 'allTxs' are not guaranteed to have been validated locally
  -- (an invalid one can be recorded on the receipt 'Wait' branch, and a
  -- follower may not have applied a valid one that conflicts with its own
  -- optimistic local state), so full application is the only place that
  -- guarantees a confirmed snapshot never contains an unvalidated transaction.
  --
  -- A claimed deposit must also be 'absorbable' next to the requested txs.
  requireApplyTxs :: UTxOType tx -> Maybe (UTxOType tx) -> [tx] -> (UTxOType tx -> Outcome tx) -> Outcome tx
  requireApplyTxs utxo mUtxoToCommit' requestedTxs cont
    | Just depositTxId <- mDepositTxId
    , Just deposited <- mUtxoToCommit'
    , not (absorbable utxo requestedTxs deposited) =
        Error $ RequireFailed ReqSnDepositOutputsHeld{depositTxId}
    | otherwise =
        -- Spec: require outputs(Treq) ∩ 𝑈_active = ∅
        case applyTransactionsWithoutCollision ledger currentSlot utxo requestedTxs of
          Left (tx, err) ->
            Error $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
          Right u -> cont u

  requireValidAccumulatorSize :: Accumulator.HydraAccumulator -> Outcome tx -> Outcome tx
  requireValidAccumulatorSize accumulator continue =
    case Accumulator.checkAccumulatorSize accumulator of
      Left AccumulatorTooLarge{utxoCount, maxAllowed} ->
        Error $ RequireFailed ReqSnUTxOSetTooLarge{utxoCount, maxAllowed}
      Right () -> continue

  -- \| Filter 'localTxs' to those that still apply against the running UTxO
  -- after each previous successful tx. The post-snapshot UTxO is not returned:
  -- aggregate will recompute it.
  pruneTransactions utxo0 = go utxo0 localTxs
   where
    go _ Seq.Empty = Seq.empty
    go u (tx Seq.:<| rest) =
      -- XXX: We prune transactions on any error, while only some of them are
      -- actually expected.
      -- For example: `OutsideValidityIntervalUTxO` ledger errors are expected
      -- here when a tx becomes invalid.
      case applyTransactionsWithoutCollision ledger currentSlot u [tx] of
        Left _ -> go u rest
        Right u' -> tx Seq.<| go u' rest
  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  Snapshot{version = confVersion} = getSnapshot confirmedSnapshot

  confUTxOToCommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToCommit}} -> utxoToCommit

  confDepositTxId = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{depositTxId}} -> depositTxId

  confUTxOToDecommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToDecommit}} -> utxoToDecommit

  -- Whether we observed the settlement of the confirmed snapshot's own commit,
  -- which is what makes that commit ours to re-sign rather than a fresh claim.
  -- The status does not matter: a rollback may have erased the settlement
  -- since, and the ticks post it again ('repostErased'), so the commit is
  -- still the one being settled.
  confirmedCommitSettled =
    case Map.lookup confVersion (settlementEntries settlements) of
      Just Settlement{snapshot} -> snapshot.depositTxId == confDepositTxId
      Nothing -> False

  seenSn = seenSnapshotNumber seenSnapshot

  -- The base UTxO of the requested snapshot. The confirmed snapshot's pending
  -- commit is spendable exactly when its increment had landed as of the
  -- version being signed, hence @sv@ and not the local version: in a straddle
  -- the local version is one ahead, and keying on it would make this node's
  -- base UTxO, and so its signed bytes, differ from everyone else's.
  confirmedUTxO = settledUTxO sv (getSnapshot confirmedSnapshot)

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version, settlements} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, headId} = st

  Environment{signingKey} = env

-- | Process a snapshot acknowledgement ('AckSn') from a party.
--
-- We do require that the is from the last seen or next expected snapshot, and
-- potentially wait wait for the corresponding 'ReqSn' before proceeding. If the
-- party hasn't sent us a signature yet, we store it. Once a signature from each
-- party has been collected, we aggregate a multi-signature and verify it is
-- correct. If everything is fine, the snapshot can be considered as the latest
-- confirmed one. Similar to processing a 'ReqTx', we check whether we are
-- leading the next snapshot and craft a corresponding 'ReqSn' if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkAckSn ::
  IsTx tx =>
  Environment ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn env@Environment{party} ChainPointTime{currentChainTime} pendingDeposits openState otherParty snapshotSignature sn =
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs snapshotBytes -> do
      -- Spec: require (j,⋅) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        requireSignatureForThisSnapshot snapshotBytes $ do
          -- Spec: ̂Σ[j] ← σⱼ
          (newState PartySignedSnapshot{snapshotNumber = snapshot.number, party = otherParty, signature = snapshotSignature} <>) $
            --       if ∀k ∈ [1..n] : (k,·) ∈ ̂Σ
            ifAllMembersHaveSigned sigs $ \sigs' -> do
              -- Spec: σ̃ ← MS-ASig(kₕˢᵉᵗᵘᵖ,̂Σ)
              let multisig = aggregateInOrder sigs' parties
              -- Spec: η ← combine(𝑈ˆ)
              --       require MS-Verify(k ̃H, (cid‖v̂‖ŝ‖η), σ̃)
              requireVerifiedMultisignature multisig snapshotBytes $
                do
                  -- NOTE: Fix all the spec comments once specification is in place
                  -- Spec: ̅S ← snObj(v̂, ŝ, Û, T̂, 𝑈𝛼, 𝑈𝜔)
                  --       ̅S.σ ← ̃σ
                  newState SnapshotConfirmed{headId, snapshot = Nothing, signatures = multisig}
                  -- Spec: if 𝑈𝛼 ≠ ⊥
                  --         postTx (increment, v̂, ŝ, η)
                  & maybePostIncrementTx snapshot multisig
                  -- Spec: if txω ≠ ⊥
                  --         postTx (decrement, v̂, ŝ, η)
                  & maybePostDecrementTx snapshot multisig
                  -- Spec: if leader(s + 1) = i ∧ T̂ ≠ ∅
                  -- REVIEW: multicast (reqSn, v, ̅S.s + 1, T̂, S.𝑈𝛼, S.txω)
                  & maybeRequestNextSnapshot snapshot
 where
  seenSn = seenSnapshotNumber seenSnapshot

  requireValidAckSn continue =
    if sn `elem` [seenSn, seenSn + 1]
      then continue
      else Error $ RequireFailed $ AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}

  waitOnSeenSnapshot continue =
    case seenSnapshot of
      -- NOTE: Ignore any redundant AckSn for snapshots we have already seen as
      -- confirmed. This is for example happening if a party runs multiple
      -- instances of hydra-node using the same keys.
      LastSeenSnapshot{lastSeen}
        | sn <= lastSeen -> noop
      SeenSnapshot{snapshot, signatories = sigs, signableBytes}
        | seenSn == sn -> continue snapshot sigs signableBytes
      _ -> wait WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    if not (Map.member otherParty sigs)
      then continue
      else Error $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys sigs, receivedSignature = otherParty}

  -- Verify before recording the signature so a bad acknowledgement cannot
  -- prevent the same party's valid acknowledgement from being collected later.
  requireSignatureForThisSnapshot snapshotBytes continue =
    if verifyBytes (vkey otherParty) snapshotSignature snapshotBytes
      then continue
      else Error $ RequireFailed $ AckSnSignatureInvalid{requestedSn = sn, receivedSignature = otherParty}

  ifAllMembersHaveSigned sigs cont =
    let sigs' = Map.insert otherParty snapshotSignature sigs
     in if Map.keysSet sigs' == Set.fromList parties
          then cont sigs'
          else noop

  requireVerifiedMultisignature multisig msg cont =
    case verifyMultiSignatureBytes vkeys multisig msg of
      Verified -> cont
      FailedKeys failures ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys = failures}
      KeyNumberMismatch ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys}

  maybeRequestNextSnapshot previous outcome =
    -- Checked against the snapshot just confirmed: the followers check the
    -- request against that snapshot too.
    case request of
      SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}
        | isLeader parameters party nextSn && (worthChaining || carriesNewDecommit) ->
            outcome
              <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
              <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)
      _ -> outcome
   where
    nextSn = previous.number + 1
    -- Confirmation can retain a settlement in this same outcome. Exclude its
    -- deposit now, before the retained collection receives that snapshot.
    deposits =
      if version > previous.version
        then maybe pendingDeposits (`Map.delete` pendingDeposits) previous.depositTxId
        else pendingDeposits
    (request, worthChaining) =
      buildSnapshotRequest (settledUTxO version previous) localTxs deposits currentDepositTxId decommitTx (settlingCommitAt env version currentChainTime deposits previous)
    carriesNewDecommit =
      isJust request.requestDecommitTx && (version > previous.version || isNothing previous.utxoToDecommit)

  maybePostIncrementTx snapshot@Snapshot{utxoToCommit, depositTxId = signedDepositTxId} signatures outcome =
    -- NOTE: use the snapshot's own deposit and not 'currentDepositTxId'. The
    -- latter can be set by a 'DepositActivated' during the ack flow of an
    -- unrelated snapshot, and only the deposit bound into the signed snapshot
    -- can be claimed by an increment on-chain.
    case (signedDepositTxId, utxoToCommit) of
      (Just depositTxId, Just _) ->
        case Map.lookup depositTxId pendingDeposits of
          Just Deposit{deposited} ->
            outcome
              <> newState CommitApproved{headId, utxoToCommit = deposited}
              <> cause
                OnChainEffect
                  { postChainTx =
                      IncrementTx
                        { headSeed
                        , headId
                        , headParameters = parameters
                        , incrementingSnapshot = snapshotEvidence snapshot signatures
                        , depositTxId
                        }
                  }
          Nothing -> outcome
      _ -> outcome

  maybePostDecrementTx snapshot@Snapshot{utxoToDecommit} signatures outcome =
    case (decommitTx, utxoToDecommit) of
      (Just tx, Just utxo) ->
        outcome
          <> newState
            DecommitApproved
              { headId
              , decommitTxId = txId tx
              , utxoToDecommit = utxo
              }
          <> cause
            OnChainEffect
              { postChainTx =
                  DecrementTx
                    { headSeed
                    , headId
                    , headParameters = parameters
                    , decrementingSnapshot = snapshotEvidence snapshot signatures
                    }
              }
      _ -> outcome

  vkeys = vkey <$> parties

  OpenState
    { parameters = parameters@HeadParameters{parties}
    , coordinatedHeadState
    , headId
    , headSeed
    } = openState

  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx, currentDepositTxId, version} = coordinatedHeadState

-- | Client request to side load confirmed snapshot.
--
-- Note this is not covered by the spec as it is not reachable from an organic use of the protocol.
--
-- It must not have any effects outside of a neutral modification of the state to:
-- * something it was before (in the case of the initial snapshot).
-- * something it would be using side communication (in the case of a confirmed snapshot).
--
-- Besides the above, it is expected to work very much like the confirmed snapshot.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientSideLoadSnapshot :: IsTx tx => OpenState tx -> ConfirmedSnapshot tx -> Outcome tx
onOpenClientSideLoadSnapshot openState requestedConfirmedSnapshot =
  case requestedConfirmedSnapshot of
    InitialSnapshot{} ->
      requireVerifiedSameSnapshot $
        newState LocalStateCleared{headId, snapshotNumber = requestedSn}
    ConfirmedSnapshot{snapshot, signatures} ->
      requireVerifiedSnapshotNumber $
        requireVerifiedL1Snapshot $
          requireVerifiedMultisignature snapshot signatures $
            changes
              [ SnapshotConfirmed{headId, snapshot = Just snapshot, signatures}
              , LocalStateCleared{headId, snapshotNumber = requestedSn}
              ]
 where
  OpenState
    { headId
    , parameters = HeadParameters{parties}
    , coordinatedHeadState
    } = openState

  CoordinatedHeadState
    { confirmedSnapshot = currentConfirmedSnapshot
    } = coordinatedHeadState

  vkeys = vkey <$> parties

  currentSnapshot@Snapshot
    { version = lastSeenSv
    , number = lastSeenSn
    , utxoToCommit = lastSeenSc
    , depositTxId = lastSeenDeposit
    , utxoToDecommit = lastSeenSd
    } = getSnapshot currentConfirmedSnapshot

  requestedSnapshot@Snapshot
    { version = requestedSv
    , number = requestedSn
    , utxoToCommit = requestedSc
    , depositTxId = requestedDeposit
    , utxoToDecommit = requestedSd
    } = getSnapshot requestedConfirmedSnapshot

  clientInput = SideLoadSnapshot requestedConfirmedSnapshot

  sideLoadFailed requirementFailure =
    cause . ClientEffect $
      ServerOutput.SideLoadSnapshotRejected{clientInput, requirementFailure}

  requireVerifiedSameSnapshot cont =
    if requestedSnapshot == currentSnapshot
      then cont
      else sideLoadFailed SideLoadInitialSnapshotMismatch

  requireVerifiedSnapshotNumber cont =
    if requestedSn >= lastSeenSn
      then cont
      else sideLoadFailed SideLoadSnNumberInvalid{requestedSn, lastSeenSn}

  requireVerifiedL1Snapshot cont
    | requestedSv /= lastSeenSv = sideLoadFailed SideLoadSvNumberInvalid{requestedSv, lastSeenSv}
    | requestedSc /= lastSeenSc = sideLoadFailed SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc}
    -- The pending commit is L1-relevant state, and since the binding change it is
    -- the deposit that identifies it, not the committed content.
    | requestedDeposit /= lastSeenDeposit = sideLoadFailed SideLoadDepositTxIdInvalid{requestedDeposit, lastSeenDeposit}
    | requestedSd /= lastSeenSd = sideLoadFailed SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}
    | otherwise = cont

  requireVerifiedMultisignature snapshot signatories cont =
    case verifyMultiSignature vkeys signatories snapshot of
      Verified -> cont
      FailedKeys failures ->
        sideLoadFailed SideLoadInvalidMultisignature{multisig = show signatories, vkeys = failures}
      KeyNumberMismatch ->
        sideLoadFailed SideLoadInvalidMultisignature{multisig = show signatories, vkeys}
