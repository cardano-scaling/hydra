{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen free programs before crem drives the production rules. Pure snapshot
-- selection is shared; instruction order and branching are frozen here.
module Test.Hydra.HeadLogic.LegacyPrograms (onOpenNetworkReqTxProgram, onOpenNetworkReqSnProgram, onOpenNetworkAckSnProgram) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    finalizedCommit,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  Effect (NetworkEffect, OnChainEffect, postChainTx),
  LogicError (RequireFailed),
  OpenState (
    OpenState,
    coordinatedHeadState,
    headId,
    headSeed,
    parameters
  ),
  RequirementFailure (
    AckSnNumberInvalid,
    InvalidMultisignature,
    ReqSnBothCommitAndDecommit,
    ReqSnCommitNotSettled,
    ReqSnDecommitNoOutputs,
    ReqSnDecommitNotSettled,
    ReqSnDepositBlockedByFinalizedCommit,
    ReqSnDepositOutputsHeld,
    ReqSnNotLeader,
    ReqSnNumberInvalid,
    ReqSnUTxOSetTooLarge,
    RequestedDepositExpired,
    RequestedDepositNotFoundLocally,
    SnapshotAlreadySigned,
    SnapshotDoesNotApply,
    decommitTxId,
    depositTxId,
    knownSignatures,
    lastSeenSn,
    leader,
    maxAllowed,
    multisig,
    receivedSignature,
    requestedSn,
    utxoCount,
    vkeys
  ),
  RuleEnvironment,
  SeenSnapshot (
    LastSeenSnapshot,
    SeenSnapshot,
    lastSeen,
    signableBytes,
    signatories,
    snapshot
  ),
  SettlingCommit (ContinueCommit),
  SnapshotRequest (
    SnapshotRequest,
    requestDecommitTx,
    requestDepositTxId,
    requestTxs
  ),
  StateChanged (
    CommitApproved,
    DecommitApproved,
    PartySignedSnapshot,
    SnapshotConfirmed,
    SnapshotRequestDecided,
    SnapshotRequested,
    TransactionReceived,
    TxInvalid,
    decommitTxId,
    headId,
    newCurrentDepositTxId,
    newLocalTxs,
    party,
    requestedSnapshot,
    signature,
    signatures,
    snapshot,
    snapshotNumber,
    transaction,
    tx,
    utxo,
    utxoToCommit,
    utxoToDecommit,
    validationError
  ),
  TTL,
  WaitReason (
    WaitOnDepositActivation,
    WaitOnDepositObserved,
    WaitOnNotApplicableTx,
    WaitOnSeenSnapshot,
    WaitOnSnapshotNumber,
    WaitOnSnapshotVersion,
    WaitOnTxs,
    WaitOnUnresolvedCommit,
    commitUTxO,
    depositTxId
  ),
  absorbable,
  chainedSnapshotRequest,
  checkTransactionOutputs,
  finalizedDepositTxId,
  isLeader,
  namesNoneOf,
  seenSnapshotNumber,
  settledUTxO,
  settlingCommitAtPeriod,
  snapshotInFlight,
 )
import Hydra.HeadLogic.Program (AcknowledgementProgram, SnapshotProgram, TransactionProgram)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.Ledger (ValidationError (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (IsTx (..), TxIdType, UTxOType, combinedUTxO, txId, utxoFromTx, withoutUTxO)
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (Signature, Verified (..), aggregateInOrder)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

onOpenNetworkReqTxProgram ::
  forall tx.
  IsTx tx =>
  RuleEnvironment ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  tx ->
  TransactionProgram tx ()
onOpenNetworkReqTxProgram env ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx = do
  alreadyApplied <-
    Program.decide "transaction is already applied locally" $
      Map.member (txId tx) allTxs && any ((== txId tx) . txId) localTxs
  unless alreadyApplied $ do
    Program.change TransactionReceived{tx}
    let outputCheck = checkTransactionOutputs localUTxO tx
    void $ Program.decide "outputs do not collide with the local UTxO" (isRight outputCheck)
    result <-
      case outputCheck of
        Right () -> Program.applyTransaction currentSlot localUTxO tx
        Left err -> pure $ Left err
    case result of
      Left err -> notApplicable err
      Right after -> do
        freshDepositOutputs <-
          Program.decide "outputs do not collide with pending deposits" $
            namesNoneOf (utxoFromTx tx) namedByDepositsInPlay
        if freshDepositOutputs
          then do
            Program.recordAppliedTransaction headId tx localUTxO after
            requestSnapshot
          else notApplicable (ValidationError "transaction creates an output a pending deposit names")
 where
  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty

  notApplicable :: ValidationError -> TransactionProgram tx ()
  notApplicable err = do
    canRetry <- Program.decide "retry budget remains" (ttl > 0)
    if canRetry
      then Program.suspend (WaitOnNotApplicableTx err)
      -- Preserve timeout invalidation, including removal from allTxs. A delayed
      -- prerequisite can still make this transaction applicable to a later ReqSn;
      -- changing that policy is separate from the interpreted-rule migration.
      else Program.change TxInvalid{headId, utxo = localUTxO, transaction = tx, validationError = err}

  requestSnapshot :: TransactionProgram tx ()
  requestSnapshot =
    case chainedSnapshotRequest confirmedUTxO (localTxs Seq.|> tx) pendingDeposits currentDepositTxId decommitTx settlingCommit of
      Nothing -> void $ Program.decide "snapshot request has useful work" False
      Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} -> do
        void $ Program.decide "snapshot request has useful work" True
        noSnapshotInFlight <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
        when noSnapshotInFlight $ do
          leader <- Program.decide "this party leads the next snapshot" (isLeader parameters party nextSn)
          when leader $ do
            Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
            Program.emit (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)

  RuleEnvironment{party} = env
  OpenState{coordinatedHeadState, headId, parameters} = st
  CoordinatedHeadState{localTxs, localUTxO, allTxs, confirmedSnapshot, seenSnapshot, decommitTx, version, currentDepositTxId} = coordinatedHeadState
  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot
  nextSn = confirmedSn + 1
  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)
  settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot)
{-# INLINE onOpenNetworkReqTxProgram #-}

onOpenNetworkReqSnProgram ::
  forall tx.
  IsTx tx =>
  RuleEnvironment ->
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
  SnapshotProgram tx ()
onOpenNetworkReqSnProgram env pendingDeposits ChainPointTime{currentSlot, currentChainTime} st ttl otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
  -- Spec: require v = v̂ ∧ s = ŝ + 1 ∧ leader(s) = j
  requireReqSn $
    -- Spec: wait ŝ = ̅S.s
    waitNoSnapshotInFlight $
      -- Spec: wait v = v̂
      -- NOTE: must be a Wait, not a require: a follower can receive ReqSn for
      -- the bumped version before its own chain handler has processed the
      -- triggering OnIncrementTx/OnDecrementTx. Erroring here would drop the
      -- message permanently (Error outcomes are not re-enqueued), leaving the
      -- head stuck until the deposit expires.
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
                              , version = version
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
                      snapshotSignature <- Program.signSnapshot nextSnapshot
                      -- Spec: multicast (ackSn, ŝ, σᵢ)
                      do
                        Program.emit (NetworkEffect $ AckSn snapshotSignature sn)
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
                        newLocalTxs <- pruneTransactions nextUTxO
                        Program.change
                          SnapshotRequested
                            { requestedSnapshot = nextSnapshot
                            , newLocalTxs
                            , newCurrentDepositTxId = mDepositTxId
                            }
 where
  requireReqSn continue = do
    validNumber <- Program.decide "request is the next snapshot" (sn == seenSn + 1)
    unless validNumber $ Program.reject $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    leader <- Program.decide "sender leads this snapshot" (isLeader parameters otherParty sn)
    unless leader $ Program.reject $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
    continue

  waitNoSnapshotInFlight continue = do
    confirmed <- Program.decide "previous snapshot is confirmed" (confSn == seenSn)
    unless confirmed $ Program.suspend $ WaitOnSnapshotNumber seenSn
    continue

  waitOnSnapshotVersion continue = do
    matches <- Program.decide "snapshot version matches" (version == sv)
    unless matches $ Program.suspend $ WaitOnSnapshotVersion sv
    continue

  waitResolvableTxs continue =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> continue $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
      unseen -> Program.suspend $ WaitOnTxs unseen

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
        case settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot) of
          Just (ContinueCommit (_, Deposit{deposited})) -> Program.suspend WaitOnUnresolvedCommit{commitUTxO = deposited}
          _ -> cont Nothing
      Just depositTxId
        -- The deposit is already claimed by a signed snapshot whose increment
        -- settled on-chain: it only resurfaces in 'pendingDeposits' when a
        -- rollback erased that increment, and honest leaders exclude it from
        -- selection (see 'scopedDeposits'). Reject rather than sign a
        -- second snapshot claiming the same deposit, see #2741.
        | finalizedDepositTxId finalizedCommit == Just depositTxId ->
            Program.reject $ RequireFailed ReqSnDepositBlockedByFinalizedCommit{depositTxId}
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
            | ttl > 0 -> Program.suspend WaitOnDepositObserved{depositTxId}
            | otherwise -> Program.reject $ RequireFailed RequestedDepositNotFoundLocally{depositTxId}
          Just Deposit{status, deposited}
            | status == Inactive -> Program.suspend WaitOnDepositActivation{depositTxId}
            -- NOTE: this makes the commits sequential in a sense that you can't
            -- commit unless the previous commit is settled.
            | sv == confVersion && isJust confUTxOToCommit ->
                -- NOTE: identity, not just content. Two deposits can record
                -- the same UTxO, and only the one bound into the confirmed
                -- snapshot is the pending commit being settled.
                --
                -- Accepted even if the deposit is 'Expired': its increment
                -- may still land.
                if confUTxOToCommit == Just deposited && confDepositTxId == Just depositTxId
                  then cont confUTxOToCommit
                  else Program.reject $ RequireFailed ReqSnCommitNotSettled
            | status == Expired -> Program.reject $ RequireFailed RequestedDepositExpired{depositTxId}
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
            Program.reject $ RequireFailed ReqSnDecommitNotSettled
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
            Program.reject $ RequireFailed ReqSnBothCommitAndDecommit{depositTxId, decommitTxId = txId decommitTx}
      -- 'Hydra.Contract.Head.checkDecrement' requires at least one decommit
      -- output, so a decommit materializing none could never settle on-chain and
      -- would be re-proposed by every later snapshot.
      Just decommitTx
        | utxoFromTx decommitTx == mempty ->
            Program.reject $ RequireFailed ReqSnDecommitNoOutputs{decommitTxId = txId decommitTx}
      Just decommitTx ->
        -- Spec:
        -- require 𝑣 = 𝑣 ̂ ∧ 𝑠 = 𝑠 ̂ + 1 ∧ leader(𝑠) = 𝑗
        -- wait 𝑠 ̂ = 𝒮.𝑠
        if sv == confVersion && isJust confUTxOToDecommit
          then
            if confUTxOToDecommit == Just (utxoFromTx decommitTx)
              then cont (confirmedUTxO, confUTxOToDecommit)
              else Program.reject $ RequireFailed ReqSnDecommitNotSettled
          else do
            result <- Program.applyTransaction currentSlot confirmedUTxO decommitTx
            case result of
              Left err ->
                Program.reject $ RequireFailed $ SnapshotDoesNotApply sn (txId decommitTx) err
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
  requireApplyTxs :: UTxOType tx -> Maybe (UTxOType tx) -> [tx] -> (UTxOType tx -> SnapshotProgram tx ()) -> SnapshotProgram tx ()
  requireApplyTxs utxo mUtxoToCommit' requestedTxs cont
    | Just depositTxId <- mDepositTxId
    , Just deposited <- mUtxoToCommit'
    , not (absorbable utxo requestedTxs deposited) =
        Program.reject $ RequireFailed ReqSnDepositOutputsHeld{depositTxId}
    | otherwise = do
        result <- applyTransactionsProgram currentSlot utxo requestedTxs
        case result of
          Left (tx, err) ->
            Program.reject $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
          Right u -> cont u

  requireValidAccumulatorSize :: Accumulator.HydraAccumulator -> SnapshotProgram tx () -> SnapshotProgram tx ()
  requireValidAccumulatorSize accumulator continue =
    case Accumulator.checkAccumulatorSize accumulator of
      Left AccumulatorTooLarge{utxoCount, maxAllowed} ->
        Program.reject $ RequireFailed ReqSnUTxOSetTooLarge{utxoCount, maxAllowed}
      Right () -> continue

  -- \| Filter 'localTxs' to those that still apply against the running UTxO
  -- after each previous successful tx. The post-snapshot UTxO is not returned:
  -- aggregate will recompute it.
  pruneTransactions utxo0 = go utxo0 localTxs
   where
    go _ Seq.Empty = pure Seq.empty
    go u (tx Seq.:<| rest) = do
      result <- applyTransactionsProgram currentSlot u [tx]
      case result of
        Left _ -> go u rest
        Right u' -> (tx Seq.<|) <$> go u' rest
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

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version, finalizedCommit} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, headId} = st
{-# INLINE onOpenNetworkReqSnProgram #-}

onOpenNetworkAckSnProgram ::
  IsTx tx =>
  RuleEnvironment ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  AcknowledgementProgram tx ()
onOpenNetworkAckSnProgram env@RuleEnvironment{party} ChainPointTime{currentChainTime} pendingDeposits openState otherParty snapshotSignature sn =
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs snapshotBytes -> do
      -- Spec: require (j,⋅) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        -- Spec: ̂Σ[j] ← σⱼ
        (Program.change PartySignedSnapshot{snapshotNumber = snapshot.number, party = otherParty, signature = snapshotSignature} >>) $
          --       if ∀k ∈ [1..n] : (k,·) ∈ ̂Σ
          ifAllMembersHaveSigned snapshot sigs $ \sigs' -> do
            -- Spec: σ̃ ← MS-ASig(kₕˢᵉᵗᵘᵖ,̂Σ)
            let multisig = aggregateInOrder sigs' parties
            -- Spec: η ← combine(𝑈ˆ)
            --       require MS-Verify(k ̃H, (cid‖v̂‖ŝ‖η), σ̃)
            requireVerifiedMultisignature multisig snapshotBytes $
              do
                -- NOTE: Fix all the spec comments once specification is in place
                -- Spec: ̅S ← snObj(v̂, ŝ, Û, T̂, 𝑈𝛼, 𝑈𝜔)
                --       ̅S.σ ← ̃σ
                Program.change SnapshotConfirmed{headId, snapshot = Nothing, signatures = multisig}
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
    do
      valid <- Program.decide "acknowledgement number is admissible" (sn `elem` [seenSn, seenSn + 1])
      if valid
        then continue
        else Program.reject $ RequireFailed $ AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}

  waitOnSeenSnapshot continue =
    case seenSnapshot of
      -- NOTE: Ignore any redundant AckSn for snapshots we have already seen as
      -- confirmed. This is for example happening if a party runs multiple
      -- instances of hydra-node using the same keys.
      LastSeenSnapshot{lastSeen}
        | sn <= lastSeen -> pure ()
      SeenSnapshot{snapshot, signatories = sigs, signableBytes}
        | seenSn == sn -> continue snapshot sigs signableBytes
      _ -> Program.suspend WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    do
      fresh <- Program.decide "party has not signed this snapshot" (not $ Map.member otherParty sigs)
      if fresh
        then continue
        else Program.reject $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys sigs, receivedSignature = otherParty}

  ifAllMembersHaveSigned snapshot sigs cont =
    let sigs' = Map.insert otherParty snapshotSignature sigs
     in do
          complete <- Program.decide "all parties have signed" (Map.keysSet sigs' == Set.fromList parties)
          if complete
            then cont sigs'
            else
              Program.change
                PartySignedSnapshot
                  { snapshotNumber = snapshot.number
                  , party = otherParty
                  , signature = snapshotSignature
                  }

  requireVerifiedMultisignature multisig msg cont =
    do
      verified <- Program.verifySnapshot vkeys multisig msg
      case verified of
        Verified -> cont
        FailedKeys failures ->
          Program.reject $
            RequireFailed $
              InvalidMultisignature{multisig = show multisig, vkeys = failures}
        KeyNumberMismatch ->
          Program.reject $
            RequireFailed $
              InvalidMultisignature{multisig = show multisig, vkeys}

  maybeRequestNextSnapshot previous outcome =
    -- Checked against the snapshot just confirmed: the followers check the
    -- request against that snapshot too.
    case chainedSnapshotRequest (settledUTxO version previous) localTxs pendingDeposits currentDepositTxId decommitTx (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous) of
      Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}
        | isLeader parameters party nextSn ->
            outcome
              >> Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
              >> Program.emit (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)
      _ -> outcome
   where
    nextSn = previous.number + 1

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
              >> Program.change CommitApproved{headId, utxoToCommit = deposited}
              >> Program.emit
                OnChainEffect
                  { postChainTx =
                      IncrementTx
                        { headSeed
                        , headId
                        , headParameters = parameters
                        , incrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
                        , depositTxId
                        }
                  }
          Nothing -> outcome
      _ -> outcome

  maybePostDecrementTx snapshot@Snapshot{utxoToDecommit} signatures outcome =
    case (decommitTx, utxoToDecommit) of
      (Just tx, Just utxo) ->
        outcome
          >> Program.change
            DecommitApproved
              { headId
              , decommitTxId = txId tx
              , utxoToDecommit = utxo
              }
          >> Program.emit
            OnChainEffect
              { postChainTx =
                  DecrementTx
                    { headSeed
                    , headId
                    , headParameters = parameters
                    , decrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
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
{-# INLINE onOpenNetworkAckSnProgram #-}

applyTransactionsProgram :: IsTx tx => ChainSlot -> UTxOType tx -> [tx] -> SnapshotProgram tx (Either (tx, ValidationError) (UTxOType tx))
applyTransactionsProgram slot = go
 where
  go u [] = pure $ Right u
  go u (tx : rest) = do
    let collision = checkTransactionOutputs u tx
    void $ Program.decide "snapshot transaction outputs are fresh" (isRight collision)
    result <- case collision of
      Left err -> pure $ Left err
      Right () -> Program.applyTransaction slot u tx
    case result of
      Left err -> pure $ Left (tx, err)
      Right u' -> go u' rest
