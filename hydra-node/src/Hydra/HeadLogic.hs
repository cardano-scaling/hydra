{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Implements the Head Protocol's /state machine/ as /pure functions/ in an event sourced manner.
--
-- More specifically, the 'update' will handle 'Input's (or rather "commands" in
-- event sourcing speak) and convert that into a list of side-'Effect's and
-- 'StateChanged' events, which in turn are applied via 'aggregateNodeState' into
-- a single 'NodeState'.
--
-- As the specification is using a more imperative way of specifying the protocol
-- behavior, one would find the decision logic in 'update' while state updates
-- can be found in the corresponding 'applyEvent' branch.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Outcome,
) where

import Hydra.Prelude

import Data.List (elemIndex, minimumBy, partition)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..), validateClientInput)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  ChainEvent (..),
  ChainStateHistory,
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
  initHistory,
  pushNewState,
  rollbackHistory,
  setLastKnown,
 )
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  causes,
  changes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FanoutStepLanded (..),
  HeadState (..),
  IdleState (IdleState, chainState),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  Settlement (..),
  SettlementStatus (..),
  Settlements,
  UnretainedSettlements,
  getChainState,
  isCollectingAcks,
  mkSeenSnapshot,
  seenSnapshotNumber,
  setChainState,
  snapshotInFlight,
 )
import Hydra.Ledger (Ledger (..), ValidationError (..), applyTransactions)
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, SyncedStatus (..), consumeDeposit, depositsForHead, recordDeposit, retentionCutoff, rollbackDeposits, syncedStatus, updateDeposit)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..))
import Hydra.Tx (
  HeadId,
  HeadSeed,
  IsTx (..),
  TxIdType,
  UTxOType,
  combinedUTxO,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (
  Signature,
  Verified (..),
  aggregateInOrder,
  sign,
  verifyMultiSignature,
  verifyMultiSignatureBytes,
 )
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

-- * The Coordinated Head protocol

-- | Maximum number of transaction ids per snapshot. This effectively limits our
-- "block size" and ensures it does not grow arbitrarily with the backlog of
-- pending transactions (localTxs). Only applied when requesting snapshots as
-- a leader; followers accept larger requests, so this can change without a
-- coordinated upgrade.
--
-- 1000 was chosen from a sweep against 100/250 on sustained-load benchmarks
-- (see hydra-cluster/bench/BASELINES.md): per-round costs that scale with the
-- backlog dominate at small caps (4.6-5.7x lower throughput at 100 with a
-- deep backlog), while peak node memory was flat across the sweep.
maxTxsPerSnapshot :: Int
maxTxsPerSnapshot = 1000

-- ** On-Chain Protocol

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  Outcome tx
onIdleClientInit env =
  cause OnChainEffect{postChainTx = InitTx{participants, headParameters}}
 where
  headParameters = mkHeadParameters env

  Environment{participants} = env

-- | Observe an init transaction and initialize parameters in an 'OpenState'.
--
-- __Transition__: 'IdleState' → 'OpenState'
onIdleChainInitTx ::
  Environment ->
  -- | New chain state.
  ChainStateType tx ->
  HeadId ->
  HeadSeed ->
  HeadParameters ->
  [OnChainId] ->
  Outcome tx
onIdleChainInitTx env newChainState headId headSeed headParameters participants
  | configuredParties == initializedParties
      && party `member` initializedParties
      && configuredContestationPeriod == contestationPeriod
      && configuredDepositPeriod == depositPeriod
      && Set.fromList configuredParticipants == Set.fromList participants =
      newState
        HeadOpened
          { parameters = headParameters
          , chainState = newChainState
          , headId
          , headSeed
          , parties
          }
  | otherwise =
      newState
        IgnoredHeadInitializing
          { headId
          , contestationPeriod
          , parties
          , participants
          }
 where
  initializedParties = Set.fromList parties

  configuredParties = Set.fromList (party : otherParties)

  HeadParameters{parties, contestationPeriod, depositPeriod} = headParameters

  Environment
    { party
    , otherParties
    , contestationPeriod = configuredContestationPeriod
    , depositPeriod = configuredDepositPeriod
    , participants = configuredParticipants
    } = env

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
  IsTx tx =>
  Environment ->
  Ledger tx ->
  ChainSlot ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger currentSlot st ttl pendingDeposits tx =
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
    case applyTransactions currentSlot localUTxO [tx] of
      Right _ -> cont
      Left (_, err)
        | ttl > 0 ->
            wait (WaitOnNotApplicableTx err)
        | otherwise ->
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
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then
        outcome
          -- XXX: This state update has no equivalence in the
          -- spec. Do we really need to store that we have
          -- requested a snapshot? If yes, should update spec.
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause
            ( NetworkEffect $
                let (nextDecommitTx, nextDeposit) =
                      selectNextIncrementalAction
                        pendingDeposits
                        currentDepositTxId
                        decommitTx
                        version
                        (getSnapshot confirmedSnapshot)
                 in ReqSn
                      version
                      nextSn
                      (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs')
                      nextDecommitTx
                      nextDeposit
            )
      else outcome

  Environment{party} = env

  Ledger{applyTransactions} = ledger

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
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  PendingDeposits tx ->
  ChainSlot ->
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
onOpenNetworkReqSn env ledger pendingDeposits currentSlot st ttl otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
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
        requireApplicableDecommitTx $ \(activeUTxOAfterDecommit, mUtxoToDecommit) ->
          -- Wait for the deposit and require any pending commit to be consistent
          waitForDeposit activeUTxOAfterDecommit $ \(activeUTxO, mUtxoToCommit) ->
            -- Resolve transactions by-id
            waitResolvableTxs $ \requestedTxs -> do
              -- Spec: require 𝑈_active ◦ Treq ≠ ⊥
              --       𝑈 ← 𝑈_active ◦ Treq
              requireApplyTxs activeUTxO requestedTxs $ \u ->
                let nextUTxO = u `withoutUTxO` fromMaybe mempty mUtxoToCommit
                    -- The predecessor is confirmed at this point (see
                    -- requireReqSn and waitNoSnapshotInFlight), so its two
                    -- accumulators cover exactly its own two owed sets (see
                    -- 'Accumulator.buildFromSnapshotUTxOs') and can be updated by
                    -- the UTxO delta instead of re-serializing and re-hashing
                    -- every output.
                    prevSnapshot = getSnapshot confirmedSnapshot
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
                        let newLocalTxs = pruneTransactions u
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

  waitForDeposit activeUTxOAfterDecommit cont =
    case mDepositTxId of
      Nothing -> cont (activeUTxOAfterDecommit, Nothing)
      Just depositTxId
        -- A proposal one version behind us carries the confirmed snapshot's own
        -- commit again ('waitOnSnapshotVersion'). We already saw its increment
        -- land, so here the deposit is consumed and retained, while the parties
        -- that have not seen the increment still hold it pending and sign a
        -- snapshot carrying it. Sign the same bytes, taking the deposited
        -- outputs from the confirmed snapshot. Only while the retained
        -- settlement is landed: once a rollback erased the increment, the
        -- deposit is settled by re-posting that snapshot and never by a new
        -- claim (#2741), which the next guard refuses.
        | sv == confVersion
        , confDepositTxId == Just depositTxId
        , Just deposited <- confUTxOToCommit
        , confirmedSettlementLanded ->
            cont (activeUTxOAfterDecommit <> deposited, confUTxOToCommit)
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
            -- The leader carries the confirmed snapshot's own pending commit
            -- again (see 'selectNextDeposit'). Match by identity, not by
            -- content: two deposits can record the same UTxO, and only the one
            -- bound into the confirmed snapshot is being settled. The deposit's
            -- local status does not matter here: the claim is signed and its
            -- increment is in flight, so our own expiry margin cannot stop it
            -- from landing (see 'stillClaimable'). Refusing would leave a round
            -- nobody signs, since every party's copy expires at the same time.
            | sv == confVersion
            , confDepositTxId == Just depositTxId
            , confUTxOToCommit == Just deposited ->
                cont (activeUTxOAfterDecommit <> deposited, confUTxOToCommit)
            | status == Inactive -> wait WaitOnDepositActivation{depositTxId}
            | status == Expired -> Error $ RequireFailed RequestedDepositExpired{depositTxId}
            -- NOTE: this makes the commits sequential in a sense that you can't
            -- commit unless the previous commit is settled.
            | sv == confVersion
            , isJust confUTxOToCommit
            , -- Only while the claimed deposit is still tracked: once it was
              -- recovered on L1 the claim is dropped (see 'stillClaimable') and
              -- the leader may propose a fresh one.
              maybe False (`Map.member` pendingDeposits) confDepositTxId ->
                Error $ RequireFailed ReqSnCommitNotSettled
            | otherwise -> do
                let activeUTxOAfterCommit = activeUTxOAfterDecommit <> deposited
                cont (activeUTxOAfterCommit, Just deposited)

  requireApplicableDecommitTx cont =
    case mDecommitTx of
      Nothing -> cont (confirmedUTxO, Nothing)
      -- Spec: require tx𝜔 = ⊥ ∨ tx𝛼 = ⊥
      --
      -- A snapshot settling both a commit and a decommit cannot be closed:
      -- close and fanout express a single incremental action
      -- ('setIncrementalActionMaybe'). The leader never proposes both (see
      -- 'selectNextIncrementalAction'), so this rejects a request that does
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
  requireApplyTxs utxo requestedTxs cont =
    case applyTransactions ledger currentSlot utxo requestedTxs of
      Left (tx, err) ->
        Error $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
      Right u -> cont u

  requireValidAccumulatorSize :: Accumulator.HydraAccumulator -> Outcome tx -> Outcome tx
  requireValidAccumulatorSize accumulator continue
    | Accumulator.accumulatorSize accumulator > Accumulator.maxAccumulatorSize =
        Error $
          RequireFailed $
            ReqSnUTxOSetTooLarge
              { utxoCount = Accumulator.accumulatorSize accumulator
              , maxAllowed = Accumulator.maxAccumulatorSize
              }
    | otherwise =
        continue

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
      case applyTransactions ledger currentSlot u [tx] of
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

  -- Whether the settlement of the confirmed snapshot's own commit or decommit
  -- landed on the chain we follow: it is retained at its version and no
  -- rollback marked it erased (see 'retainSettlement', 'markErased').
  confirmedSettlementLanded =
    case Map.lookup confVersion settlements of
      Just Settlement{status = Landed{}} -> True
      _ -> False

  seenSn = seenSnapshotNumber seenSnapshot

  -- The base UTxO of the requested snapshot. The confirmed snapshot's pending
  -- commit is spendable only if its increment had landed at the version being
  -- signed. That is why this keys on @sv@ and not on our own version: for a
  -- proposal one version behind us, using our version would give this node a
  -- different base UTxO, and so different signed bytes, than everyone else.
  confirmedUTxO = case confirmedSnapshot of
    InitialSnapshot{} -> mempty
    ConfirmedSnapshot{snapshot = Snapshot{utxo, utxoToCommit, version = snapshotVersion}} ->
      if sv > snapshotVersion
        then utxo <> fromMaybe mempty utxoToCommit
        else utxo

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
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn Environment{party} pendingDeposits openState otherParty snapshotSignature sn =
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs snapshotBytes -> do
      -- Spec: require (j,⋅) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        -- Spec: ̂Σ[j] ← σⱼ
        (newState PartySignedSnapshot{snapshotNumber = snapshot.number, party = otherParty, signature = snapshotSignature} <>) $
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

  ifAllMembersHaveSigned snapshot sigs cont =
    let sigs' = Map.insert otherParty snapshotSignature sigs
     in if Map.keysSet sigs' == Set.fromList parties
          then cont sigs'
          else
            newState
              PartySignedSnapshot
                { snapshotNumber = snapshot.number
                , party = otherParty
                , signature = snapshotSignature
                }

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

  maybeRequestNextSnapshot previous outcome = do
    let nextSn = previous.number + 1
        unsettled = unsettledCommit version previous
        (nextDecommitTx, nextDeposit) =
          selectNextIncrementalAction pendingDeposits currentDepositTxId decommitTx version previous
        carriesNewAction =
          (isJust nextDeposit && nextDeposit /= fmap snd unsettled)
            || (isJust nextDecommitTx && isNothing (unsettledDecommit version previous))
    -- A snapshot carrying only a deposit or a decommit is fine; the tick
    -- requests exactly that. Requiring local txs here left a queued deposit or
    -- decommit unrequested on a head with no traffic, until the deposit
    -- expired.
    --
    -- Request only if the snapshot carries something new. A queued deposit or
    -- decommit stays queued until its increment or decrement lands, so firing
    -- on it again would request one snapshot per round trip, and re-post the
    -- settlement each time, until then.
    if isLeader parameters party nextSn && (not (null localTxs) || carriesNewAction)
      then
        outcome
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) nextDecommitTx nextDeposit)
      else outcome

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

-- | Client request to recover deposited UTxO.
--
-- __Transition__: 'OpenState' → 'OpenState'
-- Client request to recover a deposit by posting a recover transaction on-chain.
-- Works in any head state (Open, Closed, or Idle after fanout). Deposits from a
-- previous head are never cleared from 'pendingDeposits' on fanout, so recovery
-- remains available after a head closes. A new head only sees its own deposits via
-- 'depositsForHead', so old deposits are never accidentally ingested into L2.
-- On-chain, the deposit validator only enforces that the deadline has passed and
-- that the recovered outputs match the originals — it does not require the head to
-- still be active.
onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  -- | Deposits claimed by the retained increments of an open head (see
  -- 'openRetainedDeposits').
  Set (TxIdType tx) ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits blockedDeposits recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Error $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited}
      -- The deposit resurfaced because its finalized increment was rolled
      -- back: the deposited funds are already merged into the head, so
      -- recovering them on-chain would corrupt the L2 ledger. Only re-posting
      -- the increment settles this deposit, see #2741.
      | recoverTxId `Set.member` blockedDeposits ->
          Error $ RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}
      | otherwise ->
          causes
            [ OnChainEffect
                { postChainTx =
                    RecoverTx
                      { headId
                      , recoverTxId = recoverTxId
                      , -- XXX: Why is this called deadline?
                        deadline = currentSlot
                      , recoverUTxO = deposited
                      }
                }
            ]

-- | Client request to decommit UTxO from the head.
--
-- Only possible if there is no decommit _in flight_ and if the tx applies
-- cleanly to the local ledger state.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientDecommit ::
  IsTx tx =>
  HeadId ->
  Ledger tx ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  -- | Decommit transaction.
  tx ->
  Outcome tx
onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx =
  checkNoDecommitInFlight $
    checkValidDecommitTx $
      requireDecommitOutputs headId localUTxO decommitTx $
        cause (NetworkEffect ReqDec{transaction = decommitTx})
 where
  checkNoDecommitInFlight continue =
    case mExistingDecommitTx of
      Just existingDecommitTx ->
        newState
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitAlreadyInFlight
                  { otherDecommitTxId = txId existingDecommitTx
                  }
            }
      Nothing -> continue

  checkValidDecommitTx cont =
    case applyTransactions ledger currentSlot localUTxO [decommitTx] of
      Left (_, err) ->
        newState
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitTxInvalid
                  { localUTxO
                  , validationError = err
                  }
            }
      Right _ -> cont

  CoordinatedHeadState{decommitTx = mExistingDecommitTx, localUTxO} = coordinatedHeadState

-- | Process the request 'ReqDec' to decommit something from the Open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- When node receives 'ReqDec' network message it should:
-- - Check there is no decommit in flight:
--   - Alter it's state to record what is to be decommitted
--   - Issue a server output 'DecommitRequested' with the relevant utxo
--   - Issue a 'ReqSn' since all parties need to agree in order for decommit to
--   be taken out of a Head.
-- - Check if we are the leader
onOpenNetworkReqDec ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  TTL ->
  ChainSlot ->
  PendingDeposits tx ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl currentSlot pendingDeposits openState decommitTx =
  -- Spec: wait 𝑈𝛼 = ∅ ^ txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $
    requireDecommitOutputs headId localUTxO decommitTx $
      -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
      -- Spec: txω ← tx
      newState DecommitRecorded{headId, decommitTx}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        <> maybeRequestSnapshot
 where
  -- Spec: wait 𝑈𝛼 = ∅. A snapshot must never carry both a commit and a
  -- decommit, and must never drop a commit that is still in flight. Both are
  -- enforced where the snapshot is proposed, not here: the leader carries the
  -- pending commit first and the recorded decommit in a later round
  -- ('selectNextIncrementalAction'), and every receiver rejects a proposal
  -- carrying both ('ReqSnBothCommitAndDecommit').
  --
  -- A ReqDec is a broadcast, and every party must decide the same way. Whether
  -- a deposit is queued locally depends on this node's own tick. Holding the
  -- ReqDec back on that, and rejecting it once its ttl ran out, refused the
  -- same request on some nodes and recorded it on others. The nodes that
  -- recorded it then held a decommit the leader never knew about, and it was
  -- never proposed. The model's concurrent walk found this once the late-ReqSn
  -- deadlock stopped hiding it. Only state that every party shares may decide
  -- here: a decommit already in flight, and whether the transaction applies.
  waitOnApplicableDecommit cont =
    case mExistingDecommitTx of
      Nothing ->
        case applyTransactions currentSlot localUTxO [decommitTx] of
          Right _ -> cont
          Left (_, validationError)
            | ttl > 0 ->
                wait $
                  WaitOnNotApplicableDecommitTx
                    ServerOutput.DecommitTxInvalid{localUTxO, validationError}
            | otherwise ->
                newState
                  DecommitInvalid
                    { headId
                    , decommitTx
                    , decommitInvalidReason =
                        ServerOutput.DecommitTxInvalid{localUTxO, validationError}
                    }
      Just existingDecommitTx
        | ttl > 0 ->
            wait $
              WaitOnNotApplicableDecommitTx
                DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
        | otherwise ->
            newState
              DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason =
                    DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
                }

  -- Go through the same selector as every other proposal site: a pending
  -- commit (a queued deposit, or the confirmed snapshot's unsettled claim) is
  -- carried first, and the decommit just recorded follows in a later round.
  -- That keeps a snapshot from carrying both, and from dropping an unsettled
  -- commit. See 'waitOnApplicableDecommit' for why the ReqDec itself is not
  -- held back instead.
  maybeRequestSnapshot =
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then
        let (nextDecommitTx, nextDeposit) =
              selectNextIncrementalAction
                pendingDeposits
                currentDepositTxId
                (Just decommitTx)
                version
                (getSnapshot confirmedSnapshot)
         in cause (NetworkEffect (ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) nextDecommitTx nextDeposit))
      else noop

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  CoordinatedHeadState
    { decommitTx = mExistingDecommitTx
    , confirmedSnapshot
    , localTxs
    , localUTxO
    , version
    , seenSnapshot
    , currentDepositTxId
    } = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
    } = openState

determineNextDepositStatus :: forall tx. Environment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  -- NOTE: the annotation (and hence the forall) disambiguates the record
  -- update: 'status' is also a field of 'Settlement'.
  (\deposit -> (deposit :: Deposit tx){status = determineStatus deposit}) <$> pendingDeposits
 where
  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositActivation = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  Environment{depositPeriod, depositActivation} = env

-- | Process the chain (and time) advancing in any head state.
--
-- __Transition__: 'AnyState' → 'AnyState'
--
-- This is primarily used to track deposits status changes.
onChainTick :: IsTx tx => Environment -> PendingDeposits tx -> UTCTime -> Outcome tx
onChainTick env pendingDeposits chainTime =
  mkDepositActivated newActive <> mkDepositExpired newExpired
 where
  -- XXX: This is a bit messy
  newActive = Map.difference nextActive pendingActive

  newExpired = Map.difference nextExpired pendingExpired

  pendingActive = Map.filter (\Deposit{status} -> status == Active) pendingDeposits

  pendingExpired = Map.filter (\Deposit{status} -> status == Expired) pendingDeposits

  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  nextActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  nextExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  mkDepositActivated m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositActivated{depositTxId, chainTime, deposit}

  mkDepositExpired m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositExpired{depositTxId, chainTime, deposit}

-- | Process the chain (and time) advancing in an open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- This is primarily used to track deposits and either drop them or request
-- snapshots for inclusion.
onOpenChainTick :: IsTx tx => Environment -> UTCTime -> PendingDeposits tx -> OpenState tx -> Outcome tx
onOpenChainTick env chainTime pendingDeposits st =
  -- Determine new active and new expired
  let nextDeposits = determineNextDepositStatus env pendingDeposits chainTime
      newActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits
      newExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits
   in -- Apply state changes and pick next active to request snapshot
      -- XXX: This is smelly as we rely on Map <> to override entries (left
      -- biased). This is also weird because we want to actually apply the state
      -- change and also to determine the next active.
      withNextActive currentDepositTxId (newActive <> newExpired <> pendingDeposits) $ \depositTxId ->
        -- REVIEW: this is not really a wait, but discard?
        -- TODO: Spec: wait tx𝜔 = ⊥ ∧ 𝑈𝛼 = ∅
        if isNothing decommitTx
          -- Nothing to request while the confirmed snapshot's claim is still
          -- unsettled. That deposit stays queued until its increment lands, and
          -- from then on the increment settles it, not another snapshot, so
          -- requesting it again would send one snapshot per tick. Every party
          -- refuses any other deposit until then ('ReqSnCommitNotSettled'), and
          -- the leader would sit in 'RequestedSnapshot' on its own rejected
          -- echo. The claim may not even be the pick: 'withNextActive' skips
          -- it once it is no longer active locally, while its increment can
          -- still land.
          && isNothing confirmedDepositTxId
          && not (snapshotInFlight seenSnapshot)
          && isLeader parameters party nextSn
          then
            -- XXX: This state update has no equivalence in the
            -- spec. Do we really need to store that we have
            -- requested a snapshot? If yes, should update spec.
            newState SnapshotRequestDecided{snapshotNumber = nextSn}
              -- Spec: multicast (reqSn,̂ 𝑣,̄ 𝒮.𝑠 + 1,̂ 𝒯, 𝑈𝛼, ⊥)
              <> cause (NetworkEffect $ ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) Nothing (Just depositTxId))
          else
            noop
 where
  -- Pending active deposits are picked in arrival order, except that the
  -- deposit already queued in 'currentDepositTxId' goes first. It is parked
  -- there when it activates while a snapshot is in flight, and this tick is
  -- then the only thing left to request it. The queued deposit used to be the
  -- tick's own reason not to request anything, so on a head with no local txs
  -- it sat there until it expired.
  withNextActive ::
    forall tx.
    IsTx tx =>
    Maybe (TxIdType tx) ->
    Map (TxIdType tx) (Deposit tx) ->
    (TxIdType tx -> Outcome tx) ->
    Outcome tx
  withNextActive queued deposits cont = do
    -- NOTE: Do not consider empty deposits.
    let p :: (x, Deposit tx) -> Bool
        p (_, Deposit{deposited, status}) = deposited /= mempty && status == Active
    case filter p (Map.toList deposits) of
      [] -> noop
      xs
        -- Preferred only while it is still active: a queued deposit that
        -- expired or was consumed must not hold up the others.
        | Just depositTxId <- queued
        , depositTxId `elem` (fst <$> xs) ->
            cont depositTxId
        | otherwise -> cont (fst (minimumBy (comparing ((\Deposit{created} -> created) . snd)) xs))

  nextSn = confirmedSn + 1

  Environment{party} = env

  CoordinatedHeadState
    { localTxs
    , confirmedSnapshot
    , seenSnapshot
    , version
    , decommitTx
    , currentDepositTxId
    } = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  -- The deposit the confirmed snapshot still has pending on chain, if any.
  -- Read through 'unsettledCommit' rather than off the snapshot, so that a
  -- confirmed snapshot left one version behind the chain does not look like a
  -- commit in flight forever, and through 'stillClaimable', so that a
  -- recovered deposit does not either.
  confirmedDepositTxId = snd <$> stillClaimable pendingDeposits (unsettledCommit version (getSnapshot confirmedSnapshot))

  OpenState{coordinatedHeadState, parameters} = st

-- | If this node is the snapshot leader and there is something to snapshot
-- (local transactions, a deposit to carry, or a decommit recorded while the
-- commit was in flight), request the next snapshot with the bumped version
-- after a commit or decommit lands on chain.
--
-- Guards:
--   * Only when 'newVersion' is ahead of our 'version'. Several parties post
--     the same transaction and each posting is observed, so this avoids one
--     'SnapshotRequestDecided' per observation. It also keeps a settlement
--     that landed again after a rollback (its 'newVersion' is at or behind our
--     version, which never goes down) from requesting a snapshot with a stale
--     version.
--   * Not while signatures are being collected ('SeenSnapshot'). That snapshot
--     will complete, because every party can sign it: those that saw the bump
--     before the ReqSn sign one version behind their own (see
--     'waitOnSnapshotVersion'), and 'maybeRequestNextSnapshot' then requests
--     the next one with the bumped version. Firing here would use stale
--     'localTxs' and cause 'BadInputsUTxO' on other parties.
--   * Allowed in 'RequestedSnapshot': our own request at the old version is
--     still on its way. Requesting again at the new version costs nothing,
--     since every party rejects a second request for the same number as a
--     duplicate, and it covers the case where the first one was lost.
--
-- The optional 'depositTxId' goes into the 'ReqSn': commit finalisation passes
-- 'Nothing', since the deposit is already included, and decommit finalisation
-- passes the next queued deposit if there is one. The optional decommit is one
-- recorded while the commit was in flight: every proposal carried the commit
-- first, and on a head with no local txs nothing else would propose it now
-- that the increment landed. Commit finalisation passes it; decommit
-- finalisation passes 'Nothing', since the decommit that settled is being
-- cleared and no other could be recorded meanwhile. A deposit wins over the
-- decommit, as in 'selectNextIncrementalAction'.
maybeRequestSnapshotAfterVersionBump ::
  IsTx tx =>
  HeadParameters ->
  Party ->
  SnapshotNumber ->
  Seq tx ->
  SnapshotVersion ->
  SnapshotVersion ->
  SeenSnapshot tx ->
  Maybe (TxIdType tx) ->
  Maybe tx ->
  Outcome tx
maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot depositTxId recordedDecommitTx =
  if isLeader parameters party nextSn && (not (null localTxs) || isJust nextDeposit || isJust nextDecommitTx) && newVersion > version && not (isCollectingAcks seenSnapshot)
    then
      newState SnapshotRequestDecided{snapshotNumber = nextSn}
        <> cause (NetworkEffect $ ReqSn newVersion nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) nextDecommitTx nextDeposit)
    else noop
 where
  (nextDecommitTx, nextDeposit) = case depositTxId of
    Just _ -> (Nothing, depositTxId)
    Nothing -> (recordedDecommitTx, Nothing)

-- | Observe a increment transaction. If the outputs match the ones of the
-- pending commit UTxO, then we consider the deposit/increment finalized, and remove the
-- increment UTxO from 'pendingDeposits' from the local state.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainIncrementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Deposit TxId
  TxIdType tx ->
  Outcome tx
onOpenChainIncrementTx env pendingDeposits openState newChainState newVersion depositTxId =
  newState CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
    <> maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot Nothing decommitTx
    <> repostAfterRelanding pendingDeposits openState newChainState newVersion
 where
  OpenState{headId, parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, version, seenSnapshot, decommitTx} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

-- | Observe a decrement transaction. If the outputs match the ones of the
-- pending decommit tx, then we consider the decommit finalized, and remove the
-- decommit tx in flight.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainDecrementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Outputs removed by the decrement
  UTxOType tx ->
  Outcome tx
onOpenChainDecrementTx env pendingDeposits openState newChainState newVersion distributedUTxO =
  newState
    DecommitFinalized
      { chainState = newChainState
      , headId
      , newVersion
      , distributedUTxO
      }
    <> maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot (setExistingDeposit pendingDeposits currentDepositTxId) Nothing
    <> repostAfterRelanding pendingDeposits openState newChainState newVersion
 where
  OpenState{headId, parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, currentDepositTxId, version, seenSnapshot} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- ** Closing the Head

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, ξ)
  cause
    OnChainEffect
      { postChainTx =
          CloseTx
            { headId
            , headParameters = parameters
            , openVersion = version
            , closingSnapshot = confirmedSnapshot
            }
      }
 where
  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId, parameters} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  IsTx tx =>
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  newState HeadClosed{headId, snapshotNumber = closedSnapshotNumber, chainState = newChainState, contestationDeadline}
    & maybePostContest
 where
  maybePostContest outcome =
    -- Spec: if ̅S.s > sc
    if number (getSnapshot confirmedSnapshot) > closedSnapshotNumber
      then
        outcome
          -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
          --       ξ ← ̅S.σ
          --       postTx (contest, ̅S.v, ̅S.s, η, ξ)
          <> cause
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
      else outcome

  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{parameters = headParameters, headId, coordinatedHeadState} = openState

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

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state.
  ChainStateType tx ->
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline =
  if
    | -- Spec: if ̅S.s > sc
      number (getSnapshot confirmedSnapshot) > snapshotNumber ->
        -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
        --       ξ ← ̅S.σ
        --       postTx (contest, ̅S.v, ̅S.s, η, ξ)
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
          <> cause
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
    | snapshotNumber > number (getSnapshot confirmedSnapshot) ->
        -- TODO: A more recent snapshot number was successfully contested, we will
        -- not be able to fanout! We might want to communicate that to the client!
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
    | otherwise ->
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
 where
  ClosedState{parameters = headParameters, confirmedSnapshot, headId, version} = closedState

-- | Client request to fanout the whole closed head automatically. Emits a
-- 'FanoutTx'; the chain layer either lands a single full fanout (→ 'IdleState')
-- or falls back to dynamically-chunked partial fanouts. The first observed
-- partial fanout transitions the head to 'PartialFanout' in 'AutoDrain' mode,
-- which keeps draining the rest automatically until the final (burning) step.
--
-- This node becomes the fanout /driver/: it transitions into 'PartialFanout' in
-- 'AutoDrain' mode so that, as the chain layer chunks the fanout, /this/ node
-- auto-continues to completion. Other parties that merely observe the resulting
-- partial fanout do not auto-drive (see 'onClosedChainPartialFanoutTx').
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState' (then → 'IdleState' once
-- the final fanout is observed).
onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  -- A plain 'Fanout' is a full fanout by definition, and is only reachable from
  -- 'Closed'.
  fanoutStepStateChange headId FullFanoutStep fullUTxO
    <> emitFanoutStep FullFanoutStep confirmedSnapshot version headSeed contestationDeadline
 where
  fullUTxO = computeFullFanoutUTxO closedState

  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | The state change that goes with a fanout step, for the callers that own the
-- head state: taking the driver role, or recording the selection being
-- distributed. Decided here once rather than in each handler.
--
-- The selection comes from the step rather than the caller, so a step can only
-- ever be recorded against the set it actually distributes.
fanoutStepStateChange ::
  HeadId ->
  NextFanoutStep tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  Outcome tx
fanoutStepStateChange headId step remainingOutputs =
  case step of
    -- The selection is not distributed as one: it becomes a full fanout, and this
    -- node drives the rest of it.
    FullFanoutStep -> newState HeadFanoutInitiated{headId, remainingOutputs}
    -- 'FinalStep' posts a transaction and nothing else, so the selection has to
    -- be recorded for it too: a rollback or restart before that transaction
    -- lands otherwise leaves the driver with no selection to resume from.
    FinalStep{stepDistribute} -> recordSelection stepDistribute
    PartialStep{stepDistribute} -> recordSelection stepDistribute
 where
  recordSelection selection = newState HeadPartialFanoutSelected{headId, remainingOutputs, selection}

-- | Which transaction the next fanout step has to be, carrying the sets that
-- transaction needs. Decided once by 'nextFanoutStep' so that callers deciding
-- what else to record and 'emitFanoutStep' deciding what to post cannot
-- disagree, and so that no caller can pair a step with sets it does not go with.
data NextFanoutStep tx
  = -- | The target is the whole remainder and the head is already in
    -- @FanoutProgress@ on chain: the final step, distributing the rest and
    -- burning the head tokens.
    FinalStep {stepDistribute :: UTxOType tx}
  | -- | The target is the whole remainder but the head output still carries the
    -- @Closed@ datum, which the final step is not valid against. Posted as a
    -- non-final step it would empty the head, which 'mustNotBeLastBatch' rejects,
    -- so the chunk search settles for one output less and the head needs a second
    -- transaction to finish — and cannot be drained at all when a single output
    -- is left. Covering everything is a full fanout, so post that (#2855).
    FullFanoutStep
  | -- | The target is a strict subset: a non-final chunk distributing it, proved
    -- against the set the head's current datum commits to.
    PartialStep {stepDistribute :: UTxOType tx, stepProof :: UTxOType tx}

-- | Decide the next fanout step. The multiset comparison here is the only one
-- needed: callers pass the result on rather than re-deriving it.
nextFanoutStep ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  -- | Chunk source for the next step (the user selection, or the whole remaining
  --   set when auto-draining)
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  OnChainFanoutDatum ->
  NextFanoutStep tx
nextFanoutStep confirmedSnapshot version target remaining onChainDatum
  | target `sameOutputs` remaining =
      case onChainDatum of
        DatumFanoutProgress -> FinalStep{stepDistribute = remaining}
        DatumClosed -> FullFanoutStep
  | otherwise = PartialStep{stepDistribute = target, stepProof = proofSet}
 where
  -- The set the on-chain datum commits to: the fan-out-able set from a @Closed@
  -- head, and the not-yet-distributed set once the head is in @FanoutProgress@,
  -- since every step removes exactly what it distributed.
  proofSet = case onChainDatum of
    DatumClosed -> fanoutUTxOFromSnapshot confirmedSnapshot version
    DatumFanoutProgress -> remaining

-- | Given the on-chain @version@ and a snapshot's own version, decide which of a
-- pending commit / decommit is still to be distributed on fanout. When the
-- increment has landed on chain (versions match) the commit was already applied
-- (drop it) while a pending decommit still applies; otherwise the commit still
-- applies and the decommit was already paid out. Centralises the version check
-- shared by 'mkFullFanoutTx' and 'fanoutUTxOFromSnapshot'.
effectiveCommitDecommit ::
  -- | On-chain version
  SnapshotVersion ->
  -- | Snapshot version
  SnapshotVersion ->
  -- | Pending commit
  Maybe (UTxOType tx) ->
  -- | Pending decommit
  Maybe (UTxOType tx) ->
  (Maybe (UTxOType tx), Maybe (UTxOType tx))
effectiveCommitDecommit onChainVersion snapshotVersion utxoToCommit utxoToDecommit
  | snapshotVersion == onChainVersion = (Nothing, utxoToDecommit)
  | otherwise = (utxoToCommit, Nothing)

-- | Build the full automatic 'FanoutTx' from a confirmed snapshot at the given
-- on-chain version. Shared by 'onClosedClientFanout' and the rollback re-post in
-- 'FanoutProgress' ('repostFanoutStep').
mkFullFanoutTx ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  PostChainTx tx
mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline =
  FanoutTx
    { utxo
    , utxoToCommit = effectiveCommit
    , utxoToDecommit = effectiveDecommit
    , headSeed
    , contestationDeadline
    }
 where
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot

-- | Client request to fan out a user-selected subset of a freshly closed head.
-- Validates the selection is a non-empty sub-multiset (by content) of the
-- fan-out-able UTxO, then transitions the head into 'PartialFanout' and posts
-- whichever step 'nextFanoutStep' decides on: a strict subset records the
-- selection and posts a 'PartialFanoutTx', while a selection covering the whole
-- head is a full fanout and drains it automatically instead.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedClientPartialFanout ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  Outcome tx
onClosedClientPartialFanout closedState selection
  | nullOutputs selection || not (selection `isSubMultisetOf` fullUTxO) =
      cause . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (Closed closedState)
  | otherwise =
      -- A selection covering the whole head becomes a full fanout - what the user
      -- means by "fan out everything", and the only option for a head holding a
      -- single UTxO, since a non-final batch has to leave one behind.
      fanoutStepStateChange headId step fullUTxO
        <> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- Fresh head: the on-chain datum is still @Closed@.
  step = nextFanoutStep confirmedSnapshot version selection fullUTxO DatumClosed

  fullUTxO = computeFullFanoutUTxO closedState
  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | Client request to continue a selective partial fanout. Validates the
-- selection against the current 'remainingOutputs' and emits whichever step
-- 'nextFanoutStep' decides on, which is not always a recorded selection: one
-- covering the whole remainder finalizes the head, or, if nothing has been
-- distributed yet, becomes a full fanout that drains it automatically.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutClientPartialFanout ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  Outcome tx
onPartialFanoutClientPartialFanout pfs selection
  | nullOutputs selection || not (selection `isSubMultisetOf` remainingOutputs) =
      cause . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (FanoutProgress pfs)
  | otherwise =
      fanoutStepStateChange headId step remainingOutputs
        <> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- The on-chain datum is only @FanoutProgress@ once a partial fanout has
  -- actually landed (some outputs distributed); until then it is still @Closed@
  -- and a 'FinalPartialFanoutTx' is not yet valid. Compute this the same way as
  -- 'repostFanoutStep' rather than assuming 'True'.
  step = nextFanoutStep confirmedSnapshot version selection remainingOutputs (onChainFanoutDatum distributedOutputs)

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, distributedOutputs} = pfs

-- | Observe a (full or final) fanout transaction, finalizing the head.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  newState HeadFannedOut{headId, finalizedOutputs = fanoutUTxO, chainState = newChainState}
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while this node is still 'Closed' — i.e. a partial
-- fanout this node did NOT initiate (another party did). The fanout driver moved
-- to 'PartialFanout' when it issued its 'Fanout'/'PartialFanout' command, so this
-- handler is only reached by passive observers.
--
-- The observer transitions into 'PartialFanout' in 'AwaitingSelection' mode and
-- does __not__ auto-drive the rest: only the driver advances the fanout. This is
-- what makes selective partial fanout work in a multi-party head — observers must
-- not steamroll the remaining UTxO the driver deliberately left.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Outcome tx
onClosedChainPartialFanoutTx closedState newChainState observedDistributed =
  let fullUTxO = computeFullFanoutUTxO closedState
      remaining = removeDistributedOutputs (outputsOfUTxO observedDistributed) fullUTxO
      distributedUTxO = withoutUTxO fullUTxO remaining
   in newState
        HeadPartialFannedOut
          { headId
          , distributedOutputs = distributedUTxO
          , remainingOutputs = remaining
          , chainState = newChainState
          , mode = AwaitingSelection
          }
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while in 'PartialFanout'. Updates the remaining and
-- distributed sets and, depending on the current 'FanoutMode', either continues
-- draining automatically (or within the active selection) or waits for the next
-- 'PartialFanout' command.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  Outcome tx
onPartialFanoutChainPartialFanoutTx pfs newChainState observedDistributed =
  let observedOutputs = outputsOfUTxO observedDistributed
      remaining = removeDistributedOutputs observedOutputs remainingOutputs
      distributedUTxO = withoutUTxO remainingOutputs remaining
      newMode = case mode of
        AutoDrain -> AutoDrain
        AwaitingSelection -> AwaitingSelection
        DistributingSelection sel ->
          let sel' = removeDistributedOutputs observedOutputs sel
           in if nullOutputs sel' then AwaitingSelection else DistributingSelection sel'
      record =
        newState
          HeadPartialFannedOut
            { headId
            , distributedOutputs = distributedUTxO
            , remainingOutputs = remaining
            , chainState = newChainState
            , mode = newMode
            }
      -- Already in 'FanoutProgress' on chain, so any continuation may finalize.
      finalize = emitStep (nextFanoutStep confirmedSnapshot version remaining remaining DatumFanoutProgress)
      continue
        -- The head's remaining set is now empty: emit the final (burning) step.
        -- This must happen regardless of mode — otherwise a selection that
        -- drains everything would stop at 'AwaitingSelection' and wedge the
        -- head, never burning the tokens.
        | nullOutputs remaining = finalize
        | otherwise = case newMode of
            AutoDrain -> finalize
            DistributingSelection sel' ->
              emitStep (nextFanoutStep confirmedSnapshot version sel' remaining DatumFanoutProgress)
            AwaitingSelection -> noop
   in record <> continue
 where
  emitStep step = emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, mode} = pfs

-- | Observe the final fanout while in 'PartialFanout', finalizing the head with
-- the accumulated distributed outputs plus this final batch.
--
-- __Transition__: 'PartialFanoutState' → 'IdleState'
onPartialFanoutChainFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Outcome tx
onPartialFanoutChainFanoutTx pfs newChainState fanoutUTxO =
  newState HeadFannedOut{headId, finalizedOutputs = distributedOutputs <> fanoutUTxO, chainState = newChainState}
 where
  PartialFanoutState{headId, distributedOutputs} = pfs

-- | Compute the full UTxO set to be fanned out, combining snapshot utxo
-- with utxoToCommit/utxoToDecommit based on version.
computeFullFanoutUTxO ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx
computeFullFanoutUTxO ClosedState{confirmedSnapshot, version} =
  fanoutUTxOFromSnapshot confirmedSnapshot version

-- | The fan-out-able UTxO of a confirmed snapshot at the given on-chain version:
-- the snapshot UTxO plus a pending commit (if the increment landed on chain) or a
-- pending decommit (if the decrement has not landed yet).
fanoutUTxOFromSnapshot ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  UTxOType tx
fanoutUTxOFromSnapshot confirmedSnapshot version =
  combinedUTxO utxo effectiveCommit effectiveDecommit
 where
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit

-- | Build the 'PartialFanout' ('FanoutProgress') head state from a closed head,
-- carrying over the snapshot/parameters and using the given chain state, remaining
-- and distributed UTxO and fanout 'mode'. Shared by the three @Closed →
-- FanoutProgress@ transitions in 'aggregateNodeState'.
closedToFanoutProgress ::
  ClosedState tx ->
  ChainStateType tx ->
  UTxOType tx ->
  UTxOType tx ->
  FanoutMode tx ->
  -- | The steps already observed, see 'FanoutStepLanded'
  [FanoutStepLanded tx] ->
  HeadState tx
closedToFanoutProgress closedState chainState remaining distributed mode stepsLanded =
  FanoutProgress
    PartialFanoutState
      { parameters
      , confirmedSnapshot
      , contestationDeadline
      , chainState
      , headId
      , headSeed
      , version
      , remainingOutputs = remaining
      , distributedOutputs = distributed
      , mode
      , stepsLanded
      , everLanded = not (null stepsLanded)
      }
 where
  ClosedState{parameters, confirmedSnapshot, contestationDeadline, headId, headSeed, version} = closedState

-- | Rebuild the 'ClosedState' from a 'PartialFanoutState' when reverting an
-- optimistic 'Closed' → 'PartialFanout' transition (see 'HeadFanoutReverted').
-- 'readyToFanoutSent' is restored to 'True' because a fanout is only reachable
-- after the head was announced 'ReadyToFanout'.
fanoutProgressToClosed :: PartialFanoutState tx -> ClosedState tx
fanoutProgressToClosed pfs =
  ClosedState
    { parameters
    , confirmedSnapshot
    , contestationDeadline
    , readyToFanoutSent = True
    , chainState
    , headId
    , headSeed
    , version
    }
 where
  PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version} = pfs

-- | The step this node is currently driving. 'Nothing' while a manual fanout
-- waits on the next selection, where nothing is pending.
currentFanoutStep :: IsTx tx => PartialFanoutState tx -> Maybe (NextFanoutStep tx)
currentFanoutStep PartialFanoutState{confirmedSnapshot, version, remainingOutputs, distributedOutputs, mode} =
  case mode of
    AwaitingSelection -> Nothing
    AutoDrain -> Just (step remainingOutputs)
    DistributingSelection selection -> Just (step selection)
 where
  step target = nextFanoutStep confirmedSnapshot version target remainingOutputs onChainDatum

  onChainDatum = onChainFanoutDatum distributedOutputs

-- | Whether a posted transaction is the one a given step posts, telling a failure
-- of the step this node is driving from one it has already moved past.
--
-- Compares the distributed set, not just the transaction shape, since two
-- selections can both be non-final steps. By content, because naming the same
-- outputs under other inputs is a valid way to ask for the same set.
matchesFanoutStep :: IsTx tx => NextFanoutStep tx -> PostChainTx tx -> Bool
matchesFanoutStep step postChainTx =
  case (step, postChainTx) of
    -- Only one full fanout is ever in flight: it covers the whole head.
    (FullFanoutStep, FanoutTx{}) -> True
    (FinalStep{stepDistribute}, FinalPartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    (PartialStep{stepDistribute}, PartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    _ -> False

removeDistributedOutputs :: IsTx tx => [TxOutType tx] -> UTxOType tx -> UTxOType tx
removeDistributedOutputs = flip (foldl' (flip removeOneOutputFromUTxO))

-- | Whether a UTxO has no outputs.
nullOutputs :: IsTx tx => UTxOType tx -> Bool
nullOutputs = null . outputsOfUTxO

-- | Whether the outputs of @sub@ are a sub-multiset (by content) of @sup@. This
-- mirrors how partial fanout tracks distributed UTxO by content rather than by
-- 'TxIn', so a user-provided selection is validated against what is actually
-- still in the head.
isSubMultisetOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
isSubMultisetOf sub sup =
  length (outputsOfUTxO (removeDistributedOutputs (outputsOfUTxO sub) sup))
    == length (outputsOfUTxO sup)
    - length (outputsOfUTxO sub)

-- | Whether two UTxO sets have the same outputs (by content, as a multiset).
-- The @a == b@ short-circuit avoids the O(n²) multiset comparison in the common
-- case where both arguments are the same tracked set (e.g. the auto-drain
-- @sameOutputs remaining remaining@ check on every observed chunk), which is
-- exactly the large-UTxO heads partial fanout targets.
sameOutputs :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
sameOutputs a b =
  a == b || (length (outputsOfUTxO a) == length (outputsOfUTxO b) && a `isSubMultisetOf` b)

-- | Which on-chain head datum the next fanout step will be posted against:
-- still @Closed@ (no partial fanout has landed yet) or already @FanoutProgress@.
-- A 'FinalPartialFanoutTx' (which burns the head tokens) is only valid once the
-- datum is 'DatumFanoutProgress'.
data OnChainFanoutDatum = DatumClosed | DatumFanoutProgress

-- | The on-chain datum implied by how much has been distributed so far: still
-- @Closed@ while nothing has landed, @FanoutProgress@ once some has.
onChainFanoutDatum :: IsTx tx => UTxOType tx -> OnChainFanoutDatum
onChainFanoutDatum distributed
  | nullOutputs distributed = DatumClosed
  | otherwise = DatumFanoutProgress

-- | The mode a recorded selection leaves the driver in. A selection covering the
-- whole remainder with nothing distributed yet is not distributed as a selection
-- at all: 'nextFanoutStep' routes it to a full fanout and the driver auto-drains
-- the rest, so that is the mode it has to be recorded under.
--
-- 'fanoutStepStateChange' never pairs the two - such a step is a
-- 'FullFanoutStep', recorded as 'HeadFanoutInitiated' - so this only normalises
-- a selection recorded before that routing existed and replayed here. Without it
-- the re-posts ('repostFanoutStep') would post the full fanout that
-- 'nextFanoutStep' decides on while the mode kept claiming a selection is being
-- distributed, which is what every 'HeadPartiallyFannedOut' then reports.
recordedSelectionMode ::
  IsTx tx =>
  -- | Distributed so far
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  -- | The recorded selection
  UTxOType tx ->
  FanoutMode tx
recordedSelectionMode distributed remaining selection
  | nullOutputs distributed && selection `sameOutputs` remaining = AutoDrain
  | otherwise = DistributingSelection selection

-- | Post the transaction a decided 'NextFanoutStep' calls for. The chain layer
-- chunks a full fanout, and sizes a partial one, dynamically.
--
-- Transactions only: any accompanying state change belongs to the caller that
-- owns the head state, via 'fanoutStepStateChange'. That keeps this safe to call
-- where effects can be applied but state changes cannot, as in the startup
-- re-post in 'Hydra.Node.runHydraNode'.
emitFanoutStep ::
  IsTx tx =>
  -- | The step to emit, from 'nextFanoutStep'.
  NextFanoutStep tx ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  Outcome tx
emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline =
  case step of
    FinalStep{stepDistribute} ->
      cause
        OnChainEffect
          { postChainTx =
              FinalPartialFanoutTx
                { utxoToDistribute = stepDistribute
                , headSeed
                , contestationDeadline
                }
          }
    FullFanoutStep ->
      cause OnChainEffect{postChainTx = mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline}
    PartialStep{stepDistribute, stepProof} ->
      cause
        OnChainEffect
          { postChainTx =
              PartialFanoutTx
                { utxoToDistribute = stepDistribute
                , utxoForProof = stepProof
                , headSeed
                , contestationDeadline
                }
          }

-- | Re-post the next fanout step after a chain rollback while in
-- 'FanoutProgress', so the fanout resumes instead of stalling with the
-- rolled-back transaction gone and nothing re-posted. This mirrors the
-- Increment/Decrement re-post ('repostErased'). The caller passes the progress
-- rewound to the steps still on chain ('rewindFanoutProgress'). Without the
-- rewind, automatic mode posted the step after the erased one, built against
-- a datum the chain no longer had, and manual mode posted nothing.
--
-- Which step that is comes from 'currentFanoutStep', the same derivation the
-- revert guard uses to tell this step's failure from a superseded one's.
repostFanoutStep :: IsTx tx => PartialFanoutState tx -> Outcome tx
repostFanoutStep pfs =
  case currentFanoutStep pfs of
    -- Manual mode paused on the user: nothing to re-post, wait for the next
    -- 'PartialFanout' command.
    Nothing -> noop
    -- Only the transaction is re-posted: this node is the driver already, and
    -- re-posting is not a new decision to record. So this emits no state
    -- changes, which is what lets 'Hydra.Node.runHydraNode' apply its effects
    -- alone at startup.
    Just step ->
      emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  PartialFanoutState{confirmedSnapshot, version, headSeed, contestationDeadline} = pfs

-- | Rewind the fanout's progress after a rollback to the given slot, to the
-- steps still on the chain we follow. The erased steps' outputs are back in
-- the head, and the driver's mode goes back to what those steps replaced. See
-- 'FanoutStepLanded'.
--
-- A step observed exactly at the rollback point is still on chain, so only
-- steps strictly after it are erased, as in 'markErased'.
rewindFanoutProgress :: IsTx tx => ChainSlot -> PartialFanoutState tx -> PartialFanoutState tx
rewindFanoutProgress rolledBackSlot pfs@PartialFanoutState{confirmedSnapshot, version, mode, stepsLanded, distributedOutputs}
  | null erased = pfs
  | otherwise =
      pfs
        { stepsLanded = kept
        , distributedOutputs = distributed
        , remainingOutputs = removeDistributedOutputs (outputsOfUTxO distributed) (fanoutUTxOFromSnapshot confirmedSnapshot version)
        , mode = rewoundMode
        }
 where
  (kept, erased) = partition (\FanoutStepLanded{landedAt} -> landedAt <= rolledBackSlot) stepsLanded

  -- Subtract the erased steps' outputs rather than sum the kept ones. A state
  -- persisted before steps were recorded has distributed outputs that no step
  -- accounts for, and those must stay distributed.
  distributed = removeDistributedOutputs (outputsOfUTxO (foldMap (\FanoutStepLanded{stepOutputs} -> stepOutputs) erased)) distributedOutputs

  -- The outputs of an erased step that one of this node's selections was
  -- distributing are its job again. A driver draining automatically covers
  -- them anyway, and an observer, whose steps replaced no selection, must not
  -- start driving.
  erasedSelected =
    foldMap (\FanoutStepLanded{stepOutputs} -> stepOutputs) [s | s@FanoutStepLanded{modeBefore = DistributingSelection{}} <- erased]

  rewoundMode = case mode of
    AutoDrain -> AutoDrain
    DistributingSelection selection
      | nullOutputs erasedSelected -> mode
      | otherwise -> DistributingSelection (erasedSelected <> selection)
    AwaitingSelection
      | nullOutputs erasedSelected -> AwaitingSelection
      | otherwise -> DistributingSelection erasedSelected

-- | Detect our view of the chain going out of sync and issue a 'NodeUnsynced'
-- event when this is the case.
handleOutOfSync ::
  IsChainState tx =>
  Environment ->
  -- | Current system time
  UTCTime ->
  -- | Latest Chain point observed
  ChainPointType tx ->
  -- | Latest Chain point time representation observed
  UTCTime ->
  SyncedStatus ->
  Outcome tx
handleOutOfSync Environment{unsyncedPeriod} now chainPoint chainTime syncStatus =
  -- Emit only on an actual sync-status transition, rather than on every tick, so
  -- clients are not flooded (see issue #2749). The continuous drift value is
  -- exposed as a metric ('hydra_chain_drift_seconds') instead.
  case (syncStatus, newSyncStatus) of
    (InSync, CatchingUp) -> newState NodeUnsynced{chainSlot, chainTime, drift}
    (CatchingUp, InSync) -> newState NodeSynced{chainSlot, chainTime, drift}
    _ -> noop
 where
  plus = flip addUTCTime
  chainSlot = chainPointSlot chainPoint

  threshold = unsyncedPeriodToNominalDiffTime unsyncedPeriod
  drift = now `diffUTCTime` chainTime

  -- We consider the node out of sync when:
  -- the last observed chainTime plus the delta allowed by the unsyncedPeriod (threshold)
  -- falls behind the current system time (now).
  -- NOTE: this is the same as drift > threshold
  nodeOutOfSync = chainTime `plus` threshold < now
  newSyncStatus = if nodeOutOfSync then CatchingUp else InSync

-- | The pending deposit that a local 'currentDepositTxId' still refers to, if any: the deposit must
--   be registered in 'pendingDeposits' and not 'Expired'.
--
--   Being registered and unexpired is what makes a recorded deposit id something the head may still
--   act on, and nothing else should be treated as a commit in flight. Neither of the two ways a
--   deposit stops being pending clears 'currentDepositTxId': 'DepositExpired' deliberately keeps the
--   deposit in the map so it can still be recovered, and 'DepositRecovered' only deletes the map
--   entry. So a caller that reads 'currentDepositTxId' on its own can end up waiting on a deposit
--   that is unclaimable, or already gone, and that wait never resolves.
existingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx, Deposit tx)
existingDeposit pendingDeposits currentDeposit =
  case currentDeposit of
    Nothing -> Nothing
    Just depositTxId ->
      case Map.lookup depositTxId pendingDeposits of
        Nothing -> Nothing
        Just deposit
          | deposit.status == Expired -> Nothing
          | otherwise -> Just (depositTxId, deposit)

-- | Validate whether a current deposit in the local state actually exists
--   in the map of pending deposits.
--
--   * If 'currentDeposit' is 'Nothing', returns 'Nothing'.
--   * If 'currentDeposit' is @'Just' txId@ and @txId@ is present in 'pendingDeposits'
--     and not 'Expired', returns the original 'currentDeposit'.
--   * Otherwise, returns 'Nothing'.
--
--   This is typically used to confirm that a local deposit that is to be
--   requested in 'ReqSn' is indeed still pending and has not been processed or
--   removed.
--
--   Expired deposits are dropped rather than carried: requesting one makes every
--   receiving party hard-error with 'RequestedDepositExpired', so a deposit that
--   somehow became unclaimable would stall snapshots for the whole head instead of
--   just being abandoned by its depositor.
setExistingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx)
setExistingDeposit pendingDeposits = fmap fst . existingDeposit pendingDeposits

-- | Find the oldest non-empty active deposit, if any. Deposits are selected
-- in FIFO order by their 'created' timestamp. This mirrors the selection
-- logic in 'withNextActive' used by 'onOpenChainTick'.
nextActiveDepositId :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx)
nextActiveDepositId deposits =
  case filter (\(_, Deposit{deposited, status}) -> deposited /= mempty && status == Active) (Map.toList deposits) of
    [] -> Nothing
    xs -> Just (fst (minimumBy (comparing ((.created) . snd)) xs))

-- | The commit the given confirmed snapshot still has pending on chain: the
-- deposited outputs and the deposit they came from. 'Nothing' once our version
-- moved past the snapshot's: the increment landed, so the commit is applied
-- rather than pending.
--
-- Read this instead of the snapshot's 'utxoToCommit'. A confirmed snapshot can
-- sit one version behind the chain, when another party's settlement lands
-- before our own signatures complete, and treating its applied commit as still
-- in flight would block every later deposit and decommit.
unsettledCommit :: SnapshotVersion -> Snapshot tx -> Maybe (UTxOType tx, TxIdType tx)
unsettledCommit version Snapshot{version = snapshotVersion, utxoToCommit, depositTxId}
  -- The opposite of the 'version > snapshotVersion' test that
  -- 'SnapshotRequested' and 'LocalStateCleared' use for "applied". Today this
  -- equals '==', since our version never trails the confirmed snapshot's. It
  -- is spelled this way so the two tests cannot drift apart, and so that if
  -- they ever did, a claim is carried once too often rather than dropped.
  | version <= snapshotVersion = (,) <$> utxoToCommit <*> depositTxId
  | otherwise = Nothing

-- | The decommit the given confirmed snapshot still has pending on chain, with
-- the same reasoning as 'unsettledCommit'.
unsettledDecommit :: SnapshotVersion -> Snapshot tx -> Maybe (UTxOType tx)
unsettledDecommit version Snapshot{version = snapshotVersion, utxoToDecommit}
  | version <= snapshotVersion = utxoToDecommit
  | otherwise = Nothing

-- | Keep an unsettled commit (see 'unsettledCommit') only while its deposit is
-- still tracked. That is what makes it a claim the increment in flight can
-- still settle. Once the deposit was recovered on L1 its outputs left the head
-- for good, and the claim must be dropped.
--
-- The deposit's local status is ignored on purpose. Our expiry margin is a
-- whole 'depositPeriod' ahead of the on-chain deadline, and the claim is
-- already signed, so the increment can land for a deposit we marked expired.
-- Every party's copy expires at the same time, so treating that claim as gone
-- would either drop it from the next snapshot, losing the deposited outputs at
-- close, or leave a round nobody signs.
stillClaimable :: IsTx tx => PendingDeposits tx -> Maybe (UTxOType tx, TxIdType tx) -> Maybe (UTxOType tx, TxIdType tx)
stillClaimable pendingDeposits = mfilter (\(_, depositTxId) -> Map.member depositTxId pendingDeposits)

-- | Pick the deposit to include in the next snapshot, given our version and
-- the confirmed snapshot the next one builds on.
--
-- The confirmed snapshot's unsettled commit is carried again first, whatever
-- the deposit's local status, as long as the deposit is still there to claim.
-- Dropping it would confirm a snapshot in which the deposited outputs count as
-- neither applied nor pending, and they would be lost at close. Our expiry
-- margin is a whole 'depositPeriod' ahead of the on-chain deadline, so
-- expired locally does not mean the increment cannot land.
--
-- Otherwise the deposit queued in 'currentDepositTxId' goes first, if it is
-- still pending. Then the oldest active deposit, but only when no decommit is
-- pending and the confirmed snapshot has no unsettled commit, so that we do
-- not post a second increment before 'CommitFinalized' removes the deposit.
--
-- The confirmed snapshot's own deposit is never a fresh claim: it is carried
-- again while unsettled, and done once its increment landed. It can still sit
-- in 'pendingDeposits' when a rollback erased that increment before the
-- snapshot confirmed locally, because the race branch of 'SnapshotConfirmed'
-- retains it only as the confirming outcome is applied, after the next
-- proposal was decided. Proposing it again would be refused by every party
-- ('ReqSnDepositBlockedByFinalizedCommit'), the leader's own echo included,
-- leaving the leader in a round nobody signs.
selectNextDeposit ::
  IsTx tx =>
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | Our version
  SnapshotVersion ->
  -- | The confirmed snapshot the next one builds on
  Snapshot tx ->
  Maybe (TxIdType tx)
selectNextDeposit pendingDeposits currentDepositTxId mDecommitTx version confirmed =
  claimToReCarry
    <|> setExistingDeposit freshCandidates currentDepositTxId
    <|> case (mDecommitTx, stillClaimable pendingDeposits mUnsettledCommit) of
      (Nothing, Nothing) -> nextActiveDepositId freshCandidates
      _ -> Nothing
 where
  mUnsettledCommit = unsettledCommit version confirmed

  -- Only while the deposit is still tracked: once it was recovered on L1 its
  -- outputs left the head for good, so the claim must be dropped rather than
  -- re-carried, and every receiving party would reject it anyway.
  claimToReCarry = snd <$> stillClaimable pendingDeposits mUnsettledCommit

  freshCandidates = maybe pendingDeposits (`Map.delete` pendingDeposits) confirmed.depositTxId

-- | Reject a decommit that materializes no output.
-- 'Hydra.Contract.Head.checkDecrement' requires at least one, so such a decommit
-- can never settle on-chain, and recording it would block every later snapshot
-- (which cannot carry a different one).
--
-- Belongs after the applicability check at every call site: a transaction that
-- does not apply is reported with the ledger's own, more precise reason.
requireDecommitOutputs ::
  IsTx tx =>
  HeadId ->
  UTxOType tx ->
  tx ->
  Outcome tx ->
  Outcome tx
requireDecommitOutputs headId localUTxO decommitTx continue
  | utxoFromTx decommitTx == mempty =
      newState
        DecommitInvalid
          { headId
          , decommitTx
          , decommitInvalidReason =
              ServerOutput.DecommitTxInvalid
                { localUTxO
                , validationError = ValidationError "decommit transaction has no outputs"
                }
          }
  | otherwise = continue

-- | The incremental action to put in the next 'ReqSn': a commit or a decommit,
-- never both. A snapshot carrying both cannot be closed, since close and fanout
-- express a single incremental action ('setIncrementalActionMaybe').
--
-- A commit wins: its deposit expires on-chain, while a decommit only waits. This
-- cannot starve the decommit, because 'selectNextDeposit' refuses to start a
-- *new* commit while a decommit is pending — only one already in flight can win,
-- and that one stops being selected once it settles and leaves 'pendingDeposits'.
selectNextIncrementalAction ::
  IsTx tx =>
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | Our version
  SnapshotVersion ->
  -- | The confirmed snapshot the next one builds on
  Snapshot tx ->
  (Maybe tx, Maybe (TxIdType tx))
selectNextIncrementalAction pendingDeposits currentDepositTxId mDecommitTx version confirmed =
  case selectNextDeposit pendingDeposits currentDepositTxId mDecommitTx version confirmed of
    Just depositTxId -> (Nothing, Just depositTxId)
    Nothing -> (mDecommitTx, Nothing)

-- ** Settlement retention and re-posting (#2741)

-- | The deposits claimed by retained increments. Such a deposit is only pending
-- again because a rollback erased its increment, and only re-posting that
-- increment settles it ('repostErased'). It must never be proposed for a
-- snapshot again, since the head already counts its funds, nor recovered,
-- which would corrupt the L2 ledger. See #2741.
retainedDeposits :: IsTx tx => Settlements tx -> Set (TxIdType tx)
retainedDeposits =
  Set.fromList . mapMaybe (\Settlement{snapshot} -> (getSnapshot snapshot).depositTxId) . Map.elems

-- | The deposits handlers of an open head may act on: scoped to the head and
-- excluding the deposits claimed by retained increments (see
-- 'retainedDeposits').
eligibleDeposits :: IsTx tx => OpenState tx -> PendingDeposits tx -> PendingDeposits tx
eligibleDeposits OpenState{headId, coordinatedHeadState} =
  (`Map.withoutKeys` retainedDeposits coordinatedHeadState.settlements) . depositsForHead headId

-- | The deposits blocked by the retained increments of an open head.
--
-- Empty for any other head state on purpose. An increment can only settle into
-- an open head, so once the head closes the retained snapshots can never claim
-- their deposits again. The way out for such a deposit is then to recover it
-- after its deadline and leave the deposited outputs out of the fanout with
-- 'PartialFanout', so 'Recover' must not stay blocked after close.
openRetainedDeposits :: IsTx tx => HeadState tx -> Set (TxIdType tx)
openRetainedDeposits = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{settlements}} -> retainedDeposits settlements
  _ -> mempty

-- | Retain the snapshot whose increment or decrement was seen bumping the
-- on-chain version to @newVersion@, if that is the locally confirmed snapshot
-- (based on the version just below, carrying a commit or decommit). Only that
-- snapshot can settle again if a rollback erases the settlement, and
-- 'confirmedSnapshot' may move past it. Its confirmed txs are blanked, see
-- 'Settlement'.
--
-- Nothing is retained when the observation came before the snapshot confirmed
-- locally, because another party collected the last signature and posted
-- first. Retention then happens when the snapshot confirms, see the
-- 'SnapshotConfirmed' branch of 'applyEvent'. See #2741.
retainSettlement ::
  ChainSlot ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Settlements tx ->
  Settlements tx
retainSettlement slot = retainSettlementWith (Landed slot)

-- | 'retainSettlement' with the status the entry gets: landed at the
-- observation slot, or the status noted for a version bump whose snapshot had
-- not confirmed yet (see 'UnretainedSettlements').
retainSettlementWith ::
  SettlementStatus ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Settlements tx ->
  Settlements tx
retainSettlementWith status newVersion confirmedSnapshot settlements =
  case confirmedSnapshot of
    ConfirmedSnapshot{snapshot = snapshot@Snapshot{version, utxoToCommit, utxoToDecommit}, signatures}
      | version + 1 == newVersion
      , isJust utxoToCommit || isJust utxoToDecommit ->
          -- Keep an entry already retained for this version. That entry is the
          -- snapshot whose settlement was actually observed, with its real slot
          -- and its erased mark. A later snapshot at the same version carries
          -- the same action but is not the one that settled, and this function
          -- is also called with our own version from the 'SnapshotConfirmed'
          -- race branch. Replacing the entry would re-stamp the slot and undo
          -- 'markErased'; 'nextErasedSettlement' would then find nothing,
          -- 'repostErased' would post nothing, and our version would stay above
          -- the chain's for good, which a close cannot express. A settlement
          -- that really landed again is re-stamped by 'landSettlement' instead.
          Map.insertWith
            (\_new old -> old)
            version
            Settlement{snapshot = ConfirmedSnapshot{snapshot = snapshot{confirmed = []}, signatures}, status}
            settlements
    _ -> settlements

-- | Record a settlement seen bumping the on-chain version to @newVersion@ at
-- @slot@: retain the confirmed snapshot if it is the one that settled
-- ('retainSettlement'), stamp the retained entry as landed ('landSettlement'),
-- and if nothing is retained at that version, note the bump for the snapshot
-- still to confirm ('recordUnretained').
recordSettlement :: ChainSlot -> SnapshotVersion -> CoordinatedHeadState tx -> CoordinatedHeadState tx
recordSettlement slot newVersion chs@CoordinatedHeadState{confirmedSnapshot, settlements, unretained} =
  chs{settlements = settlements', unretained = recordUnretained slot newVersion settlements' unretained}
 where
  settlements' = landSettlement slot newVersion (retainSettlement slot newVersion confirmedSnapshot settlements)

-- | Note a bump to @newVersion@ seen at @slot@ whose snapshot is not retained,
-- because it has not confirmed locally yet, see 'UnretainedSettlements'. Once
-- that version is retained the note is dropped.
recordUnretained :: ChainSlot -> SnapshotVersion -> Settlements tx -> UnretainedSettlements -> UnretainedSettlements
recordUnretained slot newVersion settlements unretained
  | newVersion == 0 = unretained
  | Map.member key settlements = Map.delete key unretained
  | otherwise = Map.insert key (Landed slot) unretained
 where
  key = newVersion - 1

-- | Record that the retained settlement bumping the on-chain version to
-- @newVersion@ landed, or landed again, at the given slot, so that a rollback
-- erasing it again still leads to a re-post.
landSettlement :: ChainSlot -> SnapshotVersion -> Settlements tx -> Settlements tx
landSettlement slot newVersion settlements
  | newVersion == 0 = settlements
  | otherwise = Map.adjust (\Settlement{snapshot} -> Settlement{snapshot, status = Landed slot}) (newVersion - 1) settlements

-- | Mark the retained settlements and notes that a rollback to the given slot
-- erased. The rollback point is the last block both chains share, so a
-- settlement observed exactly there is still on chain: strictly (<).
markErased :: ChainSlot -> CoordinatedHeadState tx -> CoordinatedHeadState tx
markErased rolledBackSlot chs@CoordinatedHeadState{settlements, unretained} =
  chs
    { settlements = Map.map (\Settlement{snapshot, status} -> Settlement{snapshot, status = erase status}) settlements
    , unretained = Map.map erase unretained
    }
 where
  erase = \case
    Landed{observedAtSlot} | rolledBackSlot < observedAtSlot -> Erased
    status -> status

-- | Drop the retained settlements and notes no rollback can reach anymore, see
-- 'retentionCutoff'. This bounds the map; without it an open head would keep
-- one snapshot per settlement forever.
--
-- Erased entries are never dropped: they still have to be re-posted, and the
-- lowest one holds back every later re-post ('nextErasedSettlement').
pruneSettlements ::
  -- | Rollback horizon
  ChainSlot ->
  -- | Current slot
  ChainSlot ->
  CoordinatedHeadState tx ->
  CoordinatedHeadState tx
pruneSettlements horizon slot chs@CoordinatedHeadState{settlements, unretained} =
  chs
    { settlements = Map.filter (\Settlement{status} -> reachable status) settlements
    , unretained = Map.filter reachable unretained
    }
 where
  reachable = \case
    Landed{observedAtSlot} -> observedAtSlot > retentionCutoff horizon slot
    Erased -> True

-- | Update the coordinated head state of an open head; any other head state is
-- left alone.
onCoordinatedHeadState :: (CoordinatedHeadState tx -> CoordinatedHeadState tx) -> HeadState tx -> HeadState tx
onCoordinatedHeadState f = \case
  Open os@OpenState{coordinatedHeadState} -> Open os{coordinatedHeadState = f coordinatedHeadState}
  other -> other

-- | The erased settlement with the lowest version, which is the one the chain
-- accepts next: the rollback took the on-chain version back to its base, and
-- each settlement bumps the version by one.
--
-- Only 'markErased' marks an entry erased, from an exact observation slot, so
-- an erased entry always means a settlement that must land again; the lowest
-- one holds back all later re-posts.
nextErasedSettlement :: Settlements tx -> Maybe (Settlement tx)
nextErasedSettlement = find (\Settlement{status} -> status == Erased) . Map.elems

-- | Post the increment or decrement of a signed snapshot.
postSettlement :: IsTx tx => HeadSeed -> HeadId -> HeadParameters -> ConfirmedSnapshot tx -> Outcome tx
postSettlement headSeed headId headParameters snapshot =
  case getSnapshot snapshot of
    Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId} ->
      cause OnChainEffect{postChainTx = IncrementTx{headSeed, headId, headParameters, incrementingSnapshot = snapshot, depositTxId}}
    Snapshot{utxoToDecommit = Just _} ->
      cause OnChainEffect{postChainTx = DecrementTx{headSeed, headId, headParameters, decrementingSnapshot = snapshot}}
    _ -> noop

-- | Re-post the in-flight settlement of the confirmed snapshot, given the
-- retained settlements as they are after a rollback, or after one landed
-- again. Its
-- increment or decrement was posted when the snapshot confirmed and never
-- observed, so it may have been in a rolled back block.
--
-- Nothing is posted while a retained settlement is erased. The chain only
-- accepts the lowest version, so the erased ones must land first, in order,
-- which the ticks do ('repostErased'). The in-flight one follows when the last
-- erased one is seen landing again ('repostAfterRelanding'). See #2741.
repostInFlightSettlement ::
  IsTx tx =>
  OpenState tx ->
  PendingDeposits tx ->
  Settlements tx ->
  Outcome tx
repostInFlightSettlement OpenState{headSeed, headId, parameters, coordinatedHeadState} pendingDeposits settlements
  | isJust (nextErasedSettlement settlements) = noop
  -- The confirmed snapshot is retained, so its settlement was observed and is
  -- on chain: nothing is in flight.
  | Map.member (getSnapshot confirmedSnapshot).version settlements = noop
  | otherwise = repostInFlight
 where
  CoordinatedHeadState{confirmedSnapshot, decommitTx} = coordinatedHeadState

  -- NOTE: the deposit comes from the confirmed snapshot itself, not from
  -- 'currentDepositTxId'. Only the deposit bound into the signed snapshot can be
  -- claimed on-chain, and 'DepositActivated' can set 'currentDepositTxId' to an
  -- unrelated deposit after that snapshot was confirmed. A deposit still
  -- pending means its increment did not settle yet.
  repostInFlight = case getSnapshot confirmedSnapshot of
    Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId}
      | Map.member depositTxId pendingDeposits ->
          postSettlement headSeed headId parameters confirmedSnapshot
    Snapshot{utxoToDecommit = Just _}
      | isJust decommitTx ->
          postSettlement headSeed headId parameters confirmedSnapshot
    _ -> noop

-- | Post, on a tick, the erased settlement due next, while it can land: a
-- decrement always can, an increment only once its deposit is back on the
-- chain we follow, since the rollback may have erased the deposit tx too.
--
-- Erased settlements are posted here and nowhere else. Every block brings a
-- tick after its observations, so this runs once per block until the
-- settlement is seen landing again, which marks it landed. A post that failed
-- for a passing reason is retried on the next block. A node restarted with an
-- erased entry resumes by itself. And the first block of the new fork gets to
-- bring the settlement back on its own before anything is posted, which a
-- fork built from the same mempool usually does. A duplicate of one already
-- landing again is refused by the chain, at no cost but a
-- 'PostTxOnChainFailed'. The next erased one follows one block after the
-- previous one landed. See #2741.
--
-- Nothing is posted while this node is catching up. Rolling forward through
-- history brings a tick per block replayed, as fast as the node can process
-- them, so posting here would submit the same transaction once per block the
-- node is behind. Those submissions are built against a chain view that is
-- behind as well, and the settlement may already have landed on the part of
-- the chain the node has not reached yet. The first tick after the node is in
-- sync posts it.
repostErased :: IsTx tx => SyncedStatus -> OpenState tx -> PendingDeposits tx -> Outcome tx
repostErased syncStatus OpenState{headSeed, headId, parameters, coordinatedHeadState = CoordinatedHeadState{settlements}} pendingDeposits =
  case (syncStatus, nextErasedSettlement settlements) of
    (InSync, Just Settlement{snapshot})
      | canLand (getSnapshot snapshot) -> postSettlement headSeed headId parameters snapshot
    _ -> noop
 where
  canLand Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId} = Map.member depositTxId pendingDeposits
  canLand _ = True

-- | After an increment or decrement was observed: if it landed again (its
-- 'newVersion' is not ahead of our 'version', which never goes down, so this
-- settlement was applied before and then erased by a rollback) and it was the
-- last erased one, post the in-flight settlement it was holding back, see
-- 'repostInFlightSettlement'.
repostAfterRelanding ::
  IsChainState tx =>
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  SnapshotVersion ->
  Outcome tx
repostAfterRelanding pendingDeposits openState newChainState newVersion
  | newVersion <= version =
      repostInFlightSettlement openState pendingDeposits (landSettlement (chainStateSlot newChainState) newVersion settlements)
  | otherwise = noop
 where
  OpenState{coordinatedHeadState = CoordinatedHeadState{version, settlements}} = openState

-- | Handles inputs and converts them into 'StateChanged' events along with
-- 'Effect's, in case it is processed successfully. Later, the Node will
-- apply the events via 'aggregateNodeState', resulting in a new 'NodeState'.
update ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current system time.
  UTCTime ->
  -- | Current NodeState to validate the command against.
  NodeState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
update env ledger now nodeState ev
  -- SECURITY: before anything else, in every head state and whether or not the
  -- node is in sync. An accumulator over more elements than the trusted setup
  -- supports has no commitment at all -- forcing it calls 'error', see
  -- 'Hydra.Tx.Accumulator.checkAccumulatorSize'. Almost every way this input
  -- can be turned away echoes it back to clients, and encoding that echo forces
  -- the accumulator: 'RejectedInputBecauseUnsynced' while catching up,
  -- 'CommandFailed' in a state that does not handle the command,
  -- 'SideLoadSnapshotRejected' from the open-state checks, 'UnhandledInput'.
  -- So an oversized snapshot has to be rejected here, ahead of all of them.
  --
  -- This is a backstop. The client API rejects such a snapshot before it is ever
  -- queued (see 'Hydra.API.ClientInput.validateClientInput'), which it must,
  -- since the node traces an input before the head logic sees it and tracing
  -- forces the accumulator too. That is also where a client gets a useful
  -- error. Reaching here means some new producer of 'SideLoadSnapshot' skipped
  -- that check, so this only has to be safe, not informative -- hence 'Error'
  -- via 'SideLoadSnapshotFailed', which carries the failure alone. Note this
  -- cannot go through 'sideLoadFailed': that emits a 'SideLoadSnapshotRejected'
  -- client message, which echoes the input.
  | ClientInput clientInput <- ev
  , Left (utxoCount, maxAllowed) <- validateClientInput clientInput =
      Error . SideLoadSnapshotFailed $ SideLoadUTxOSetTooLarge{utxoCount, maxAllowed}
  | otherwise =
      case nodeState of
        NodeCatchingUp{headState, chainPointTime} ->
          updateCatchingUpHead env ledger now chainPointTime nodeState.pendingDeposits headState ev (syncedStatus nodeState)
        NodeInSync{headState, chainPointTime} ->
          updateInSyncHead env ledger now chainPointTime nodeState.pendingDeposits headState ev (syncedStatus nodeState)

updateCatchingUpHead ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current system time.
  UTCTime ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  Outcome tx
updateCatchingUpHead env ledger now chainPointTime pendingDeposits st ev syncStatus =
  case ev of
    ChainInput{} ->
      handleChainInput env ledger now chainPointTime pendingDeposits st ev syncStatus
    ClientInput{clientInput} ->
      cause . ClientEffect $ ServerOutput.RejectedInputBecauseUnsynced clientInput drift
    NetworkInput{} ->
      wait WaitOnNodeInSync{currentSlot}
 where
  ChainPointTime{currentSlot, drift} = chainPointTime

updateInSyncHead ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current system time.
  UTCTime ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  Outcome tx
updateInSyncHead env ledger now chainPointTime pendingDeposits st ev syncStatus =
  case ev of
    ChainInput{} ->
      handleChainInput env ledger now chainPointTime pendingDeposits st ev syncStatus
    ClientInput{} ->
      handleClientInput env ledger chainPointTime pendingDeposits st ev
    NetworkInput{} ->
      handleNetworkInput env ledger chainPointTime pendingDeposits st ev

-- * Input Handlers

handleChainInput ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current system time.
  UTCTime ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  Outcome tx
handleChainInput env _ledger now _chainPointTime pendingDeposits st ev syncStatus = case (st, ev) of
  (Idle _, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState}) ->
    onIdleChainInitTx env newChainState headId headSeed headParameters participants
  -- Open
  ( Open openState@OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState, ChainInput Tick{chainTime, chainPoint}) ->
    -- XXX: We originally forgot the normal TickObserved state event here and so
    -- time did not advance in an open head anymore. This is a hint that we
    -- should compose event handling better.
    newState TickObserved{chainPoint, chainTime}
      <> handleOutOfSync env now chainPoint chainTime syncStatus
      <> onChainTick env pendingDeposits chainTime
      <> onOpenChainTick env chainTime (eligibleDeposits openState pendingDeposits) openState
      <> repostErased syncStatus openState (depositsForHead openState.headId pendingDeposits)
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId}, newChainState})
    | ourHeadId == headId ->
        onOpenChainIncrementTx env (depositsForHead headId pendingDeposits) openState newChainState newVersion depositTxId
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO}, newChainState})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx env (eligibleDeposits openState pendingDeposits) openState newChainState newVersion distributedUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed ClosedState{contestationDeadline, readyToFanoutSent, headId}, ChainInput Tick{chainTime, chainPoint})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        newState TickObserved{chainPoint, chainTime}
          <> handleOutOfSync env now chainPoint chainTime syncStatus
          <> onChainTick env pendingDeposits chainTime
          <> newState HeadIsReadyToFanout{headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onClosedChainFanoutTx closedState newChainState fanoutUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onClosedChainPartialFanoutTx closedState newChainState distributedOutputs
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainPartialFanoutTx partialFanoutState newChainState distributedOutputs
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainFanoutTx partialFanoutState newChainState fanoutUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Node-level: deposit/recover observations scoped to our head
  (Open OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        -- A retained deposit observed again means a rollback erased both the
        -- deposit and its increment, and the deposit tx just landed again on
        -- the new chain. The next tick re-posts the increment, see
        -- 'repostErased'.
        newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        Continue [] []
  (Closed ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        Continue [] []
  -- Mirror the 'Closed' case while mid-fanout: a deposit observed during a
  -- (partial) fanout must still be recorded so it remains recoverable via
  -- 'Recover'. Without this the input falls through to 'Error' and is dropped.
  (FanoutProgress PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        Continue [] []
  (Idle _, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    Continue [] []
  -- Deposit recovery is node-level: emit DepositRecovered for any tracked deposit
  -- regardless of which head is currently active. Previous-head deposits survive
  -- fanout in 'pendingDeposits', so recovery works even while a new head is Open.
  -- Unrelated deposits (never in pendingDeposits) are silently ignored.
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState})
    | Map.member recoveredTxId pendingDeposits ->
        newState DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
    | otherwise ->
        Continue [] []
  -- Open + Rollback: re-post the in-flight settlement, which may have been in
  -- a rolled back block. The erased ones are posted by the ticks that follow
  -- ('repostErased'). See #2741.
  (Open openState@OpenState{headId, coordinatedHeadState}, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
      <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
      <> repostInFlightSettlement openState (depositsForHead headId pendingDeposits) (markErased (chainStateSlot rolledBackChainState) coordinatedHeadState).settlements
  -- FanoutProgress + Rollback: re-post the next fanout step so the fanout
  -- resumes rather than stalling (the in-flight fanout tx may have been rolled
  -- back). Mirrors the Open re-post above.
  (FanoutProgress partialFanoutState, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
      <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
      <> repostFanoutStep (rewindFanoutProgress (chainStateSlot rolledBackChainState) partialFanoutState)
  -- General
  (_, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
      <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
  (_, ChainInput Tick{chainTime, chainPoint}) ->
    newState TickObserved{chainPoint, chainTime}
      <> handleOutOfSync env now chainPoint chainTime syncStatus
      <> onChainTick env pendingDeposits chainTime
  (_, ChainInput PostTxError{postTxError = StalePartialFanoutTx}) ->
    -- The chain advanced past this step before we could post it (another node
    -- was faster). The chain observation loop already emitted the correct next
    -- step, so this is safe to ignore.
    noop
  (FanoutProgress pfs, ChainInput PostTxError{postChainTx, postTxError})
    -- We optimistically moved 'Closed' → 'PartialFanout' when the fanout was
    -- initiated. If posting the initiating fanout tx fails terminally before
    -- anything has been distributed on chain (so the on-chain datum is still
    -- 'Closed'), revert to 'Closed' rather than wedging the head — otherwise
    -- 'Fanout' stays rejected and there is no clean way to recover. Once any
    -- partial fanout has landed ('distributedOutputs' non-empty) the on-chain
    -- datum is genuinely 'FanoutProgress', so we must not revert.
    --
    -- Only for a failure of the step this node is actually driving, of which
    -- 'AwaitingSelection' has none. A superseded transaction can still be in
    -- flight — a selection covering the whole remainder turns into a full fanout
    -- and leaves the earlier chunk behind — and reverting on its failure would
    -- drop the driver role for a transaction nobody is waiting on.
    --
    -- Nor once any step ever landed ('everLanded'). A fork can erase every
    -- landed step and rewind the progress to nothing distributed. The re-post
    -- issued then fails when the new fork already brought the erased step
    -- back. That is a race which observing the step land again resolves, not
    -- a failed start; reverting would turn this node into a passive observer
    -- of a fanout it was driving.
    | PartialFanoutState{headId, distributedOutputs, everLanded} <- pfs
    , nullOutputs distributedOutputs
    , not everLanded
    , maybe False (`matchesFanoutStep` postChainTx) (currentFanoutStep pfs) ->
        newState HeadFanoutReverted{headId}
          <> cause (ClientEffect ServerOutput.PostTxOnChainFailed{postChainTx, postTxError})
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    cause . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  _ ->
    Error $ UnhandledInput ev st

handleNetworkInput ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
handleNetworkInput env ledger ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  -- Open
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env ledger currentSlot openState ttl (eligibleDeposits openState pendingDeposits) tx
  -- NOTE: 'ReqSn' gets the unfiltered (per-head) deposits: it distinguishes a
  -- requested deposit that is blocked by a finalized increment (hard error)
  -- from one that is simply not known locally (wait).
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    onOpenNetworkReqSn env ledger (depositsForHead ourHeadId pendingDeposits) currentSlot openState ttl sender sv sn txIds decommitTx depositTxId
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    onOpenNetworkAckSn env (eligibleDeposits openState pendingDeposits) openState sender snapshotSignature sn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ledger ttl currentSlot (eligibleDeposits openState pendingDeposits) openState transaction
  _ ->
    Error $ UnhandledInput ev st

onConnectionEvent :: Text -> Network.Connectivity -> Outcome tx
onConnectionEvent misconfiguredPeers = \case
  Network.NetworkConnected ->
    newState NetworkConnected
  Network.NetworkDisconnected ->
    newState NetworkDisconnected
  Network.VersionMismatch{ourVersion, theirVersion} ->
    newState NetworkVersionMismatch{ourVersion, theirVersion}
  Network.ClusterIDMismatch{clusterPeers} ->
    newState NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers}
  Network.PeerConnected{peer} ->
    newState PeerConnected{peer}
  Network.PeerDisconnected{peer} ->
    newState PeerDisconnected{peer}

handleClientInput ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
handleClientInput env ledger ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
  (Idle _, ClientInput Init) ->
    onIdleClientInit env
  -- Open
  (Open openState, ClientInput Close) ->
    onOpenClientClose openState
  (Open openState, ClientInput SafeClose) ->
    onOpenClientClose openState
  (Open{}, ClientInput (NewTx tx)) ->
    onOpenClientNewTx tx
  (Open openState@OpenState{headId = ourHeadId}, ClientInput (SideLoadSnapshot confirmedSnapshot)) ->
    let Snapshot{headId = otherHeadId} = getSnapshot confirmedSnapshot
     in if ourHeadId == otherHeadId
          then onOpenClientSideLoadSnapshot openState confirmedSnapshot
          else Error NotOurHead{ourHeadId, otherHeadId}
  (Open OpenState{headId, coordinatedHeadState}, ClientInput Decommit{decommitTx}) -> do
    onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx
  -- Closed
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, ClientInput PartialFanout{utxoToFanout}) ->
    onClosedClientPartialFanout closedState utxoToFanout
  -- PartialFanout: once a partial fanout has started, only further
  -- 'PartialFanout' commands are accepted (a plain 'Fanout' falls through to the
  -- general 'CommandFailed' below).
  (FanoutProgress partialFanoutState, ClientInput PartialFanout{utxoToFanout}) ->
    onPartialFanoutClientPartialFanout partialFanoutState utxoToFanout
  -- Node-level
  (_, ClientInput Recover{recoverTxId}) -> do
    onClientRecover currentSlot pendingDeposits (openRetainedDeposits st) recoverTxId
  -- General
  (_, ClientInput{clientInput}) ->
    cause . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    Error $ UnhandledInput ev st

-- * NodeState aggregate

-- | Reflect 'StateChanged' events onto the 'NodeState' aggregateNodeState.
-- Events carrying a 'HeadId' that does not match the current state are silently
-- ignored, preventing cross-head state contamination during event replay.
-- Events without a 'HeadId' are always applied.
aggregateNodeState ::
  IsChainState tx =>
  -- | Rollback horizon of the network, see 'Hydra.Node.Environment.rollbackHorizon'
  ChainSlot ->
  NodeState tx ->
  StateChanged tx ->
  NodeState tx
aggregateNodeState rollbackHorizon nodeState sc =
  case (headIdOf (headState nodeState), eventHeadId sc) of
    (Just sid, Just eid) | sid /= eid -> nodeState
    _ ->
      let st = applyEvent (headState nodeState) sc
          chainPointTimeState = chainPointTime nodeState
       in case sc of
            HeadOpened{chainState} ->
              nodeState
                { headState = st
                , chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}
                }
            DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
              recordDeposit rollbackHorizon (chainStateSlot chainState) depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} $
                nodeState{headState = st}
            DepositActivated{depositTxId, deposit} ->
              updateDeposit depositTxId deposit $
                nodeState{headState = st}
            DepositExpired{depositTxId, deposit} ->
              -- NB: We keep expired deposits since we actually need them when Recovering.
              -- There is a corresponding error RequestedDepositExpired which gives users context on stale ReqSn.
              updateDeposit depositTxId deposit $
                nodeState{headState = st}
            DepositRecovered{chainState, depositTxId} ->
              consumeDeposit rollbackHorizon (chainStateSlot chainState) depositTxId $
                case st of
                  Open os@OpenState{coordinatedHeadState} ->
                    nodeState
                      { headState =
                          Open
                            os
                              { coordinatedHeadState =
                                  coordinatedHeadState
                                    { currentDepositTxId =
                                        if coordinatedHeadState.currentDepositTxId == Just depositTxId
                                          then Nothing
                                          else coordinatedHeadState.currentDepositTxId
                                    }
                              }
                      }
                  _ ->
                    nodeState{headState = st}
            CommitFinalized{chainState, newVersion, depositTxId} ->
              consumeDeposit rollbackHorizon (chainStateSlot chainState) depositTxId $ case st of
                Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{localUTxO, confirmedSnapshot, seenSnapshot}}
                  -- Re-observation: the increment re-landed after a rollback
                  -- (the local 'version' never rolls back, so a 'newVersion'
                  -- not ahead of it means this finalization was applied
                  -- before). Only convergence bookkeeping: in particular
                  -- 'localUTxO' must not absorb the deposit again (its outputs
                  -- may have been spent on L2 in the meantime and the union
                  -- would resurrect them) and an unrelated deposit already
                  -- parked in 'currentDepositTxId' for the next snapshot must
                  -- be left alone. See #2741.
                  | newVersion <= chs.version ->
                      nodeState
                        { headState =
                            Open
                              os
                                { chainState
                                , coordinatedHeadState =
                                    (recordSettlement (chainStateSlot chainState) newVersion chs)
                                      { currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId
                                      }
                                }
                        }
                  | otherwise ->
                      nodeState
                        { headState =
                            Open
                              os
                                { chainState
                                , coordinatedHeadState =
                                    (recordSettlement (chainStateSlot chainState) newVersion chs)
                                      { version = newVersion
                                      , -- NOTE: This must correspond to the just finalized
                                        -- depositTxId, but we should not verify this here.
                                        currentDepositTxId = Nothing
                                      , localUTxO = localUTxO <> maybe mempty (.deposited) (Map.lookup depositTxId nodeState.pendingDeposits)
                                      , -- A snapshot already in 'SeenSnapshot' still completes now
                                        -- that our version moved past its own: every party can
                                        -- sign it, those that saw the bump before the ReqSn one
                                        -- version behind their own (see 'waitOnSnapshotVersion').
                                        -- Keep it so it confirms and 'maybeRequestNextSnapshot'
                                        -- requests the next one with the bumped version; reset
                                        -- only when nothing is in flight. (A local 'SeenSnapshot'
                                        -- only proves this party echoed the ReqSn, not that every
                                        -- party did.)
                                        seenSnapshot = case seenSnapshot of
                                          SeenSnapshot{} -> seenSnapshot
                                          _ -> LastSeenSnapshot{lastSeen = (getSnapshot confirmedSnapshot).number}
                                      }
                                }
                        }
                _ ->
                  nodeState{headState = st}
            TickObserved{chainPoint, chainTime} ->
              -- Retained settlements no rollback can reach anymore are dropped.
              nodeState{headState = onCoordinatedHeadState (pruneSettlements rollbackHorizon (chainPointSlot chainPoint)) st, chainPointTime = chainPointTimeState{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
            ChainRolledBack{chainState} ->
              -- Deposits are derived from L1: restore the view at the rolled
              -- back slot. A deposit whose increment or recover was erased is
              -- pending again, a deposit whose deposit tx was erased is gone,
              -- and observing the new chain brings the view back in line. See
              -- #2741. Retained settlements observed after the rolled back slot
              -- are no longer on chain: mark them, so the ticks re-post them in
              -- order, see 'repostErased'.
              rollbackDeposits (chainStateSlot chainState) $
                nodeState{headState = onCoordinatedHeadState (markErased (chainStateSlot chainState)) st, chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}}
            NodeUnsynced{chainSlot, chainTime, drift} ->
              NodeCatchingUp{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
            NodeSynced{chainSlot, chainTime, drift} ->
              NodeInSync{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
            -- Restore the full snapshot: a checkpoint carries the aggregated
            -- 'pendingDeposits' and 'chainPointTime' (and synced constructor),
            -- which the default arm below would otherwise drop on replay.
            Checkpoint checkpointedNodeState ->
              checkpointedNodeState
            _ ->
              nodeState{headState = st}

-- * HeadState aggregate helpers

-- | Extract the 'HeadId' from a 'StateChanged' event, if the event carries one.
-- Events that do not carry a 'HeadId' always pass through 'aggregateNodeState' unchanged.
eventHeadId :: StateChanged tx -> Maybe HeadId
eventHeadId = \case
  HeadOpened{headId} -> Just headId
  TransactionAppliedToLocalUTxO{headId} -> Just headId
  SnapshotConfirmed{headId} -> Just headId
  LocalStateCleared{headId} -> Just headId
  DepositRecorded{headId} -> Just headId
  DepositRecovered{} -> Nothing
  CommitApproved{headId} -> Just headId
  CommitFinalized{headId} -> Just headId
  DecommitRecorded{headId} -> Just headId
  DecommitApproved{headId} -> Just headId
  DecommitInvalid{headId} -> Just headId
  DecommitFinalized{headId} -> Just headId
  HeadIsReadyToFanout{headId} -> Just headId
  HeadClosed{headId} -> Just headId
  HeadContested{headId} -> Just headId
  HeadFannedOut{headId} -> Just headId
  TxInvalid{headId} -> Just headId
  HeadPartialFannedOut{headId} -> Just headId
  HeadFanoutInitiated{headId} -> Just headId
  HeadPartialFanoutSelected{headId} -> Just headId
  HeadFanoutReverted{headId} -> Just headId
  -- The headId in IgnoredHeadInitializing is the OTHER head's id (not ours),
  -- so it must not be used to filter against the current head state.
  IgnoredHeadInitializing{} -> Nothing
  TransactionReceived{} -> Nothing
  SnapshotRequestDecided{} -> Nothing
  SnapshotRequested{} -> Nothing
  PartySignedSnapshot{} -> Nothing
  DepositActivated{} -> Nothing
  DepositExpired{} -> Nothing
  ChainRolledBack{} -> Nothing
  TickObserved{} -> Nothing
  NetworkDisconnected -> Nothing
  NetworkConnected -> Nothing
  PeerConnected{} -> Nothing
  PeerDisconnected{} -> Nothing
  NetworkVersionMismatch{} -> Nothing
  NetworkClusterIDMismatch{} -> Nothing
  Checkpoint{} -> Nothing
  NodeUnsynced{} -> Nothing
  NodeSynced{} -> Nothing

-- | Extract the 'HeadId' from the current 'HeadState', if any.
headIdOf :: HeadState tx -> Maybe HeadId
headIdOf = \case
  Idle _ -> Nothing
  Open OpenState{headId} -> Just headId
  Closed ClosedState{headId} -> Just headId
  FanoutProgress PartialFanoutState{headId} -> Just headId

applyEvent :: IsChainState tx => HeadState tx -> StateChanged tx -> HeadState tx
applyEvent st = \case
  NetworkConnected -> st
  NetworkDisconnected -> st
  NetworkVersionMismatch{} -> st
  NetworkClusterIDMismatch{} -> st
  PeerConnected{} -> st
  PeerDisconnected{} -> st
  HeadOpened{headSeed, headId, parameters, chainState} ->
    Open
      OpenState
        { headId
        , headSeed
        , parameters
        , coordinatedHeadState =
            CoordinatedHeadState
              { localUTxO = mempty
              , allTxs = mempty
              , localTxs = mempty
              , confirmedSnapshot = InitialSnapshot{headId}
              , seenSnapshot = NoSeenSnapshot
              , currentDepositTxId = Nothing
              , decommitTx = Nothing
              , version = 0
              , settlements = mempty
              , unretained = mempty
              }
        , chainState
        }
  TransactionReceived{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { allTxs = Map.insert (txId tx) tx allTxs
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> st
  TransactionAppliedToLocalUTxO{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { localUTxO =
                      -- NOTE: Safe to use localUTxO here because the tx was
                      -- ledger-validated before this event was emitted.
                      -- 'aggregate' folds events in order, so 'localUTxO'
                      -- here always reflects all previously applied transactions.
                      applyTxTo tx localUTxO
                  , -- NOTE: Order of transactions is important here. See also
                    -- 'pruneTransactions'.
                    localTxs = localTxs Seq.|> tx
                  }
            }
       where
        CoordinatedHeadState{localUTxO, localTxs} = coordinatedHeadState
      _otherState -> st
  SnapshotRequestDecided{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot =
                      RequestedSnapshot
                        { lastSeen = seenSnapshotNumber seenSnapshot
                        , requested = snapshotNumber
                        }
                  }
            }
       where
        CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
      _otherState -> st
  SnapshotRequested{requestedSnapshot = snapshot, newLocalTxs, newCurrentDepositTxId} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot = mkSeenSnapshot snapshot mempty
                  , localTxs = newLocalTxs
                  , -- NOTE: pure UTxO arithmetic. 'newLocalTxs' was pre-pruned
                    -- by 'pruneTransactions' in 'onOpenNetworkReqSn' (so each tx
                    -- is guaranteed to apply), making 'applyTxTo' safe to use
                    -- without ledger validation.
                    --
                    -- A pending commit ('utxoToCommit') is only spendable once its
                    -- on-chain increment has landed (chain 'version' ahead of the
                    -- snapshot's). Before then it must NOT be part of the spendable
                    -- localUTxO, otherwise the same deposit UTxO could be spent once
                    -- per snapshot round (it is re-injected here) and inflate the L2
                    -- balance. Mirrors 'confirmedUTxO'; the deposit enters localUTxO
                    -- at 'CommitFinalized'.
                    localUTxO =
                      let activeUTxO =
                            if version > snapshot.version
                              then snapshot.utxo <> fromMaybe mempty snapshot.utxoToCommit
                              else snapshot.utxo
                       in foldl' (flip applyTxTo) activeUTxO newLocalTxs
                  , allTxs = foldr (Map.delete . txId) allTxs snapshot.confirmed
                  , currentDepositTxId = newCurrentDepositTxId
                  }
            }
       where
        CoordinatedHeadState{allTxs, version} = coordinatedHeadState
      _otherState -> st
  PartySignedSnapshot{party, signature} ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState =
            chs@CoordinatedHeadState
              { seenSnapshot = ss@SeenSnapshot{signatories}
              }
          } ->
          Open
            os
              { coordinatedHeadState =
                  chs
                    { seenSnapshot =
                        ss
                          { signatories = Map.insert party signature signatories
                          }
                    }
              }
      _otherState -> st
  SnapshotConfirmed{snapshot = mSnapshot, signatures} ->
    case st of
      Open os@OpenState{chainState, coordinatedHeadState = chs@CoordinatedHeadState{seenSnapshot, version}} ->
        case mSnapshot <|> snapshotFromSeen seenSnapshot of
          Just snapshot ->
            -- The snapshot's settling transaction (increment/decrement) may
            -- have been observed on chain *before* this local confirmation
            -- (another party collected the last AckSn and posted first) —
            -- recognizable by 'version' already being one past the snapshot's.
            -- Nothing could be retained at observation time (see
            -- 'retainSettlement'), so retain now, with the status the bump was
            -- noted with ('UnretainedSettlements'). A rollback in between marked
            -- that note erased. Stamping the current chain slot instead would
            -- call the erased settlement landed, since the chain state was
            -- rewound too, and it would never be re-posted.
            let confirmed = ConfirmedSnapshot{snapshot, signatures}
                status = fromMaybe (Landed (chainStateSlot chainState)) (Map.lookup snapshot.version chs.unretained)
             in Open
                  os
                    { coordinatedHeadState =
                        chs
                          { confirmedSnapshot = confirmed
                          , seenSnapshot = LastSeenSnapshot snapshot.number
                          , settlements = retainSettlementWith status version confirmed chs.settlements
                          , -- Snapshots confirm in order, so every note at or below
                            -- this version is used up or dead. Dropping them keeps
                            -- the notes bounded.
                            unretained = Map.filterWithKey (\k _ -> k > snapshot.version) chs.unretained
                          }
                    }
          Nothing -> Hydra.Prelude.error "applyEvent: SnapshotConfirmed but no snapshot in event or seenSnapshot"
      _otherState -> st
   where
    snapshotFromSeen :: SeenSnapshot tx -> Maybe (Snapshot tx)
    snapshotFromSeen (SeenSnapshot sn _ _) = Just sn
    snapshotFromSeen _ = Nothing
  LocalStateCleared{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState = coordinatedHeadState@CoordinatedHeadState{confirmedSnapshot, version = currentVersion}} ->
        Open
          os
            { coordinatedHeadState =
                case confirmedSnapshot of
                  InitialSnapshot{} ->
                    coordinatedHeadState
                      { localUTxO = mempty
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = NoSeenSnapshot
                      }
                  ConfirmedSnapshot{snapshot = Snapshot{utxo, utxoToCommit, version = snapshotVersion}} ->
                    coordinatedHeadState
                      { -- NOTE: Include utxoToCommit in localUTxO when the corresponding
                        -- increment has been finalized on-chain (i.e. the chain-observed
                        -- version has advanced past the snapshot's version). Without this,
                        -- a side-loaded deposit snapshot would leave the head unable to
                        -- spend the deposited UTxO.
                        localUTxO =
                          if currentVersion > snapshotVersion
                            then utxo <> fromMaybe mempty utxoToCommit
                            else utxo
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = LastSeenSnapshot snapshotNumber
                      , decommitTx = Nothing
                      , currentDepositTxId = Nothing
                      }
            }
      _otherState -> st
  DepositRecorded{} -> st
  DepositActivated{depositTxId, deposit} -> case st of
    Open os@OpenState{headId = ourHeadId, coordinatedHeadState = chs}
      | deposit.headId == ourHeadId ->
          -- Spec: txω = ⊥ ∨ txα = ⊥ — deposit and decommit are mutually exclusive.
          -- Only queue the deposit when no decommit is pending; otherwise the tick
          -- will pick it up once the decommit completes.
          case chs.decommitTx of
            Just _ -> st
            Nothing
              -- The finalized deposit can be re-activated when a rollback rewinds
              -- the deposit view to before its activation: never park it in
              -- 'currentDepositTxId' — it is settled by re-posting the increment,
              -- not by a new snapshot, see 'retainedDeposits' and #2741.
              | depositTxId `Set.member` retainedDeposits chs.settlements -> st
              | otherwise -> Open os{coordinatedHeadState = chs{currentDepositTxId = chs.currentDepositTxId <|> Just depositTxId}}
    _ -> st
  DepositExpired{} -> st
  CommitApproved{} -> st
  DepositRecovered{} -> st
  CommitFinalized{} -> st
  DecommitRecorded{decommitTx} -> case st of
    Open
      os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { -- Apply the decommit to localUTxO and remove its outputs:
                    -- decommit's outputs leave the head, so net effect is
                    -- removing the spent inputs from localUTxO.
                    localUTxO = applyTxTo decommitTx localUTxO `withoutUTxO` utxoFromTx decommitTx
                  , decommitTx = Just decommitTx
                  }
            }
       where
        CoordinatedHeadState{localUTxO} = coordinatedHeadState
    _otherState -> st
  DecommitApproved{} -> st
  DecommitInvalid{} -> st
  DecommitFinalized{chainState, newVersion} ->
    case st of
      Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}}
        -- Re-observation: the decrement re-landed after a rollback (the local
        -- 'version' never rolls back, so a 'newVersion' not ahead of it means
        -- this finalization was applied before). Only convergence bookkeeping:
        -- in particular an unrelated newer decommit may already be in flight
        -- and must be left alone. See #2741.
        | newVersion <= chs.version ->
            Open
              os
                { chainState
                , coordinatedHeadState =
                    recordSettlement (chainStateSlot chainState) newVersion chs
                }
        | otherwise ->
            Open
              os
                { chainState
                , coordinatedHeadState =
                    (recordSettlement (chainStateSlot chainState) newVersion chs)
                      { decommitTx = Nothing
                      , version = newVersion
                      , -- A snapshot already in 'SeenSnapshot' still completes now
                        -- that our version moved past its own: every party can
                        -- sign it, those that saw the bump before the ReqSn one
                        -- version behind their own (see 'waitOnSnapshotVersion').
                        -- Keep it so it confirms and 'maybeRequestNextSnapshot'
                        -- requests the next one with the bumped version; reset
                        -- only when nothing is in flight. (A local 'SeenSnapshot'
                        -- only proves this party echoed the ReqSn, not that every
                        -- party did.)
                        seenSnapshot = case seenSnapshot of
                          SeenSnapshot{} -> seenSnapshot
                          _ -> LastSeenSnapshot{lastSeen = (getSnapshot confirmedSnapshot).number}
                      }
                }
      _otherState -> st
  HeadClosed{chainState, contestationDeadline} ->
    case st of
      Open
        OpenState
          { parameters
          , coordinatedHeadState =
            CoordinatedHeadState
              { confirmedSnapshot
              , version
              }
          , headId
          , headSeed
          } ->
          Closed
            ClosedState
              { parameters
              , confirmedSnapshot
              , contestationDeadline
              , readyToFanoutSent = False
              , chainState
              , headId
              , headSeed
              , version
              }
      _otherState -> st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version} ->
        Closed
          ClosedState
            { parameters
            , confirmedSnapshot
            , contestationDeadline
            , readyToFanoutSent
            , chainState
            , headId
            , headSeed
            , version
            }
      _otherState -> st
  HeadFannedOut{chainState} ->
    case st of
      Closed _ ->
        Idle $ IdleState{chainState}
      FanoutProgress _ ->
        Idle $ IdleState{chainState}
      _otherState -> st
  HeadFanoutInitiated{remainingOutputs} ->
    case st of
      -- This node initiated a full automatic fanout: become the driver in
      -- 'AutoDrain' mode so its observations auto-continue to completion.
      Closed cst@ClosedState{chainState} ->
        closedToFanoutProgress cst chainState remainingOutputs mempty AutoDrain []
      -- A target covering the whole remainder before anything landed is a full
      -- fanout too ('nextFanoutStep'), so the driver switches to draining
      -- automatically.
      FanoutProgress pfs -> FanoutProgress pfs{mode = AutoDrain, remainingOutputs}
      _otherState -> st
  HeadPartialFanoutSelected{remainingOutputs, selection} ->
    case st of
      -- First selective partial fanout from a freshly closed head: enter the
      -- 'PartialFanout' state with nothing distributed yet.
      Closed cst@ClosedState{chainState} ->
        closedToFanoutProgress cst chainState remainingOutputs mempty (recordedSelectionMode mempty remainingOutputs selection) []
      -- Continuing: just record the new active selection.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs} ->
        FanoutProgress pfs{mode = recordedSelectionMode distributedOutputs remainingOutputs selection}
      _otherState -> st
  HeadFanoutReverted{} ->
    case st of
      -- Roll the optimistic transition back: the initiating fanout tx failed to
      -- post and nothing landed on chain, so the head is really still 'Closed'.
      FanoutProgress pfs -> Closed (fanoutProgressToClosed pfs)
      _otherState -> st
  HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
    case st of
      -- First partial fanout observed by a passive observer: transition from
      -- 'Closed' into 'PartialFanout' (using the observed chain state).
      -- The observer had no mode to replace: waiting is what its erased step
      -- rewinds to.
      Closed cst ->
        closedToFanoutProgress cst chainState remainingOutputs newlyDistributed mode [landed AwaitingSelection]
      -- Subsequent steps: accumulate distributed outputs, update remaining/mode
      -- and record the step with the mode it replaces.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs = priorDistributed, mode = priorMode, stepsLanded} ->
        FanoutProgress
          pfs
            { chainState
            , remainingOutputs
            , distributedOutputs = priorDistributed <> newlyDistributed
            , mode
            , stepsLanded = stepsLanded <> [landed priorMode]
            , everLanded = True
            }
      _otherState -> st
   where
    landed modeBefore = FanoutStepLanded{landedAt = chainStateSlot chainState, stepOutputs = newlyDistributed, modeBefore}
  HeadIsReadyToFanout{} ->
    case st of
      Closed cst -> Closed cst{readyToFanoutSent = True}
      _otherState -> st
  ChainRolledBack{chainState} ->
    case st of
      -- The fanout's progress is chain-derived: rewind it to the steps still
      -- on the chain this node follows, see 'rewindFanoutProgress'.
      FanoutProgress pfs -> FanoutProgress (rewindFanoutProgress (chainStateSlot chainState) pfs){chainState}
      _otherState -> setChainState chainState st
  TickObserved{} -> st
  IgnoredHeadInitializing{} -> st
  TxInvalid{transaction} -> case st of
    Open ost@OpenState{coordinatedHeadState = coordState@CoordinatedHeadState{allTxs = allTransactions}} ->
      Open ost{coordinatedHeadState = coordState{allTxs = Map.delete (txId transaction) allTransactions}}
    _otherState -> st
  Checkpoint nodeState -> headState nodeState
  NodeSynced{} -> st
  NodeUnsynced{} -> st

aggregateState ::
  IsChainState tx =>
  -- | Rollback horizon, see 'aggregateNodeState'
  ChainSlot ->
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState rollbackHorizon s outcome =
  foldl' (aggregateNodeState rollbackHorizon) s $ collectStateChanged outcome
 where
  collectStateChanged :: Outcome tx -> [StateChanged tx]
  collectStateChanged = \case
    Error{} -> []
    Wait{stateChanges} -> stateChanges
    Continue{stateChanges} -> stateChanges

aggregateChainStateHistory :: IsChainState tx => ChainStateHistory tx -> StateChanged tx -> ChainStateHistory tx
aggregateChainStateHistory history = \case
  NetworkConnected -> history
  NetworkDisconnected -> history
  NetworkVersionMismatch{} -> history
  NetworkClusterIDMismatch{} -> history
  PeerConnected{} -> history
  PeerDisconnected{} -> history
  HeadOpened{chainState} -> pushNewState chainState history
  TransactionAppliedToLocalUTxO{} -> history
  SnapshotRequestDecided{} -> history
  SnapshotRequested{} -> history
  TransactionReceived{} -> history
  PartySignedSnapshot{} -> history
  SnapshotConfirmed{} -> history
  DepositRecorded{chainState} -> pushNewState chainState history
  DepositActivated{} -> history
  DepositExpired{} -> history
  DepositRecovered{chainState} -> pushNewState chainState history
  CommitFinalized{chainState} -> pushNewState chainState history
  DecommitRecorded{} -> history
  DecommitFinalized{chainState} -> pushNewState chainState history
  HeadClosed{chainState} -> pushNewState chainState history
  HeadContested{chainState} -> pushNewState chainState history
  HeadIsReadyToFanout{} -> history
  HeadFannedOut{chainState} -> pushNewState chainState history
  HeadPartialFannedOut{chainState} -> pushNewState chainState history
  HeadFanoutInitiated{} -> history
  HeadPartialFanoutSelected{} -> history
  HeadFanoutReverted{} -> history
  ChainRolledBack{chainState} -> rollbackHistory (chainStateSlot chainState) history
  TickObserved{chainPoint} -> setLastKnown chainPoint history
  CommitApproved{} -> history
  DecommitApproved{} -> history
  DecommitInvalid{} -> history
  IgnoredHeadInitializing{} -> history
  TxInvalid{} -> history
  LocalStateCleared{} -> history
  -- FIXME: This makes chain sync starting after rollbacks past the chain state impossible
  Checkpoint nodeState -> initHistory $ getChainState nodeState.headState
  NodeUnsynced{} -> history
  NodeSynced{} -> history
