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
  module Hydra.HeadLogic.Settlement,
  module Hydra.HeadLogic.Snapshot.Request,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Outcome,
) where

import Hydra.Prelude

import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..), validateClientInput)
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
import Hydra.HeadLogic.Settlement
import Hydra.HeadLogic.Snapshot.Request
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
  SettlementConflict,
  SettlementStatus (..),
  Settlements,
  UnretainedSettlements,
  adjustSettlementStatus,
  deleteUnretained,
  emptySettlements,
  firstErasedSettlement,
  getChainState,
  insertUnretained,
  isCollectingAcks,
  mapSettlementStatuses,
  mapUnretainedStatuses,
  mkSeenSnapshot,
  pruneRetainedSettlements,
  pruneUnretained,
  seenSnapshotNumber,
  setChainState,
  settlementDeposits,
  settlementEntries,
  snapshotInFlight,
  tryInsertSettlement,
  unretainedAfter,
  unretainedEntries,
 )
import Hydra.Ledger (Ledger (..), ValidationError (..), applyTransactions)
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, SyncedStatus (..), consumeDeposit, depositsForHead, recordDeposit, rollbackDeposits, syncedStatus, updateDeposit)
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
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (
  MultiSignature,
  Signature,
  Verified (..),
  aggregateInOrder,
  sign,
  verify,
  verifyMultiSignature,
  verifyMultiSignatureBytes,
 )
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

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
      Just Settlement{snapshot} -> (getSnapshot snapshot).depositTxId == confDepositTxId
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
        requireSignatureForThisSnapshot snapshot $ do
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

  -- Verify before recording the signature so a bad acknowledgement cannot
  -- prevent the same party's valid acknowledgement from being collected later.
  requireSignatureForThisSnapshot snapshot continue =
    if verify (vkey otherParty) snapshotSignature snapshot
      then continue
      else Error $ RequireFailed $ AckSnSignatureInvalid{requestedSn = sn, receivedSignature = otherParty}

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
-- A peer that withheld its 'AckSn' can close with the snapshot we are still
-- collecting signatures for. The close carries its full multisignature, so
-- adopt it as confirmed; otherwise we could not fan out the closed head.
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
  -- | Multisignature carried by the close redeemer.
  MultiSignature (Snapshot tx) ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline observedSignatures =
  adoptClosedSnapshot
    <> newState HeadClosed{headId, snapshotNumber = closedSnapshotNumber, chainState = newChainState, contestationDeadline}
    & maybePostContest
 where
  -- The multisignature verifying over our own signable bytes proves the head
  -- closed with exactly the snapshot we saw. Anything else leaves the confirmed
  -- snapshot as it is.
  adoptClosedSnapshot =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber}, signableBytes}
        | seenSnNumber == closedSnapshotNumber
        , Verified <- verifyMultiSignatureBytes vkeys observedSignatures signableBytes ->
            newState SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
      _ -> noop

  HeadParameters{parties} = headParameters

  vkeys = vkey <$> parties

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

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, version} = coordinatedHeadState

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
-- A peer that withheld its 'AckSn' can close with an older snapshot and then
-- contest with the one we are still collecting signatures for. The contest
-- carries its full multisignature, so adopt it as confirmed, like
-- 'onOpenChainCloseTx' does for a close.
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
  -- | Multisignature carried by the contest redeemer.
  MultiSignature (Snapshot tx) ->
  Outcome tx
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline observedSignatures =
  if
    | -- The multisignature verifying over our own signable bytes proves the
      -- head was contested with exactly the snapshot we saw.
      Just snapshot@Snapshot{number = unconfirmedSn} <- unconfirmedSnapshot
    , unconfirmedSn == snapshotNumber
    , Verified <- verifyMultiSignature vkeys observedSignatures snapshot ->
        newState SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
          <> newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
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
  ClosedState{parameters = headParameters, confirmedSnapshot, headId, version, unconfirmedSnapshot} = closedState

  HeadParameters{parties} = headParameters

  vkeys = vkey <$> parties

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
-- after the head was announced 'ReadyToFanout'. The contestation period is over
-- by then, so no contest can confirm an 'unconfirmedSnapshot' anymore.
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
    , unconfirmedSnapshot = Nothing
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
  , Left AccumulatorTooLarge{utxoCount, maxAllowed} <- validateClientInput clientInput =
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
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline, signatures}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline signatures
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
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId, snapshotNumber, signatures}, newChainState})
    | ourHeadId == headId ->
        onOpenChainIncrementTx env (depositsForHead headId pendingDeposits) openState newChainState newVersion depositTxId snapshotNumber signatures
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO, snapshotNumber, signatures}, newChainState})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx env (depositsForHead headId pendingDeposits) openState newChainState newVersion distributedUTxO snapshotNumber signatures
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline, signatures}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline signatures
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
handleNetworkInput env ledger chainPointTime pendingDeposits st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  -- Open
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env ledger chainPointTime openState ttl (eligibleDeposits openState pendingDeposits) tx
  -- NOTE: 'ReqSn' gets the unfiltered (per-head) deposits: it distinguishes a
  -- requested deposit that is blocked by a finalized increment (hard error)
  -- from one that is simply not known locally (wait).
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    onOpenNetworkReqSn env ledger (depositsForHead ourHeadId pendingDeposits) chainPointTime openState ttl sender sv sn txIds decommitTx depositTxId
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    onOpenNetworkAckSn env chainPointTime (eligibleDeposits openState pendingDeposits) openState sender snapshotSignature sn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ledger ttl chainPointTime (eligibleDeposits openState pendingDeposits) openState transaction
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
  Network.BroadcastStalled{pendingBroadcasts, stallReason} ->
    newState NetworkBroadcastStalled{pendingBroadcasts, stallReason}
  Network.BroadcastResumed ->
    newState NetworkBroadcastResumed

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
  Either (SettlementConflict tx) (NodeState tx)
aggregateNodeState rollbackHorizon nodeState sc =
  case (headIdOf (headState nodeState), eventHeadId sc) of
    (Just sid, Just eid) | sid /= eid -> pure nodeState
    _ -> do
      st <- applyEvent (headState nodeState) sc
      let chainPointTimeState = chainPointTime nodeState
      case sc of
        HeadOpened{chainState} ->
          pure $
            nodeState
              { headState = st
              , chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}
              }
        DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
          pure $
            recordDeposit rollbackHorizon (chainStateSlot chainState) depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} $
              nodeState{headState = st}
        DepositActivated{depositTxId, deposit} ->
          pure $
            updateDeposit depositTxId deposit $
              nodeState{headState = st}
        DepositExpired{depositTxId, deposit} ->
          pure $
            -- Keep expired deposits so they can still be recovered.
            updateDeposit depositTxId deposit $
              nodeState{headState = st}
        DepositRecovered{chainState, depositTxId} ->
          pure $
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
          fmap (consumeDeposit rollbackHorizon (chainStateSlot chainState) depositTxId) $ case st of
            Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{localUTxO, confirmedSnapshot, seenSnapshot}}
              -- Only convergence bookkeeping: in particular 'localUTxO'
              -- must not absorb the deposit again (its outputs may have
              -- been spent on L2 in the meantime and the union would
              -- resurrect them) and an unrelated deposit already parked in
              -- 'currentDepositTxId' for the next snapshot must be left
              -- alone. See #2741.
              | isReobservation newVersion chs -> do
                  recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesIncrement chs
                  pure $
                    nodeState
                      { headState =
                          Open
                            os
                              { chainState
                              , coordinatedHeadState =
                                  recorded
                                    { currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId
                                    }
                              }
                      }
              | otherwise -> do
                  recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesIncrement chs
                  pure $
                    nodeState
                      { headState =
                          Open
                            os
                              { chainState
                              , coordinatedHeadState =
                                  recorded
                                    { version = newVersion
                                    , -- NOTE: This must correspond to the just finalized
                                      -- depositTxId, but we should not verify this here.
                                      currentDepositTxId = Nothing
                                    , localUTxO = localUTxO <> maybe mempty (.deposited) (Map.lookup depositTxId nodeState.pendingDeposits)
                                    , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                                    }
                              }
                      }
            _ ->
              pure $ nodeState{headState = st}
         where
          matchesIncrement snapshot = snapshot.depositTxId == Just depositTxId && isJust snapshot.utxoToCommit
        TickObserved{chainPoint, chainTime} ->
          pure $
            -- Retained settlements no rollback can reach anymore are dropped.
            nodeState{headState = onCoordinatedHeadState (pruneSettlements rollbackHorizon (chainPointSlot chainPoint)) st, chainPointTime = chainPointTimeState{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
        ChainRolledBack{chainState} ->
          pure $
            -- Deposits are L1-derived: restore the view at the rolled-back
            -- slot. Deposits whose consuming tx (increment/recover) was
            -- erased resurface, deposits whose deposit tx was erased vanish;
            -- forward re-observation converges the view again. See #2741.
            -- Retained settlements observed after the rolled back slot are
            -- no longer on chain: mark them so they get re-posted in order,
            -- see 'repostErased'.
            rollbackDeposits (chainStateSlot chainState) $
              nodeState{headState = onCoordinatedHeadState (markErased (chainStateSlot chainState)) st, chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}}
        NodeUnsynced{chainSlot, chainTime, drift} ->
          pure $
            NodeCatchingUp{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
        NodeSynced{chainSlot, chainTime, drift} ->
          pure $
            NodeInSync{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
        -- Restore the full snapshot: a checkpoint carries the aggregated
        -- 'pendingDeposits' and 'chainPointTime' (and synced constructor),
        -- which the default arm below would otherwise drop on replay.
        Checkpoint checkpointedNodeState ->
          pure checkpointedNodeState
        _ ->
          pure $
            nodeState{headState = st}

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
  NetworkBroadcastStalled{} -> Nothing
  NetworkBroadcastResumed -> Nothing
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

applyEvent :: IsChainState tx => HeadState tx -> StateChanged tx -> Either (SettlementConflict tx) (HeadState tx)
applyEvent st = \case
  NetworkConnected -> pure st
  NetworkDisconnected -> pure st
  NetworkVersionMismatch{} -> pure st
  NetworkClusterIDMismatch{} -> pure st
  NetworkBroadcastStalled{} -> pure st
  NetworkBroadcastResumed -> pure st
  PeerConnected{} -> pure st
  PeerDisconnected{} -> pure st
  HeadOpened{headSeed, headId, parameters, chainState} ->
    pure $
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
                , settlements = emptySettlements
                , unretained = mempty
                }
          , chainState
          }
  TransactionReceived{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState
                    { allTxs = Map.insert (txId tx) tx allTxs
                    }
              }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> pure st
  TransactionAppliedToLocalUTxO{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
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
      _otherState -> pure st
  SnapshotRequestDecided{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
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
      _otherState -> pure st
  SnapshotRequested{requestedSnapshot = snapshot, newLocalTxs, newCurrentDepositTxId} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState
                    { seenSnapshot = mkSeenSnapshot snapshot mempty
                    , localTxs = newLocalTxs
                    , localUTxO =
                        case (decommitTx, snapshot.utxoToDecommit) of
                          -- A commit can defer the recorded decommit to a later
                          -- snapshot. Reserve its inputs after reapplying local
                          -- transactions, which may produce those inputs.
                          (Just tx, Nothing) -> applyTxTo tx rebuiltUTxO `withoutUTxO` utxoFromTx tx
                          _ -> rebuiltUTxO
                    , allTxs = foldr (Map.delete . txId) allTxs snapshot.confirmed
                    , currentDepositTxId = newCurrentDepositTxId
                    }
              }
       where
        -- 'pruneTransactions' validates these transactions before the event.
        rebuiltUTxO = foldl' (flip applyTxTo) (settledUTxO version snapshot) newLocalTxs
        CoordinatedHeadState{allTxs, version, decommitTx} = coordinatedHeadState
      _otherState -> pure st
  PartySignedSnapshot{party, signature} ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState =
            chs@CoordinatedHeadState
              { seenSnapshot = ss@SeenSnapshot{signatories}
              }
          } ->
          pure $
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
      _otherState -> pure st
  SnapshotConfirmed{snapshot = mSnapshot, signatures} ->
    case st of
      Open os@OpenState{chainState, coordinatedHeadState = chs@CoordinatedHeadState{seenSnapshot, version}} ->
        case mSnapshot <|> snapshotFromSeen seenSnapshot of
          Just snapshot -> do
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
                status = fromMaybe (Landed (chainStateSlot chainState)) (Map.lookup snapshot.version chs.unretained.unretainedEntries)
            settlements <-
              retainSettlementWith
                status
                version
                confirmed
                chs.settlements
            pure $
              Open
                os
                  { coordinatedHeadState =
                      chs
                        { confirmedSnapshot = confirmed
                        , seenSnapshot = LastSeenSnapshot snapshot.number
                        , settlements
                        , -- Confirmation consumes its note before dropping older notes.
                          unretained = unretainedAfter snapshot.version chs.unretained
                        }
                  }
          -- Nothing to confirm: leave the state as it is rather than crashing
          -- the node, which would also happen on every replay of the event.
          Nothing -> pure st
      -- Only emitted, with the snapshot, for the 'unconfirmedSnapshot' adopted
      -- from the multisignature of an observed contest. Anything else leaves
      -- the closed head as it is, as it did before this case existed.
      Closed cst ->
        case mSnapshot of
          Just snapshot ->
            pure $
              Closed
                cst
                  { confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures}
                  , unconfirmedSnapshot = Nothing
                  }
          Nothing -> pure st
      _otherState -> pure st
   where
    snapshotFromSeen :: SeenSnapshot tx -> Maybe (Snapshot tx)
    snapshotFromSeen (SeenSnapshot sn _ _) = Just sn
    snapshotFromSeen _ = Nothing
  LocalStateCleared{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState = coordinatedHeadState@CoordinatedHeadState{confirmedSnapshot, version = currentVersion}} ->
        pure $
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
                    ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToDecommit}} ->
                      coordinatedHeadState
                        { localUTxO = settledUTxO currentVersion snapshot
                        , localTxs = mempty
                        , allTxs = mempty
                        , seenSnapshot = LastSeenSnapshot snapshotNumber
                        , -- A decommit the snapshot still settles is kept: every
                          -- request at this version must carry it (see
                          -- 'onOpenNetworkReqSn'), and a side-load cannot change
                          -- the pending decommit, so the recorded tx is the one
                          -- producing it. Any other is pruned like the pending txs.
                          decommitTx = do
                            tx <- coordinatedHeadState.decommitTx
                            guard (currentVersion == snapshot.version && Just (utxoFromTx tx) == utxoToDecommit)
                            pure tx
                        , currentDepositTxId = Nothing
                        }
              }
      _otherState -> pure st
  DepositRecorded{} -> pure st
  DepositActivated{depositTxId, deposit} -> case st of
    Open os@OpenState{headId = ourHeadId, coordinatedHeadState = chs}
      | deposit.headId == ourHeadId ->
          -- Spec: txω = ⊥ ∨ txα = ⊥ — deposit and decommit are mutually exclusive.
          -- Only queue the deposit when no decommit is pending; otherwise the tick
          -- will pick it up once the decommit completes.
          case chs.decommitTx of
            Just _ -> pure st
            Nothing
              -- The finalized deposit can be re-activated when a rollback rewinds
              -- the deposit view to before its activation: never park it in
              -- 'currentDepositTxId' — it is settled by re-posting the increment,
              -- not by a new snapshot, see 'retainedDeposits' and #2741.
              | depositTxId `Set.member` retainedDeposits chs.settlements -> pure st
              -- Also do not park a deposit the next snapshot request cannot
              -- include, see 'absorbable'.
              | not (absorbable (nextRequestUTxO chs) (toList (Seq.take maxTxsPerSnapshot chs.localTxs)) deposit.deposited) -> pure st
              | otherwise -> pure $ Open os{coordinatedHeadState = chs{currentDepositTxId = chs.currentDepositTxId <|> Just depositTxId}}
    _ -> pure st
  DepositExpired{} -> pure st
  CommitApproved{} -> pure st
  DepositRecovered{} -> pure st
  CommitFinalized{} -> pure st
  DecommitRecorded{decommitTx} -> case st of
    Open
      os@OpenState{coordinatedHeadState} ->
        pure $
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
    _otherState -> pure st
  DecommitApproved{} -> pure st
  DecommitInvalid{} -> pure st
  DecommitFinalized{chainState, newVersion, distributedUTxO} ->
    case st of
      Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}}
        -- Only convergence bookkeeping: in particular an unrelated newer
        -- decommit may already be in flight and must be left alone. See #2741.
        | isReobservation newVersion chs -> do
            recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesDecrement chs
            pure $
              Open
                os
                  { chainState
                  , coordinatedHeadState =
                      recorded
                  }
        | otherwise -> do
            recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesDecrement chs
            pure $
              Open
                os
                  { chainState
                  , coordinatedHeadState =
                      recorded
                        { decommitTx = Nothing
                        , version = newVersion
                        , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                        }
                  }
      _otherState -> pure st
   where
    -- The chain observer relabels outputs, so compare their content and counts.
    matchesDecrement snapshot = maybe False (`sameOutputs` distributedUTxO) snapshot.utxoToDecommit
  HeadClosed{chainState, contestationDeadline} ->
    case st of
      Open
        OpenState
          { parameters
          , coordinatedHeadState =
            CoordinatedHeadState
              { confirmedSnapshot
              , seenSnapshot
              , version
              }
          , headId
          , headSeed
          } ->
          pure $
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
                , -- Keep the snapshot still collecting AckSns: a later contest
                  -- may carry its full multisignature.
                  unconfirmedSnapshot = case seenSnapshot of
                    SeenSnapshot{snapshot} -> Just snapshot
                    _ -> Nothing
                }
      _otherState -> pure st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version, unconfirmedSnapshot} ->
        pure $
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
              , unconfirmedSnapshot
              }
      _otherState -> pure st
  HeadFannedOut{chainState} ->
    case st of
      Closed _ ->
        pure $ Idle $ IdleState{chainState}
      FanoutProgress _ ->
        pure $ Idle $ IdleState{chainState}
      _otherState -> pure st
  HeadFanoutInitiated{remainingOutputs} ->
    case st of
      -- This node initiated a full automatic fanout: become the driver in
      -- 'AutoDrain' mode so its observations auto-continue to completion.
      Closed cst@ClosedState{chainState} ->
        pure $ closedToFanoutProgress cst chainState remainingOutputs mempty AutoDrain []
      -- A target covering the whole remainder before anything landed is a full
      -- fanout too ('nextFanoutStep'), so the driver switches to draining
      -- automatically.
      FanoutProgress pfs -> pure $ FanoutProgress pfs{mode = AutoDrain, remainingOutputs}
      _otherState -> pure st
  HeadPartialFanoutSelected{remainingOutputs, selection} ->
    case st of
      -- First selective partial fanout from a freshly closed head: enter the
      -- 'PartialFanout' state with nothing distributed yet.
      Closed cst@ClosedState{chainState} ->
        pure $ closedToFanoutProgress cst chainState remainingOutputs mempty (recordedSelectionMode mempty remainingOutputs selection) []
      -- Continuing: just record the new active selection.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs} ->
        pure $ FanoutProgress pfs{mode = recordedSelectionMode distributedOutputs remainingOutputs selection}
      _otherState -> pure st
  HeadFanoutReverted{} ->
    case st of
      -- Roll the optimistic transition back: the initiating fanout tx failed to
      -- post and nothing landed on chain, so the head is really still 'Closed'.
      FanoutProgress pfs -> pure $ Closed (fanoutProgressToClosed pfs)
      _otherState -> pure st
  HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
    case st of
      -- First partial fanout observed by a passive observer: transition from
      -- 'Closed' into 'PartialFanout' (using the observed chain state).
      -- The observer had no mode to replace: waiting is what its erased step
      -- rewinds to.
      Closed cst ->
        pure $ closedToFanoutProgress cst chainState remainingOutputs newlyDistributed mode [landed AwaitingSelection]
      -- Subsequent steps: accumulate distributed outputs, update remaining/mode
      -- and record the step with the mode it replaces.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs = priorDistributed, mode = priorMode, stepsLanded} ->
        pure $
          FanoutProgress
            pfs
              { chainState
              , remainingOutputs
              , distributedOutputs = priorDistributed <> newlyDistributed
              , mode
              , stepsLanded = stepsLanded <> [landed priorMode]
              , everLanded = True
              }
      _otherState -> pure st
   where
    landed modeBefore = FanoutStepLanded{landedAt = chainStateSlot chainState, stepOutputs = newlyDistributed, modeBefore}
  HeadIsReadyToFanout{} ->
    case st of
      Closed cst -> pure $ Closed cst{readyToFanoutSent = True}
      _otherState -> pure st
  ChainRolledBack{chainState} ->
    case st of
      -- The fanout's progress is chain-derived: rewind it to the steps still
      -- on the chain this node follows, see 'rewindFanoutProgress'.
      FanoutProgress pfs -> pure $ FanoutProgress (rewindFanoutProgress (chainStateSlot chainState) pfs){chainState}
      _otherState -> pure $ setChainState chainState st
  TickObserved{} -> pure st
  IgnoredHeadInitializing{} -> pure st
  TxInvalid{transaction} -> case st of
    Open ost@OpenState{coordinatedHeadState = coordState@CoordinatedHeadState{allTxs = allTransactions}} ->
      pure $ Open ost{coordinatedHeadState = coordState{allTxs = Map.delete (txId transaction) allTransactions}}
    _otherState -> pure st
  Checkpoint nodeState -> pure $ headState nodeState
  NodeSynced{} -> pure st
  NodeUnsynced{} -> pure st

aggregateState ::
  IsChainState tx =>
  -- | Rollback horizon, see 'aggregateNodeState'
  ChainSlot ->
  NodeState tx ->
  Outcome tx ->
  Either (SettlementConflict tx) (NodeState tx)
aggregateState rollbackHorizon s outcome =
  foldlM (aggregateNodeState rollbackHorizon) s $ collectStateChanged outcome
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
  NetworkBroadcastStalled{} -> history
  NetworkBroadcastResumed -> history
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
