{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Implements the Head Protocol's /state machine/ as /pure functions/ in an event sourced manner.
--
-- More specifically, the 'update' will handle 'Input's (or rather "commands" in
-- event sourcing speak) and convert that into a list of side-'Effect's and
-- 'StateChanged' events, which in turn are 'aggregate'd into a single
-- 'HeadState'.
--
-- As the specification is using a more imperative way of specifying the protocol
-- behavior, one would find the decision logic in 'update' while state updates
-- can be found in the corresponding 'aggregate' branch.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Outcome,
) where

import Hydra.Prelude

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  ChainEvent (..),
  ChainStateHistory,
  OnChainTx (..),
  PostChainTx (..),
  initHistory,
  pushNewState,
  rollbackHistory,
 )
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
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
  Committed,
  CoordinatedHeadState (..),
  Deposit (..),
  DepositStatus (..),
  HeadState (..),
  IdleState (IdleState, chainState),
  InitialState (..),
  NodeState (..),
  OpenState (..),
  PendingCommits,
  PendingDeposits,
  SeenSnapshot (..),
  getChainState,
  seenSnapshotNumber,
  setChainState,
 )
import Hydra.Ledger (
  Ledger (..),
  applyTransactions,
 )
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Tx (
  HeadId,
  HeadSeed,
  IsTx (..),
  TxIdType,
  UTxOType,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Tx.Crypto (
  Signature,
  Verified (..),
  aggregateInOrder,
  sign,
  verifyMultiSignature,
 )
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

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

-- * The Coordinated Head protocol

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

-- | Observe an init transaction, initialize parameters in an 'InitialState' and
-- notify clients that they can now commit.
--
-- __Transition__: 'IdleState' → 'InitialState'
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
      && Set.fromList configuredParticipants == Set.fromList participants =
      newState
        HeadInitialized
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

  HeadParameters{parties, contestationPeriod} = headParameters

  Environment
    { party
    , otherParties
    , contestationPeriod = configuredContestationPeriod
    , participants = configuredParticipants
    } = env

-- | Observe a commit transaction and record the committed UTxO in the state.
-- Also, if this is the last commit to be observed, post a collect-com
-- transaction on-chain.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialChainCommitTx ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | Committing party
  Party ->
  -- | Committed UTxO
  UTxOType tx ->
  Outcome tx
onInitialChainCommitTx st newChainState pt utxo =
  newState CommittedUTxO{headId, party = pt, committedUTxO = utxo, chainState = newChainState}
    <> causes [postCollectCom | canCollectCom]
 where
  postCollectCom =
    OnChainEffect
      { postChainTx =
          CollectComTx
            { utxo = fold newCommitted
            , headId
            , headParameters = parameters
            }
      }

  canCollectCom = null remainingParties

  remainingParties = Set.delete pt pendingCommits

  newCommitted = Map.insert pt utxo committed

  InitialState{pendingCommits, committed, headId, parameters} = st

-- | Client request to abort the head. This leads to an abort transaction on
-- chain, reimbursing already committed UTxOs.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientAbort ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  Outcome tx
onInitialClientAbort st =
  cause OnChainEffect{postChainTx = AbortTx{utxo = fold committed, headSeed}}
 where
  InitialState{committed, headSeed} = st

-- | Observe an abort transaction by switching the state and notifying clients
-- about it.
--
-- __Transition__: 'InitialState' → 'IdleState'
onInitialChainAbortTx ::
  Monoid (UTxOType tx) =>
  -- | New chain state
  ChainStateType tx ->
  Committed tx ->
  HeadId ->
  Outcome tx
onInitialChainAbortTx newChainState committed headId =
  newState HeadAborted{headId, utxo = fold committed, chainState = newChainState}

-- | Observe a collectCom transaction. We initialize the 'OpenState' using the
-- head parameters from 'IdleState' and construct an 'InitialSnapshot' holding
-- @u0@ from the committed UTxOs.
--
-- __Transition__: 'InitialState' → 'OpenState'
onInitialChainCollectTx ::
  IsChainState tx =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onInitialChainCollectTx st newChainState =
  -- Spec: 𝑈₀  ← ⋃ⁿⱼ₌₁ 𝑈ⱼ
  let u0 = fold committed
   in -- Spec: L̂  ← 𝑈₀
      --       ̅S  ← snObj(0, 0, 𝑈₀, ∅, ∅)
      --       v , ŝ ← 0
      --       T̂  ← ∅
      --       txω ← ⊥
      --       𝑈𝛼 ← ∅
      newState HeadOpened{headId, chainState = newChainState, initialUTxO = u0}
 where
  -- TODO: Do we want to check whether this even matches our local state? For
  -- example, we do expect `null remainingParties` but what happens if it's
  -- untrue?
  InitialState{committed, headId} = st

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
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger currentSlot st ttl tx =
  -- Keep track of transactions by-id
  (newState TransactionReceived{tx} <>) $
    -- Spec: wait L̂ ◦ tx ≠ ⊥
    waitApplyTx $ \newLocalUTxO ->
      -- Spec: T̂ ← T̂ ⋃ {tx}
      --       L̂  ← L̂ ◦ tx
      newState TransactionAppliedToLocalUTxO{headId, tx, newLocalUTxO}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        & maybeRequestSnapshot (confirmedSn + 1)
 where
  waitApplyTx cont =
    case applyTransactions currentSlot localUTxO [tx] of
      Right utxo' -> cont utxo'
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
    if not snapshotInFlight && isLeader parameters party nextSn
      then
        outcome
          -- XXX: This state update has no equivalence in the
          -- spec. Do we really need to store that we have
          -- requested a snapshot? If yes, should update spec.
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> localTxs') decommitTx currentDepositTxId)
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

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  -- NOTE: Order of transactions is important here. See also
  -- 'pruneTransactions'.
  localTxs' = localTxs <> [tx]

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
onOpenNetworkReqSn env ledger pendingDeposits currentSlot st otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
  -- Spec: require v = v̂ ∧ s = ŝ + 1 ∧ leader(s) = j
  requireReqSn $
    -- Spec: wait ŝ = ̅S.s
    waitNoSnapshotInFlight $
      -- TODO: is this really needed?
      -- Spec: wait v = v̂
      waitOnSnapshotVersion $
        -- TODO: this is missing!? Spec: require tx𝜔 = ⊥ ∨ tx𝛼 = ⊥
        -- Require any pending utxo to decommit to be consistent
        requireApplicableDecommitTx $ \(activeUTxOAfterDecommit, mUtxoToDecommit) ->
          -- Wait for the deposit and require any pending commit to be consistent
          waitForDeposit activeUTxOAfterDecommit $ \(activeUTxO, mUtxoToCommit) ->
            -- Resolve transactions by-id
            waitResolvableTxs $ \requestedTxs -> do
              -- Spec: require 𝑈_active ◦ Treq ≠ ⊥
              --       𝑈 ← 𝑈_active ◦ Treq
              requireApplyTxs activeUTxO requestedTxs $ \u -> do
                let snapshotUTxO = u `withoutUTxO` fromMaybe mempty mUtxoToCommit
                -- Spec: ŝ ← ̅S.s + 1
                -- NOTE: confSn == seenSn == sn here
                let nextSnapshot =
                      Snapshot
                        { headId
                        , version = version
                        , number = sn
                        , confirmed = requestedTxs
                        , utxo = snapshotUTxO
                        , utxoToCommit = mUtxoToCommit
                        , utxoToDecommit = mUtxoToDecommit
                        }

                -- Spec: 𝜂 ← combine(𝑈)
                --       𝜂𝛼 ← combine(𝑈𝛼)
                --       𝜂𝜔 ← combine(outputs(tx𝜔 ))
                --       σᵢ ← MS-Sign(kₕˢⁱᵍ, (cid‖v‖ŝ‖η‖η𝛼‖ηω))
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
                  let (newLocalTxs, newLocalUTxO) = pruneTransactions u
                  newState
                    SnapshotRequested
                      { snapshot = nextSnapshot
                      , requestedTxIds
                      , newLocalUTxO
                      , newLocalTxs
                      , newCurrentDepositTxId = mDepositTxId
                      }
 where
  requireReqSn continue
    | sv /= version =
        Error $ RequireFailed $ ReqSvNumberInvalid{requestedSv = sv, lastSeenSv = version}
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

  waitOnSnapshotVersion continue
    | version == sv =
        continue
    | otherwise =
        wait $ WaitOnSnapshotVersion sv

  waitResolvableTxs continue =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> continue $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
      unseen -> wait $ WaitOnTxs unseen

  waitForDeposit activeUTxOAfterDecommit cont =
    case mDepositTxId of
      Nothing -> cont (activeUTxOAfterDecommit, Nothing)
      Just depositTxId ->
        -- XXX: We may need to wait quite long here and this makes losing
        -- the 'ReqSn' due to a restart (fail-recovery) quite likely
        case Map.lookup depositTxId pendingDeposits of
          Nothing -> wait WaitOnDepositObserved{depositTxId}
          Just Deposit{status, deposited}
            | status == Inactive -> wait WaitOnDepositActivation{depositTxId}
            | status == Expired -> Error $ RequireFailed RequestedDepositExpired{depositTxId}
            | otherwise ->
                -- NOTE: this makes the commits sequential in a sense that you can't
                -- commit unless the previous commit is settled.
                if sv == confVersion && isJust confUTxOToCommit
                  then
                    if confUTxOToCommit == Just deposited
                      then cont (activeUTxOAfterDecommit <> deposited, confUTxOToCommit)
                      else Error $ RequireFailed ReqSnCommitNotSettled
                  else do
                    let activeUTxOAfterCommit = activeUTxOAfterDecommit <> deposited
                    cont (activeUTxOAfterCommit, Just deposited)

  requireApplicableDecommitTx cont =
    case mDecommitTx of
      Nothing -> cont (confirmedUTxO, Nothing)
      Just decommitTx ->
        -- Spec:
        -- require tx𝜔 = ⊥ ∨ 𝑈𝛼 = ∅
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

  -- NOTE: at this point we know those transactions apply on the localUTxO because they
  -- are part of the localTxs. The snapshot can contain less transactions than the ones
  -- we have seen at this stage, but they all _must_ apply correctly to the latest
  -- snapshot's UTxO set, eg. it's illegal for a snapshot leader to request a snapshot
  -- containing transactions that do not apply cleanly.
  requireApplyTxs utxo requestedTxs cont =
    case applyTransactions ledger currentSlot utxo requestedTxs of
      Left (tx, err) ->
        Error $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
      Right u -> cont u

  pruneTransactions utxo = do
    -- NOTE: Using foldl' is important to apply transacations in the correct
    -- order. That is, left-associative as new transactions are first validated
    -- and then appended to `localTxs` (when aggregating
    -- 'TransactionAppliedToLocalUTxO').
    foldl' go ([], utxo) localTxs
   where
    go (txs, u) tx =
      -- XXX: We prune transactions on any error, while only some of them are
      -- actually expected.
      -- For example: `OutsideValidityIntervalUTxO` ledger errors are expected
      -- here when a tx becomes invalid.
      case applyTransactions ledger currentSlot u [tx] of
        Left (_, _) -> (txs, u)
        Right u' -> (txs <> [tx], u')

  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  Snapshot{version = confVersion} = getSnapshot confirmedSnapshot

  confUTxOToCommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToCommit}} -> utxoToCommit

  confUTxOToDecommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToDecommit}} -> utxoToDecommit

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = case confirmedSnapshot of
    InitialSnapshot{initialUTxO} -> initialUTxO
    ConfirmedSnapshot{snapshot = Snapshot{utxo, utxoToCommit}} -> utxo <> fromMaybe mempty utxoToCommit

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version} = coordinatedHeadState

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
    waitOnSeenSnapshot $ \snapshot sigs -> do
      -- Spec: require (j,⋅) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        -- Spec: ̂Σ[j] ← σⱼ
        (newState PartySignedSnapshot{snapshot, party = otherParty, signature = snapshotSignature} <>) $
          --       if ∀k ∈ [1..n] : (k,·) ∈ ̂Σ
          ifAllMembersHaveSigned snapshot sigs $ \sigs' -> do
            -- Spec: σ̃ ← MS-ASig(kₕˢᵉᵗᵘᵖ,̂Σ)
            let multisig = aggregateInOrder sigs' parties
            -- Spec: η ← combine(𝑈ˆ)
            --       𝜂𝛼 ← combine(𝑈𝛼)
            --       𝑈𝜔 ← outputs(tx𝜔 )
            --       ηω ← combine(𝑈𝜔)
            --       require MS-Verify(k ̃H, (cid‖v̂‖ŝ‖η‖η𝛼‖ηω), σ̃)
            requireVerifiedMultisignature multisig snapshot $
              do
                -- Spec: ̅S ← snObj(v̂, ŝ, Û, T̂, 𝑈𝛼, 𝑈𝜔)
                --       ̅S.σ ← ̃σ
                newState SnapshotConfirmed{headId, snapshot, signatures = multisig}
                -- Spec: if η𝛼 ≠ ⊥
                --         postTx (increment, v̂, ŝ, η, η𝛼, ηω)
                & maybePostIncrementTx snapshot multisig
                -- Spec: if txω ≠ ⊥
                --         postTx (decrement, v̂, ŝ, η, η𝛼, ηω)
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
      SeenSnapshot snapshot sigs
        | seenSn == sn -> continue snapshot sigs
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
                { snapshot
                , party = otherParty
                , signature = snapshotSignature
                }

  requireVerifiedMultisignature multisig msg cont =
    case verifyMultiSignature vkeys multisig msg of
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
    if isLeader parameters party nextSn && not (null localTxs)
      then
        outcome
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> localTxs) decommitTx currentDepositTxId)
      else outcome

  maybePostIncrementTx snapshot@Snapshot{utxoToCommit} signatures outcome =
    -- TODO: check status (again)?
    case find (\(_, Deposit{deposited}) -> Just deposited == utxoToCommit) $ Map.toList pendingDeposits of
      Just (depositTxId, Deposit{deposited}) ->
        outcome
          <> newState CommitApproved{headId, utxoToCommit = deposited}
          <> cause
            OnChainEffect
              { postChainTx =
                  IncrementTx
                    { headId
                    , headParameters = parameters
                    , incrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
                    , depositTxId
                    }
              }
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
                    { headId
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
    } = openState

  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx, currentDepositTxId, version} = coordinatedHeadState

-- | Client request to recover deposited UTxO.
--
-- __Transition__: 'OpenState' → 'OpenState'
onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Error $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited} ->
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
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl currentSlot openState decommitTx =
  -- Spec: wait 𝑈𝛼 = ∅ ^ txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $ \newLocalUTxO -> do
    -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
    let decommitUTxO = utxoFromTx decommitTx
        activeUTxO = newLocalUTxO `withoutUTxO` decommitUTxO
    -- Spec: txω ← tx
    newState DecommitRecorded{headId, decommitTx, newLocalUTxO = activeUTxO, utxoToDecommit = decommitUTxO}
      -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
      --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
      <> maybeRequestSnapshot
 where
  waitOnApplicableDecommit cont =
    case mExistingDecommitTx of
      Nothing ->
        case applyTransactions currentSlot localUTxO [decommitTx] of
          Right utxo' -> cont utxo'
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

  maybeRequestSnapshot =
    if not snapshotInFlight && isLeader parameters party nextSn
      then cause (NetworkEffect (ReqSn version nextSn (txId <$> localTxs) (Just decommitTx) Nothing))
      else noop

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  CoordinatedHeadState
    { decommitTx = mExistingDecommitTx
    , confirmedSnapshot
    , localTxs
    , localUTxO
    , version
    , seenSnapshot
    } = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
    } = openState

-- | Process the chain (and time) advancing in an open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- This is primarily used to track deposits and either drop them or request
-- snapshots for inclusion.
onOpenChainTick :: IsTx tx => Environment -> PendingDeposits tx -> OpenState tx -> UTCTime -> Outcome tx
onOpenChainTick env pendingDeposits st chainTime =
  -- Determine new active and new expired
  updateDeposits $ \newActive newExpired ->
    -- Emit state change for both
    -- XXX: This is a bit messy
    ((mkDepositActivated newActive <> mkDepositExpired newExpired) <>) $
      -- Apply state changes and pick next active to request snapshot
      -- XXX: This is smelly as we rely on Map <> to override entries (left
      -- biased). This is also weird because we want to actually apply the state
      -- change and also to determine the next active.
      withNextActive (newActive <> newExpired <> pendingDeposits) $ \depositTxId ->
        -- REVIEW: this is not really a wait, but discard?
        -- TODO: Spec: wait tx𝜔 = ⊥ ∧ 𝑈𝛼 = ∅
        if isNothing decommitTx
          && isNothing currentDepositTxId
          && not snapshotInFlight
          && isLeader parameters party nextSn
          then
            -- XXX: This state update has no equivalence in the
            -- spec. Do we really need to store that we have
            -- requested a snapshot? If yes, should update spec.
            newState SnapshotRequestDecided{snapshotNumber = nextSn}
              -- Spec: multicast (reqSn,̂ 𝑣,̄ 𝒮.𝑠 + 1,̂ 𝒯, 𝑈𝛼, ⊥)
              <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> localTxs) Nothing (Just depositTxId))
          else
            noop
 where
  updateDeposits cont =
    uncurry cont $ Map.foldlWithKey updateDeposit (mempty, mempty) pendingDeposits

  updateDeposit (newActive, newExpired) depositTxId deposit@Deposit{status} =
    let newStatus = determineStatus deposit
        d' = deposit{status = newStatus}
     in case newStatus of
          Active | status /= Active -> (Map.insert depositTxId d' newActive, newExpired)
          Expired | status /= Expired -> (newActive, Map.insert depositTxId d' newExpired)
          _ -> (newActive, newExpired)

  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositPeriod = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  -- REVIEW! check what if there are more than 1 new active deposit
  -- What is the sorting criteria to pick next?
  withNextActive :: forall tx. (Eq (UTxOType tx), Monoid (UTxOType tx)) => Map (TxIdType tx) (Deposit tx) -> (TxIdType tx -> Outcome tx) -> Outcome tx
  withNextActive deposits cont = do
    -- NOTE: Do not consider empty deposits.
    let p :: (x, Deposit tx) -> Bool
        p (_, Deposit{deposited, status}) = deposited /= mempty && status == Active
    maybe noop (cont . fst) . find p $ Map.toList deposits

  mkDepositActivated m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositActivated{depositTxId, chainTime, deposit}

  mkDepositExpired m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositExpired{depositTxId, chainTime, deposit}

  nextSn = confirmedSn + 1

  Environment{party, depositPeriod} = env

  CoordinatedHeadState
    { localTxs
    , confirmedSnapshot
    , seenSnapshot
    , version
    , decommitTx
    , currentDepositTxId
    } = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  OpenState{coordinatedHeadState, parameters} = st

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

-- | Observe a increment transaction. If the outputs match the ones of the
-- pending commit UTxO, then we consider the deposit/increment finalized, and remove the
-- increment UTxO from 'pendingDeposits' from the local state.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainIncrementTx ::
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Deposit TxId
  TxIdType tx ->
  Outcome tx
onOpenChainIncrementTx openState newChainState newVersion depositTxId =
  newState CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
 where
  OpenState{headId} = openState

-- | Observe a decrement transaction. If the outputs match the ones of the
-- pending decommit tx, then we consider the decommit finalized, and remove the
-- decommit tx in flight.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainDecrementTx ::
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Outputs removed by the decrement
  UTxOType tx ->
  Outcome tx
onOpenChainDecrementTx openState newChainState newVersion distributedUTxO =
  newState
    DecommitFinalized
      { chainState = newChainState
      , headId
      , newVersion
      , distributedUTxO
      }
 where
  OpenState{headId} = openState

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
  -- Spec: η ← combine(̅S.𝑈)
  --       η𝛼 ← combine(S.𝑈𝛼)
  --       ηω ← combine(S.𝑈ω)
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, η𝛼, ηω,ξ)
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
          -- XXX: As we use 'version' in the contest here, this is implies
          -- that our last 'confirmedSnapshot' must match version or
          -- version-1. Assert this fact?
          -- Spec: η ← combine(̅S.𝑈)
          --       η𝛼 ← combine(S.𝑈𝛼)
          --       ηω ← combine(S.𝑈ω)
          --       ξ ← ̅S.σ
          --       postTx (contest, ̅S.v, ̅S.s, η, η𝛼, ηω, ξ)
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
              [ SnapshotConfirmed{headId, snapshot, signatures}
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
    , utxoToDecommit = lastSeenSd
    } = getSnapshot currentConfirmedSnapshot

  requestedSnapshot@Snapshot
    { version = requestedSv
    , number = requestedSn
    , utxoToCommit = requestedSc
    , utxoToDecommit = requestedSd
    } = getSnapshot requestedConfirmedSnapshot

  requireVerifiedSameSnapshot cont =
    if requestedSnapshot == currentSnapshot
      then cont
      else Error . SideLoadSnapshotFailed $ SideLoadInitialSnapshotMismatch

  requireVerifiedSnapshotNumber cont =
    if requestedSn >= lastSeenSn
      then cont
      else Error . SideLoadSnapshotFailed $ SideLoadSnNumberInvalid{requestedSn, lastSeenSn}

  requireVerifiedL1Snapshot cont
    | requestedSv /= lastSeenSv = Error . SideLoadSnapshotFailed $ SideLoadSvNumberInvalid{requestedSv, lastSeenSv}
    | requestedSc /= lastSeenSc = Error . SideLoadSnapshotFailed $ SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc}
    | requestedSd /= lastSeenSd = Error . SideLoadSnapshotFailed $ SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}
    | otherwise = cont

  requireVerifiedMultisignature snapshot signatories cont =
    case verifyMultiSignature vkeys signatories snapshot of
      Verified -> cont
      FailedKeys failures ->
        Error . SideLoadSnapshotFailed $ SideLoadInvalidMultisignature{multisig = show signatories, vkeys = failures}
      KeyNumberMismatch ->
        Error . SideLoadSnapshotFailed $ SideLoadInvalidMultisignature{multisig = show signatories, vkeys}

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
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
        -- XXX: As we use 'version' in the contest here, this is implies
        -- that our last 'confirmedSnapshot' must match version or
        -- version-1. Assert this fact?
        -- Spec: η ← combine(̅S.𝑈)
        --       η𝛼 ← combine(S.𝑈𝛼)
        --       ηω ← combine(S.𝑈ω)
        --       ξ ← ̅S.σ
        --       postTx (contest, ̅S.v, ̅S.s, η, η𝛼, ηω, ξ)
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

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedClientFanout ::
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  cause
    OnChainEffect
      { postChainTx =
          FanoutTx
            { utxo
            , -- XXX: Perhaps move this check down so it can be more readily
              -- tested.
              -- Commit:
              -- Here we check that to include in the fanout; if the versions
              -- are the same, the utxoToCommit has not been used on chain yet
              -- so we disregard, so we must not fan it out.
              utxoToCommit =
                if snapshotVersion == version
                  then Nothing
                  else utxoToCommit
            , -- For decommit, if it hasn't happened, we _must_ fan it out.
              utxoToDecommit =
                if snapshotVersion == version
                  then utxoToDecommit
                  else Nothing
            , headSeed
            , contestationDeadline
            }
      }
 where
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot

  ClosedState{headSeed, confirmedSnapshot, contestationDeadline, version} = closedState

-- | Observe a fanout transaction by finalize the head state and notifying
-- clients about it.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  newState HeadFannedOut{headId, utxo = fanoutUTxO, chainState = newChainState}
 where
  ClosedState{headId} = closedState

-- | Handles inputs and converts them into 'StateChanged' events along with
-- 'Effect's, in case it is processed successfully. Later, the Node will
-- 'aggregate' the events, resulting in a new 'HeadState'.
update ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current NodeState to validate the command against.
  NodeState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
update env ledger NodeState{headState = st, pendingDeposits, currentSlot} ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  (Idle _, ClientInput Init) ->
    onIdleClientInit env
  (Idle _, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState}) ->
    onIdleChainInitTx env newChainState headId headSeed headParameters participants
  (Initial initialState@InitialState{headId = ourHeadId}, ChainInput Observation{observedTx = OnCommitTx{headId, party = pt, committed = utxo}, newChainState})
    | ourHeadId == headId -> onInitialChainCommitTx initialState newChainState pt utxo
    | otherwise -> Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Initial initialState, ClientInput Abort) ->
    onInitialClientAbort initialState
  (Initial initialState@InitialState{headId = ourHeadId}, ChainInput Observation{observedTx = OnCollectComTx{headId}, newChainState})
    | ourHeadId == headId -> onInitialChainCollectTx initialState newChainState
    | otherwise -> Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Initial InitialState{headId = ourHeadId, committed}, ChainInput Observation{observedTx = OnAbortTx{headId}, newChainState})
    | ourHeadId == headId -> onInitialChainAbortTx newChainState committed headId
    | otherwise -> Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Open
  (Open openState, ClientInput Close) ->
    onOpenClientClose openState
  (Open{}, ClientInput (NewTx tx)) ->
    onOpenClientNewTx tx
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env ledger currentSlot openState ttl tx
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    onOpenNetworkReqSn env ledger pendingDeposits currentSlot openState sender sv sn txIds decommitTx depositTxId
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    onOpenNetworkAckSn env pendingDeposits openState sender snapshotSignature sn
  ( Open openState@OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ClientInput (SideLoadSnapshot confirmedSnapshot)) ->
    let Snapshot{headId = otherHeadId} = getSnapshot confirmedSnapshot
     in if ourHeadId == otherHeadId
          then onOpenClientSideLoadSnapshot openState confirmedSnapshot
          else Error NotOurHead{ourHeadId, otherHeadId}
  -- NOTE: If posting the collectCom transaction failed in the open state, then
  -- another party likely opened the head before us and it's okay to ignore.
  (Open{}, ChainInput PostTxError{postChainTx = CollectComTx{}}) ->
    noop
  (Open OpenState{headId, coordinatedHeadState}, ClientInput Decommit{decommitTx}) -> do
    onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ledger ttl currentSlot openState transaction
  (Open OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{}, ChainInput Tick{chainTime, point}) ->
    -- XXX: We originally forgot the normal TickObserved state event here and so
    -- time did not advance in an open head anymore. This is a hint that we
    -- should compose event handling better.
    newState TickObserved{point}
      <> onOpenChainTick env pendingDeposits openState chainTime
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId}, newChainState})
    | ourHeadId == headId ->
        onOpenChainIncrementTx openState newChainState newVersion depositTxId
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO}, newChainState})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx openState newChainState newVersion distributedUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed ClosedState{contestationDeadline, readyToFanoutSent, headId}, ChainInput Tick{chainTime, point})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        newState TickObserved{point}
          <> newState HeadIsReadyToFanout{headId}
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onClosedChainFanoutTx closedState newChainState fanoutUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Node-level
  (_, ClientInput Recover{recoverTxId}) -> do
    onClientRecover currentSlot pendingDeposits recoverTxId
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState}) ->
    newState DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
  -- General
  (_, ChainInput Rollback{rolledBackChainState}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
  (_, ChainInput Tick{point}) ->
    newState TickObserved{point}
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    cause . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  (_, ClientInput{clientInput}) ->
    cause . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    Error $ UnhandledInput ev st

-- * NodeState aggregate

-- | Reflect 'StateChanged' events onto the 'NodeState' aggregateNodeState.
aggregateNodeState :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
aggregateNodeState nodeState sc =
  let st = aggregate (headState nodeState) sc
      ns@NodeState{pendingDeposits} = nodeState{headState = st}
   in case sc of
        HeadOpened{chainState} ->
          ns{pendingDeposits, currentSlot = chainStateSlot chainState}
        DepositRecorded{headId, depositTxId, deposited, created, deadline} ->
          ns{pendingDeposits = Map.insert depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} pendingDeposits}
        DepositActivated{depositTxId, deposit} ->
          ns{pendingDeposits = Map.insert depositTxId deposit pendingDeposits}
        DepositExpired{depositTxId, deposit} ->
          ns{pendingDeposits = Map.insert depositTxId deposit pendingDeposits}
        DepositRecovered{depositTxId} ->
          ns{pendingDeposits = Map.delete depositTxId pendingDeposits}
        CommitFinalized{chainState, newVersion, depositTxId} ->
          case st of
            Open
              os@OpenState{coordinatedHeadState} ->
                let deposit = Map.lookup depositTxId pendingDeposits
                    newUTxO = maybe mempty (\Deposit{deposited} -> deposited) deposit
                 in ns
                      { headState =
                          Open
                            os
                              { chainState
                              , coordinatedHeadState =
                                  coordinatedHeadState
                                    { version = newVersion
                                    , -- NOTE: This must correspond to the just finalized
                                      -- depositTxId, but we should not verify this here.
                                      currentDepositTxId = Nothing
                                    , localUTxO = localUTxO <> newUTxO
                                    }
                              }
                      , pendingDeposits = Map.delete depositTxId pendingDeposits
                      }
               where
                CoordinatedHeadState{localUTxO} = coordinatedHeadState
            _otherState ->
              ns
                { pendingDeposits = Map.delete depositTxId pendingDeposits
                }
        TickObserved{point} ->
          ns{currentSlot = chainPointSlot point}
        _ -> ns

-- * HeadState aggregate

-- | Reflect 'StateChanged' events onto the 'HeadState' aggregate.
aggregate :: IsChainState tx => HeadState tx -> StateChanged tx -> HeadState tx
aggregate st = \case
  NetworkConnected -> st
  NetworkDisconnected -> st
  NetworkVersionMismatch{} -> st
  NetworkClusterIDMismatch{} -> st
  PeerConnected{} -> st
  PeerDisconnected{} -> st
  HeadInitialized{parameters = parameters@HeadParameters{parties}, headId, headSeed, chainState} ->
    Initial
      InitialState
        { parameters = parameters
        , pendingCommits = Set.fromList parties
        , committed = mempty
        , chainState
        , headId
        , headSeed
        }
  CommittedUTxO{committedUTxO, chainState, party} ->
    case st of
      Initial InitialState{parameters, pendingCommits, committed, headId, headSeed} ->
        Initial
          InitialState
            { parameters
            , pendingCommits = remainingParties
            , committed = newCommitted
            , chainState
            , headId
            , headSeed
            }
       where
        newCommitted = Map.insert party committedUTxO committed
        remainingParties = Set.delete party pendingCommits
      _otherState -> st
  HeadAborted{chainState} ->
    Idle $
      IdleState
        { chainState
        }
  HeadOpened{chainState, initialUTxO} ->
    case st of
      Initial InitialState{parameters, headId, headSeed} ->
        Open
          OpenState
            { parameters
            , coordinatedHeadState =
                CoordinatedHeadState
                  { localUTxO = initialUTxO
                  , allTxs = mempty
                  , localTxs = mempty
                  , confirmedSnapshot = InitialSnapshot{headId, initialUTxO}
                  , seenSnapshot = NoSeenSnapshot
                  , currentDepositTxId = Nothing
                  , decommitTx = Nothing
                  , version = 0
                  }
            , chainState
            , headId
            , headSeed
            }
      _otherState -> st
  TransactionReceived{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { allTxs = allTxs <> fromList [(txId tx, tx)]
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> st
  TransactionAppliedToLocalUTxO{tx, newLocalUTxO} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { localUTxO = newLocalUTxO
                  , -- NOTE: Order of transactions is important here. See also
                    -- 'pruneTransactions'.
                    localTxs = localTxs <> [tx]
                  }
            }
       where
        CoordinatedHeadState{localTxs} = coordinatedHeadState
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
  SnapshotRequested{snapshot, requestedTxIds, newLocalUTxO, newLocalTxs, newCurrentDepositTxId} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot = SeenSnapshot snapshot mempty
                  , localTxs = newLocalTxs
                  , localUTxO = newLocalUTxO
                  , allTxs = foldr Map.delete allTxs requestedTxIds
                  , currentDepositTxId = newCurrentDepositTxId
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
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
  SnapshotConfirmed{snapshot, signatures} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { confirmedSnapshot =
                      ConfirmedSnapshot
                        { snapshot
                        , signatures
                        }
                  , seenSnapshot = LastSeenSnapshot number
                  }
            }
       where
        Snapshot{number} = snapshot
      _otherState -> st
  LocalStateCleared{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState = coordinatedHeadState@CoordinatedHeadState{confirmedSnapshot}} ->
        Open
          os
            { coordinatedHeadState =
                case confirmedSnapshot of
                  InitialSnapshot{initialUTxO} ->
                    coordinatedHeadState
                      { localUTxO = initialUTxO
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = NoSeenSnapshot
                      }
                  ConfirmedSnapshot{snapshot = Snapshot{utxo}} ->
                    coordinatedHeadState
                      { localUTxO = utxo
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = LastSeenSnapshot snapshotNumber
                      }
            }
      _otherState -> st
  DepositRecorded{} -> st
  DepositActivated{} -> st
  DepositExpired{} -> st
  CommitApproved{} -> st
  DepositRecovered{} -> st
  CommitFinalized{} -> st
  DecommitRecorded{decommitTx, newLocalUTxO} -> case st of
    Open
      os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { localUTxO = newLocalUTxO
                  , decommitTx = Just decommitTx
                  }
            }
    _otherState -> st
  DecommitApproved{} -> st
  DecommitInvalid{} -> st
  DecommitFinalized{chainState, newVersion} ->
    case st of
      Open
        os@OpenState{coordinatedHeadState} ->
          Open
            os
              { chainState
              , coordinatedHeadState =
                  coordinatedHeadState
                    { decommitTx = Nothing
                    , version = newVersion
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
        Idle $
          IdleState
            { chainState
            }
      _otherState -> st
  HeadIsReadyToFanout{} ->
    case st of
      Closed cst -> Closed cst{readyToFanoutSent = True}
      _otherState -> st
  ChainRolledBack{chainState} ->
    setChainState chainState st
  TickObserved{} -> st
  IgnoredHeadInitializing{} -> st
  TxInvalid{transaction} -> case st of
    Open ost@OpenState{coordinatedHeadState = coordState@CoordinatedHeadState{allTxs = allTransactions}} ->
      Open ost{coordinatedHeadState = coordState{allTxs = foldr Map.delete allTransactions [txId transaction]}}
    _otherState -> st
  Checkpoint NodeState{headState = state'} -> state'

aggregateState ::
  IsChainState tx =>
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState s outcome =
  foldl' aggregateNodeState s $ collectStateChanged outcome
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
  HeadInitialized{chainState} -> pushNewState chainState history
  CommittedUTxO{chainState} -> pushNewState chainState history
  HeadAborted{chainState} -> pushNewState chainState history
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
  ChainRolledBack{chainState} ->
    rollbackHistory (chainStateSlot chainState) history
  TickObserved{} -> history
  CommitApproved{} -> history
  DecommitApproved{} -> history
  DecommitInvalid{} -> history
  IgnoredHeadInitializing{} -> history
  TxInvalid{} -> history
  LocalStateCleared{} -> history
  Checkpoint NodeState{headState = state'} -> initHistory $ getChainState state'
