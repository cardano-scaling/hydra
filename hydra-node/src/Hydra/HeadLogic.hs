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
  module Hydra.HeadLogic.Snapshot,
  module Hydra.HeadLogic.Close,
  module Hydra.HeadLogic.Settlement,
  module Hydra.HeadLogic.Snapshot.Request,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Outcome,
) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
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
import Hydra.HeadLogic.Close
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
import Hydra.HeadLogic.Snapshot
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
import Hydra.Ledger (Ledger)
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, SyncedStatus (..), consumeDeposit, depositsForHead, recordDeposit, rollbackDeposits, syncedStatus, updateDeposit)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..))
import Hydra.Tx (
  HeadId,
  HeadSeed,
  IsTx (..),
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)

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
