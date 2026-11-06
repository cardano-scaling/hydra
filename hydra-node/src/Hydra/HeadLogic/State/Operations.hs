{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Pure state operations shared by input rules and event application.
module Hydra.HeadLogic.State.Operations where

import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    seenSnapshot,
    version
  ),
  FanoutMode (AutoDrain, DistributingSelection),
  FinalizedSnapshot (..),
  HeadState (..),
  OpenState (OpenState, headId),
  PartialFanoutState (..),
  SeenSnapshot (LastSeenSnapshot, SeenSnapshot, lastSeen, snapshot),
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx (..), TxIdType, UTxOType, utxoFromTx, withoutUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotVersion, getSnapshot)

-- | A confirmation can arrive after its increment/decrement observation. Keep
-- it for reposting if that observation rolls back. Shared with side-loading.
retainConfirmedSettlement :: IsTx tx => ChainSlot -> SnapshotVersion -> ConfirmedSnapshot tx -> Bool -> Maybe (FinalizedSnapshot tx) -> Maybe (FinalizedSnapshot tx)
retainConfirmedSettlement slot version confirmed settles previous
  | version == (getSnapshot confirmed).version + 1 && settles = Just $ FinalizedSnapshot confirmed slot
  | otherwise = previous

-- | Maximum number of transaction ids per snapshot. This effectively limits our
-- "block size" and ensures it does not grow arbitrarily with the backlog of
-- pending transactions (localTxs). Only applied when requesting snapshots as
-- a leader; followers accept larger requests, so this can change without a
-- coordinated upgrade.
--
-- 4000 was chosen from backlog-drain sweeps at caps 1000 to 8000 (see
-- hydra-cluster/bench/BASELINES.md): per-round costs scale with the backlog,
-- so under sustained overload larger caps drain strictly faster (2.7x at
-- 4000 vs 1000 on a 24k-tx backlog) with lower confirmation latency and
-- lower peak memory, while the cap never binds under light load. Gains taper
-- beyond 4000 and worst-case round size keeps growing (ReqSn bytes, failed
-- round rework, follower reapply stall), so we stop at roughly 10x below
-- etcd's 1.5MiB request limit, which a too-large ReqSn would wedge on.
maxTxsPerSnapshot :: Int
maxTxsPerSnapshot = 4000

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
  HeadState tx
closedToFanoutProgress closedState chainState remaining distributed mode =
  FanoutProgress $ closedToPartialFanout closedState chainState remaining distributed mode

closedToPartialFanout :: ClosedState tx -> ChainStateType tx -> UTxOType tx -> UTxOType tx -> FanoutMode tx -> PartialFanoutState tx
closedToPartialFanout closedState chainState remaining distributed mode =
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

-- | The deposit claimed by a retained finalized increment ('finalizedCommit'),
-- if any. Such a deposit only resurfaces in 'pendingDeposits' when a rollback
-- erased its increment, and it is settled solely by re-posting that increment
-- ('maybeRepostIncrementTx'): it must never be proposed for a snapshot again
-- (the deposited funds are already counted in the head) nor recovered (that
-- would corrupt the L2 ledger). See #2741.
finalizedDepositTxId :: IsTx tx => Maybe (FinalizedSnapshot tx) -> Maybe (TxIdType tx)
finalizedDepositTxId finalizedCommit = do
  FinalizedSnapshot{snapshot} <- finalizedCommit
  (getSnapshot snapshot).depositTxId

-- | The UTxO the next request is validated against: the snapshot in flight, if
-- any, else the confirmed one.
nextRequestUTxO :: IsTx tx => CoordinatedHeadState tx -> UTxOType tx
nextRequestUTxO CoordinatedHeadState{seenSnapshot, confirmedSnapshot, version} =
  case seenSnapshot of
    SeenSnapshot{snapshot} -> settledUTxO version snapshot
    _ -> settledUTxO version (getSnapshot confirmedSnapshot)

-- | Whether a request applying these txs to this UTxO can absorb the deposit:
-- it must name no output the UTxO holds or the txs create. Deposit datums are
-- not authenticated, so anyone can name existing outputs, and absorbing such a
-- deposit would count value the ledger does not have.
absorbable :: IsTx tx => UTxOType tx -> [tx] -> UTxOType tx -> Bool
absorbable utxo txs =
  let created = foldMap utxoFromTx txs
   in \deposited -> namesNoneOf deposited utxo && namesNoneOf deposited created

-- | Whether a deposit of the given outputs names none of the output references
-- the given UTxO holds.
namesNoneOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
namesNoneOf deposited utxo =
  -- NOTE: remove the UTxO from the deposit, not the deposit from the UTxO: the
  -- deposit is small, so this is cheaper.
  deposited `withoutUTxO` utxo == deposited

-- | Update the retained 'finalizedCommit' on (re-)observation of an increment
-- claiming @depositTxId@:
--
--   * the locally confirmed snapshot claims it: retain that snapshot — only it
--     can re-claim the deposit if a rollback erases the increment, and
--     'confirmedSnapshot' may advance past it;
--   * the already retained snapshot claims it: the increment re-landed (e.g.
--     re-posted after a rollback) — re-stamp the observation slot, so a
--     rollback of the re-landed increment still triggers a re-post;
--   * neither: the observation raced local snapshot confirmation (another
--     party collected the last AckSn and posted first) — retention happens
--     once the snapshot confirms, see the 'SnapshotConfirmed' branch of
--     the snapshot coordination machine.
--
-- See #2741.
retainFinalizedCommit ::
  IsTx tx =>
  ChainSlot ->
  TxIdType tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  Maybe (FinalizedSnapshot tx)
retainFinalizedCommit slot depositTxId confirmedSnapshot retained
  | claims confirmedSnapshot = Just FinalizedSnapshot{snapshot = confirmedSnapshot, observedAtSlot = slot}
  | Just f@FinalizedSnapshot{snapshot} <- retained, claims snapshot = Just f{observedAtSlot = slot}
  | otherwise = retained
 where
  claims s = (getSnapshot s).depositTxId == Just depositTxId

-- | Update the retained 'finalizedDecommit' on (re-)observation of a decrement
-- bumping to @newVersion@; the counterpart of 'retainFinalizedCommit'. A
-- snapshot is tied to the decrement by identity, not by merely carrying a
-- decommit: only the decrement of that very snapshot bumps the version to
-- exactly one past the snapshot's (with a second decommit racing, the locally
-- confirmed snapshot may still be the previous, already settled one).
retainFinalizedDecommit ::
  IsTx tx =>
  ChainSlot ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  Maybe (FinalizedSnapshot tx)
retainFinalizedDecommit slot newVersion confirmedSnapshot retained
  | settles confirmedSnapshot = Just FinalizedSnapshot{snapshot = confirmedSnapshot, observedAtSlot = slot}
  | Just f@FinalizedSnapshot{snapshot} <- retained, settles snapshot = Just f{observedAtSlot = slot}
  | otherwise = retained
 where
  settles s = isJust (getSnapshot s).utxoToDecommit && (getSnapshot s).version + 1 == newVersion

-- | Whether a finalized increment or decrement at 'newVersion' was applied
-- before and re-landed after a rollback. The local version never rolls back.
isReobservation :: SnapshotVersion -> CoordinatedHeadState tx -> Bool
isReobservation newVersion CoordinatedHeadState{version} = newVersion <= version

-- | The 'SeenSnapshot' after a version bump. A snapshot already in flight is
-- kept: every party has processed its 'ReqSn', so it can still complete and
-- chain the next one with the bumped version.
seenSnapshotAfterVersionBump :: IsTx tx => ConfirmedSnapshot tx -> SeenSnapshot tx -> SeenSnapshot tx
seenSnapshotAfterVersionBump confirmedSnapshot = \case
  seen@SeenSnapshot{} -> seen
  _ -> LastSeenSnapshot{lastSeen = (getSnapshot confirmedSnapshot).number}

-- | The spendable UTxO of a snapshot, given the head's current 'SnapshotVersion'.
-- A pending commit only becomes spendable once its increment has landed on
-- chain, which bumps the version past the snapshot's.
settledUTxO :: IsTx tx => SnapshotVersion -> Snapshot tx -> UTxOType tx
settledUTxO headVersion Snapshot{utxo, utxoToCommit, version}
  | headVersion > version = utxo <> fromMaybe mempty utxoToCommit
  | otherwise = utxo

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
