module Hydra.HeadLogic.State.Coordinated.Replay (replay) where

import Crem.BaseMachine (InitialState, withInitialState)
import Crem.StateMachine (StateMachineT (Executable), run)
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State.Coordinated.Snapshots qualified as Snapshots (
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.State (
  SnapshotOutput,
  State (..),
  Update (Update),
  contextOf,
  mkSnapshotOutput,
  withContext,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.State qualified as State (
  Context (chainSlot),
  accept,
  adoptSnapshot,
  confirm,
  recordSignature,
  request,
 )
import Hydra.HeadLogic.State.Types (
  LocalTransactionCommand (..),
  SnapshotCommand (..),
  TransactionUpdate (..),
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId, UTxOType)
import Hydra.Tx.Crypto (HydraKey, VerificationKey)

-- | Decide how to treat events upon replay.
--
-- Either, we do nothing (Nothing), construct a new SnapshotOutput directly
-- (`mkSnapshotOutput`) or evaluate the snapshot machine with some input (`live`).
replay ::
  forall tx.
  IsChainState tx =>
  HeadId ->
  [VerificationKey HydraKey] ->
  InitialState (State tx) ->
  UTxOType tx ->
  StateChanged tx ->
  Maybe (SnapshotOutput tx)
replay headId parties initial deposited event = withInitialState initial $ \state _ -> importEvent state
 where
  importEvent :: State tx phase -> Maybe (SnapshotOutput tx)
  importEvent state = case event of
    SnapshotRequestDecided number ->
      Just $ mkSnapshotOutput $ State.request state number
    SnapshotRequested snapshot txs deposit ->
      Just $ mkSnapshotOutput $ State.accept parties state snapshot txs deposit
    PartySignedSnapshot number party signature -> case state of
      Collecting{} -> Just $ mkSnapshotOutput $ State.recordSignature state number party signature
      Idle{} -> Nothing
      Requested{} -> Nothing
    SnapshotConfirmed{snapshot = Nothing, signatures} -> case state of
      Collecting{} -> Just $ mkSnapshotOutput $ State.confirm headId state signatures
      Idle{} -> Nothing
      Requested{} -> Nothing
    SnapshotConfirmed{snapshot = Just snapshot, signatures} ->
      Just $ mkSnapshotOutput $ State.adoptSnapshot headId state snapshot signatures
    TransactionAppliedToLocalUTxO{tx} ->
      Just $ mkSnapshotOutput $ Update state (ReplayTransactionApplication tx) []
    DecommitRecorded{decommitTx = tx} ->
      Just $ mkSnapshotOutput $ Update state (ReplayPendingDecommit tx) []
    ChainRolledBack{chainState} ->
      Just $ mkSnapshotOutput $ Update (withContext (contextOf state){State.chainSlot = chainStateSlot chainState} state) TransactionsUnchanged []
    TransactionReceived tx ->
      Just $ mkSnapshotOutput $ Update state (RunLocalTransactionCommand $ ReceiveTransaction tx) []
    TxInvalid{utxo, transaction = tx, validationError = err} ->
      Just $ mkSnapshotOutput $ Update state (RunLocalTransactionCommand $ MarkTransactionInvalid utxo tx err) []
    LocalStateCleared{snapshotNumber = number} ->
      Just $ live $ ClearLocalState number
    DepositActivated depositId chainTime deposit ->
      Just $ live $ ActivateDeposit depositId chainTime deposit
    DepositRecovered chainState observedHead depositId recovered ->
      Just $ live $ RecordDepositRecovery chainState observedHead depositId recovered
    CommitFinalized{chainState, newVersion, depositTxId} ->
      Just $ live $ RecordCommitFinalization chainState newVersion depositTxId deposited
    DecommitFinalized{chainState, newVersion, distributedUTxO} ->
      Just $ live $ RecordDecommitFinalization chainState newVersion distributedUTxO
    -- The registry owns recording and expiry. Expiry does not discard a signed
    -- claim: its increment may still land before recovery consumes the deposit.
    DepositRecorded{} -> Nothing
    DepositExpired{} -> Nothing
    -- Confirmation already records the certificate and reconciles reservations.
    -- These events only notify clients of approval or rejection.
    CommitApproved{} -> Nothing
    DecommitApproved{} -> Nothing
    DecommitInvalid{} -> Nothing
    -- Connectivity notifications do not change the coordinated ledger.
    NetworkConnected -> Nothing
    NetworkDisconnected -> Nothing
    PeerConnected{} -> Nothing
    PeerDisconnected{} -> Nothing
    NetworkVersionMismatch{} -> Nothing
    NetworkClusterIDMismatch{} -> Nothing
    NetworkBroadcastStalled{} -> Nothing
    NetworkBroadcastResumed -> Nothing
    -- The enclosing lifecycle owns opening, closing and fanout.
    HeadOpened{} -> Nothing
    HeadClosed{} -> Nothing
    HeadContested{} -> Nothing
    HeadIsReadyToFanout{} -> Nothing
    HeadFanoutInitiated{} -> Nothing
    HeadPartialFanoutSelected{} -> Nothing
    HeadFanoutReverted{} -> Nothing
    HeadFannedOut{} -> Nothing
    HeadPartialFannedOut{} -> Nothing
    IgnoredHeadInitializing{} -> Nothing
    -- State.Node applies time and synchronisation changes.
    TickObserved{} -> Nothing
    NodeUnsynced{} -> Nothing
    NodeSynced{} -> Nothing
    -- State.Node.step restores every owner directly from the checkpoint.
    Checkpoint{} -> Nothing

  -- Run the command through the normal Snapshots machine.
  live command = fst $ runIdentity $ run (Executable $ Snapshots.machine headId parties initial) command
