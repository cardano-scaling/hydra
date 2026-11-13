module Hydra.HeadLogic.Machine.Coordinated.Snapshots.Replay (replay) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (InitialState, withInitialState)
import Crem.StateMachine (StateMachineT (Executable), run)
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots qualified as Snapshots
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State (
  SnapshotOutput,
  State (..),
  Update (Update),
  contextOf,
  retained,
  withContext,
 )
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State qualified as State
import Hydra.HeadLogic.Machine.Coordinated.Types (
  Command (..),
  LedgerUpdate (..),
  TransactionCommand (..),
 )
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.Tx (HeadId, UTxOType)
import Hydra.Tx.Crypto (HydraKey, VerificationKey)

-- | Restore retained facts in the snapshot and transaction owners. 'Nothing'
-- leaves these owners unchanged; the registry, lifecycle and clock still replay
-- their events. The supplied deposit value precedes registry consumption.
replay ::
  forall tx.
  IsChainState tx =>
  HeadId ->
  [VerificationKey HydraKey] ->
  InitialState (State tx) ->
  UTxOType tx ->
  StateChanged tx ->
  Maybe (SnapshotOutput tx)
replay headId committee initial deposited event = withInitialState initial $ \state _ -> importEvent state
 where
  importEvent :: State tx phase -> Maybe (SnapshotOutput tx)
  importEvent state = case event of
    SnapshotRequestDecided number ->
      Just $ retained $ State.request state number
    SnapshotRequested snapshot txs deposit ->
      Just $ retained $ State.accept committee state snapshot txs deposit
    PartySignedSnapshot number party signature -> Just $ case state of
      Collecting{} -> retained $ State.recordSignature state number party signature
      Idle{} -> unchanged state
      Requested{} -> unchanged state
    SnapshotConfirmed{snapshot = Nothing, signatures} -> Just $ case state of
      Collecting{} -> retained $ State.confirm headId state signatures
      Idle{} -> unchanged state
      Requested{} -> unchanged state
    SnapshotConfirmed{snapshot = Just snapshot, signatures} ->
      Just $ retained $ State.adoptSnapshot headId state snapshot signatures
    TransactionAppliedToLocalUTxO{tx} ->
      Just $ retained $ Update state (ReplayTransaction tx) []
    DecommitRecorded{decommitTx = tx} ->
      Just $ retained $ Update state (ReplayDecommit tx) []
    ChainRolledBack{chainState} ->
      Just $ retained $ Update (withContext (contextOf state){State.chainSlot = chainStateSlot chainState} state) Unchanged []
    TransactionReceived tx ->
      Just $ live $ Transactions $ ReceiveTransaction tx
    TxInvalid{utxo, transaction = tx, validationError = err} ->
      Just $ live $ Transactions $ ForgetTransaction utxo tx err
    LocalStateCleared{snapshotNumber = number} ->
      Just $ live $ ClearLocalState number
    DepositActivated depositId chainTime deposit ->
      Just $ live $ ActivateDeposit depositId chainTime deposit
    DepositRecovered chainState observedHead depositId recovered ->
      Just $ live $ RecoverDeposit chainState observedHead depositId recovered
    CommitFinalized{chainState, newVersion, depositTxId} ->
      Just $ live $ SettleCommit chainState newVersion depositTxId deposited
    DecommitFinalized{chainState, newVersion, distributedUTxO} ->
      Just $ live $ SettleDecommit chainState newVersion distributedUTxO
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
    -- The clock owner applies time and synchronization changes.
    TickObserved{} -> Nothing
    NodeUnsynced{} -> Nothing
    NodeSynced{} -> Nothing
    -- Machine.step restores every owner directly from the checkpoint.
    Checkpoint{} -> Nothing

  unchanged :: State tx phase -> SnapshotOutput tx
  unchanged state = retained $ Update state Unchanged []
  -- Reuse commands whose storage updates also apply to retained events. This
  -- runs a pure state machine; replay does not publish the resulting events.
  live command = fst $ runIdentity $ run (Executable $ Snapshots.machine headId committee initial) command
