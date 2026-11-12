-- | Import retained facts without applying live admission or verification.
-- Recovery uses the same storage updates, but does not add edges to live graphs.
module Hydra.HeadLogic.Machine.Coordinated.Snapshots.Replay (replay) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (InitialState, withInitialState)
import Crem.StateMachine (StateMachineT (Executable), run)
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots qualified as Snapshots
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State (SnapshotOutput, State (..), Update (Update), contextOf, retained, withContext)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State qualified as State
import Hydra.HeadLogic.Machine.Coordinated.Types (Command (..), LedgerUpdate (..), TransactionCommand (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.Tx (HeadId, UTxOType)
import Hydra.Tx.Crypto (HydraKey, VerificationKey)

replay :: forall tx. IsChainState tx => HeadId -> [VerificationKey HydraKey] -> InitialState (State tx) -> UTxOType tx -> StateChanged tx -> Maybe (SnapshotOutput tx)
replay headId committee initial deposited event = withInitialState initial $ \state _ -> importEvent state
 where
  importEvent :: State tx phase -> Maybe (SnapshotOutput tx)
  importEvent state = case event of
    SnapshotRequestDecided number -> Just $ retained $ State.request state number
    SnapshotRequested snapshot txs deposit -> Just $ retained $ State.accept committee state snapshot txs deposit
    PartySignedSnapshot number party signature -> Just $ case state of
      Collecting{} -> retained $ State.recordSignature state number party signature
      _ -> unchanged state
    SnapshotConfirmed _ Nothing signatures -> Just $ case state of
      Collecting{} -> retained $ State.confirm headId state signatures
      _ -> unchanged state
    SnapshotConfirmed _ (Just snapshot) signatures -> Just $ retained $ State.adoptSnapshot headId state snapshot signatures
    TransactionAppliedToLocalUTxO _ tx -> Just $ retained $ Update state (ReplayTransaction tx) []
    DecommitRecorded _ tx -> Just $ retained $ Update state (ReplayDecommit tx) []
    ChainRolledBack{chainState} -> Just $ retained $ Update (withContext (contextOf state){State.chainSlot = chainStateSlot chainState} state) Unchanged []
    TransactionReceived tx -> Just $ live $ Transactions $ ReceiveTransaction tx
    TxInvalid _ utxo tx err -> Just $ live $ Transactions $ ForgetTransaction utxo tx err
    LocalStateCleared _ number -> Just $ live $ ClearLocalState number
    DepositActivated depositId chainTime deposit -> Just $ live $ ActivateDeposit depositId chainTime deposit
    DepositRecovered chainState observedHead depositId recovered -> Just $ live $ RecoverDeposit chainState observedHead depositId recovered
    CommitFinalized{chainState, newVersion, depositTxId} -> Just $ live $ SettleCommit chainState newVersion depositTxId deposited
    DecommitFinalized{chainState, newVersion, distributedUTxO} -> Just $ live $ SettleDecommit chainState newVersion distributedUTxO
    _ -> Nothing
  unchanged :: State tx phase -> SnapshotOutput tx
  unchanged state = retained $ Update state Unchanged []
  live command = fst $ runIdentity $ run (Executable $ Snapshots.machine headId committee initial) command
