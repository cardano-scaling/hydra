{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Compose snapshot coordination and transaction ownership. The command port
-- is separate from persistence; accepted commands produce the existing events.
module Hydra.HeadLogic.Machine.Coordinated (
  Runtime,
  Command,
  start,
  step,
  currentState,
  stage,
  reserveDecommit,
  acceptSnapshot,
  recordSignature,
  confirmSnapshot,
  requestSnapshot,
  receiveTransaction,
  retainSnapshotDecommit,
  applyTransaction,
  forgetTransaction,
  adoptSnapshot,
  clearLocalState,
  settleCommit,
  settleDecommit,
  replayEvent,
  components,
) where

import Hydra.Prelude

import Crem.StateMachine (StateMachineT (..), run, withoutRendering)
import Hydra.Chain.ChainState (IsChainState (ChainStateType), chainStateSlot)
import Hydra.HeadLogic.Error (LogicError (AssertionFailed))
import Hydra.HeadLogic.Machine.Coordinated.Snapshots qualified as Snapshots
import Hydra.HeadLogic.Machine.Coordinated.Transactions (Output (..), TransactionState (..))
import Hydra.HeadLogic.Machine.Coordinated.Transactions qualified as Transactions
import Hydra.HeadLogic.Machine.Coordinated.Types (Command (..), SnapshotState (..), TransactionCommand (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotRequest (SnapshotRequest)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), OpenState (..))
import Hydra.Ledger (ValidationError)
import Hydra.Tx (TxIdType, UTxOType)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.HeadParameters (HeadParameters (parties))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (Snapshot, SnapshotNumber, SnapshotVersion)

-- Staging can carry the continuation without reading its persisted projection.
-- Forcing that projection would repeat ledger work just to emit the next event.
data Runtime tx = Runtime ~(CoordinatedHeadState tx) (StateMachineT Identity (Command tx) (Output tx))

start :: IsChainState tx => OpenState tx -> Runtime tx
start os = Runtime os.coordinatedHeadState (withoutRendering $ components os)

-- | The ports enforce the direction of ownership: snapshots can request a
-- rebase; transactions return their own state after performing it.
-- Diagrams use this same composition; 'start' drops the rendering dictionaries.
components :: IsChainState tx => OpenState tx -> StateMachineT Identity (Command tx) (Output tx)
components os =
  Basic (Snapshots.machine os.headId (vkey <$> os.parameters.parties) snapshots)
    `Sequential` Basic (Transactions.machine os.headId snapshots transactions chs.decommitTx)
 where
  chs = os.coordinatedHeadState
  snapshots = SnapshotState chs.version (chainStateSlot os.chainState) chs.confirmedSnapshot chs.seenSnapshot chs.finalizedCommit chs.finalizedDecommit
  transactions = TransactionState chs.localUTxO chs.localTxs chs.allTxs chs.currentDepositTxId

currentState :: Runtime tx -> CoordinatedHeadState tx
currentState (Runtime current _) = current

step :: Runtime tx -> Command tx -> ([StateChanged tx], Runtime tx)
step runtime command = fromRight ([], runtime) $ advance runtime command

advance :: Runtime tx -> Command tx -> Either Text ([StateChanged tx], Runtime tx)
advance (Runtime current machine) command =
  let (Output{snapshots, transactions, decommit, events, refusal}, next) = runIdentity $ run machine command
      updated =
        current
          { version = snapshots.version
          , confirmedSnapshot = snapshots.confirmed
          , seenSnapshot = snapshots.seen
          , finalizedCommit = snapshots.finalizedCommit
          , finalizedDecommit = snapshots.finalizedDecommit
          , localUTxO = transactions.localUTxO
          , localTxs = transactions.localTxs
          , allTxs = transactions.allTxs
          , currentDepositTxId = transactions.currentDepositTxId
          , decommitTx = decommit
          }
   in maybe (Right (events, Runtime updated next)) Left refusal

-- | Staging preserves Program's transaction boundary: a later wait retains the
-- prefix; a rejection discards it. The returned continuation is only tentative.
stage :: Runtime tx -> Command tx -> Program tx ledger signing (Runtime tx)
stage runtime command = case advance runtime command of
  Left message -> Program.reject $ AssertionFailed message
  Right (events, next) -> case (command, events) of
    (Transactions (ApplyValidatedTransaction tx before after), [TransactionAppliedToLocalUTxO headId _]) ->
      Program.recordAppliedTransaction headId tx before after $> next
    _ -> traverse_ Program.change events $> next

receiveTransaction :: tx -> Command tx
receiveTransaction = Transactions . ReceiveTransaction

-- | Keep the accepted request's payload without replacing a local reservation.
-- Confirmation promotes it; the existing TransactionReceived event persists it.
retainSnapshotDecommit :: tx -> Command tx
retainSnapshotDecommit = Transactions . RetainSnapshotDecommit

-- | The live ledger reply supplies the new UTxO; replay reconstructs it from
-- the existing event. Experimental resolved replay uses the same endpoints.
applyTransaction :: tx -> UTxOType tx -> UTxOType tx -> Command tx
applyTransaction tx before after = Transactions $ ApplyValidatedTransaction tx before after

forgetTransaction :: UTxOType tx -> tx -> ValidationError -> Command tx
forgetTransaction utxo tx err = Transactions $ ForgetTransaction utxo tx err

adoptSnapshot :: VerifiedSnapshot tx -> Command tx
adoptSnapshot = AdoptSnapshot

clearLocalState :: SnapshotNumber -> Command tx
clearLocalState = ClearLocalState

settleCommit :: ChainStateType tx -> SnapshotVersion -> TxIdType tx -> UTxOType tx -> Command tx
settleCommit = SettleCommit

settleDecommit :: ChainStateType tx -> SnapshotVersion -> UTxOType tx -> Command tx
settleDecommit = SettleDecommit

reserveDecommit :: tx -> Command tx
reserveDecommit = Transactions . ReserveDecommit

acceptSnapshot :: Snapshot tx -> Seq tx -> Maybe (TxIdType tx) -> Command tx
acceptSnapshot = AcceptSnapshot

recordSignature :: SnapshotNumber -> Party -> Signature (Snapshot tx) -> Command tx
recordSignature = RecordSignature

-- | The token binds the signatures to the candidate verified by the rule.
confirmSnapshot :: VerifiedSnapshot tx -> Command tx
confirmSnapshot = ConfirmSnapshot

requestSnapshot :: SnapshotRequest tx -> Command tx
requestSnapshot = RequestSnapshot

-- | Trusted replay adapter. The registry supplies the deposited UTxO before
-- consumption; protocol admission and cryptography have already happened.
-- Replay commands stay private: live rules cannot stage unverified evidence.
replayEvent :: IsChainState tx => UTxOType tx -> StateChanged tx -> Runtime tx -> Runtime tx
replayEvent deposited event runtime = maybe runtime (snd . step runtime) $ fromEvent deposited event

fromEvent :: IsChainState tx => UTxOType tx -> StateChanged tx -> Maybe (Command tx)
fromEvent deposited = \case
  TransactionReceived tx -> Just $ Transactions $ ReceiveTransaction tx
  TransactionAppliedToLocalUTxO _ tx -> Just $ Transactions $ ApplyTransaction tx
  TxInvalid _ utxo tx err -> Just $ Transactions $ ForgetTransaction utxo tx err
  DecommitRecorded _ tx -> Just $ Transactions $ ReplayDecommit tx
  SnapshotRequestDecided number -> Just $ ReplaySnapshotRequest number
  SnapshotRequested snapshot txs deposit -> Just $ acceptSnapshot snapshot txs deposit
  PartySignedSnapshot number party signature -> Just $ recordSignature number party signature
  SnapshotConfirmed _ Nothing signatures -> Just $ ReplayConfirmation signatures
  SnapshotConfirmed _ (Just snapshot) signatures -> Just $ ReplayAdoption snapshot signatures
  LocalStateCleared _ number -> Just $ ClearLocalState number
  DepositActivated depositId chainTime deposit -> Just $ ActivateDeposit depositId chainTime deposit
  DepositRecovered chainState headId depositId recovered -> Just $ RecoverDeposit chainState headId depositId recovered
  CommitFinalized{chainState, newVersion, depositTxId} -> Just $ SettleCommit chainState newVersion depositTxId deposited
  DecommitFinalized{chainState, newVersion, distributedUTxO} -> Just $ SettleDecommit chainState newVersion distributedUTxO
  ChainRolledBack{chainState} -> Just $ Rollback $ chainStateSlot chainState
  _ -> Nothing
