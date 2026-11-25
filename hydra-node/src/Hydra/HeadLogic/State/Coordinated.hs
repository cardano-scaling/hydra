{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.State.Coordinated (
  Runtime,
  DecommitPhase (..),
  DecommitTopology,
  Command,
  Candidate,
  candidate,
  start,
  step,
  currentState,
  advance,
  recordDecommit,
  acceptSnapshot,
  recordSignature,
  confirmSnapshot,
  requestSnapshot,
  receiveTransaction,
  retainSnapshotDecommit,
  applyTransaction,
  markTransactionInvalid,
  adoptSnapshot,
  clearLocalState,
  recordCommitFinalization,
  recordDecommitFinalization,
  replayEvent,
  inspect,
) where

import Crem.BaseMachine (InitialState, withInitialState)
import Crem.Inspect qualified as Crem (inspectStates)
import Crem.StateMachine (
  StateMachineT (..),
  run,
 )
import Hydra.Chain.ChainState (
  IsChainState (ChainStateType),
  chainStateSlot,
 )
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Snapshot.Candidate (Candidate)
import Hydra.HeadLogic.Snapshot.Request (SnapshotRequest)
import Hydra.HeadLogic.Snapshot.Verification (
  VerifiedCandidate,
  VerifiedSnapshot,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (..),
  OpenState (..),
 )
import Hydra.HeadLogic.State.Coordinated.Commits qualified as Commits (
  Output (..),
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Replay qualified as Replay (
  replay,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots qualified as Snapshots (
  candidate,
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.State qualified as State (
  SnapshotOutput (..),
  State,
  Update (..),
  mkSnapshotOutput,
  restore,
  viewInitial,
 )
import Hydra.HeadLogic.State.Coordinated.Transactions (
  DecommitPhase (..),
  DecommitTopology,
  Output (..),
  TransactionState (..),
 )
import Hydra.HeadLogic.State.Coordinated.Transactions qualified as Transactions (
  Output,
  machine,
 )
import Hydra.HeadLogic.State.Types (
  Command (..),
  LocalTransactionCommand (..),
  SnapshotCommand (..),
  SnapshotState (..),
  TransactionUpdate (..),
 )
import Hydra.HeadLogic.Validation (ValidatedDecommit, ValidatedSnapshot, ValidatedTransaction)
import Hydra.Ledger (ValidationError)
import Hydra.Prelude
import Hydra.Tx (HeadId, TxIdType, UTxOType)
import Hydra.Tx.Crypto (HydraKey, Signature, VerificationKey)
import Hydra.Tx.HeadParameters (HeadParameters (parties))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
 )

data Runtime tx = Runtime
  { headId :: HeadId
  , parties :: [VerificationKey HydraKey]
  , coordinatedState :: ~(CoordinatedHeadState tx)
  , snapshots :: InitialState (State.State tx)
  , snapshotMachine :: ~(StateMachineT Identity (SnapshotCommand tx) (State.SnapshotOutput tx))
  , transactionMachine :: ~(StateMachineT Identity (State.SnapshotOutput tx) (Transactions.Output tx))
  , commitMachine :: ~(StateMachineT Identity (Transactions.Output tx) (Commits.Output tx))
  }

start :: IsChainState tx => OpenState tx -> Runtime tx
start os =
  Runtime
    { headId = os.headId
    , parties
    , coordinatedState = os.coordinatedHeadState
    , snapshots
    , snapshotMachine = Executable $ Snapshots.machine os.headId parties snapshots
    , transactionMachine = Executable $ Transactions.machine os.headId snapshotState (transactionState os.coordinatedHeadState) os.coordinatedHeadState.decommitTx
    , commitMachine = Executable $ Commits.machine snapshotState os.coordinatedHeadState.currentDepositTxId
    }
 where
  parties = vkey <$> os.parameters.parties
  snapshots = restoreSnapshots parties os
  snapshotState = State.viewInitial snapshots

-- | Inspect each owner without executing commands or forcing the local ledger.
inspect :: Runtime tx -> [(Text, Maybe Text)]
inspect Runtime{..} =
  [("Snapshots", description) | (_, description) <- Crem.inspectStates snapshotMachine]
    <> [("Transactions", description) | (_, description) <- Crem.inspectStates transactionMachine]
    <> [("Commits", description) | (_, description) <- Crem.inspectStates commitMachine]

restoreSnapshots ::
  IsChainState tx =>
  [VerificationKey HydraKey] ->
  OpenState tx ->
  InitialState (State.State tx)
restoreSnapshots parties os =
  State.restore parties $
    SnapshotState
      { chainSlot = chainStateSlot os.chainState
      , confirmed = confirmedSnapshot
      , seen = seenSnapshot
      , ..
      }
 where
  CoordinatedHeadState{..} = os.coordinatedHeadState

transactionState :: CoordinatedHeadState tx -> TransactionState tx
transactionState chs = TransactionState{..}
 where
  CoordinatedHeadState{..} = chs

currentState :: Runtime tx -> CoordinatedHeadState tx
currentState Runtime{coordinatedState} = coordinatedState

candidate :: Runtime tx -> Maybe (Candidate tx)
candidate Runtime{headId, snapshots} = Snapshots.candidate headId snapshots

step :: Runtime tx -> Command tx -> ([StateChanged tx], Runtime tx)
step runtime command = fromRight ([], runtime) $ advance runtime command

advance :: Runtime tx -> Command tx -> Either Text ([StateChanged tx], Runtime tx)
advance runtime@Runtime{..} command =
  let (input, nextSnapshots) = case command of
        Transactions transaction ->
          let update = withInitialState snapshots $ \snapshotState _ ->
                State.mkSnapshotOutput $ State.Update snapshotState (RunLocalTransactionCommand transaction) []
           in (update, snapshotMachine)
        Snapshots snapshot ->
          runIdentity $ run snapshotMachine snapshot
      (output, next) = applyUpdates input runtime{snapshotMachine = nextSnapshots}
   in maybe (Right (output.transactions.events, next)) Left output.transactions.refusal

-- | Reconcile transactions against the updated snapshot before changing commit
-- selection. Both live commands and retained events use this ordering.
applyUpdates :: State.SnapshotOutput tx -> Runtime tx -> (Commits.Output tx, Runtime tx)
applyUpdates input runtime@Runtime{..} =
  let (transactions, nextTransactions) = runIdentity $ run transactionMachine input
      (output, nextCommits) = runIdentity $ run commitMachine transactions
      updated =
        runtime
          { coordinatedState = project output
          , snapshots = transactions.snapshots
          , transactionMachine = nextTransactions
          , commitMachine = nextCommits
          }
   in (output, updated)

project :: Commits.Output tx -> CoordinatedHeadState tx
project Commits.Output{transactions = Output{snapshots = owned, transactions, decommit}, currentDepositTxId} =
  CoordinatedHeadState
    { confirmedSnapshot = confirmed
    , seenSnapshot = seen
    , decommitTx = decommit
    , ..
    }
 where
  SnapshotState{..} = State.viewInitial owned
  TransactionState{..} = transactions

receiveTransaction :: tx -> Command tx
receiveTransaction = Transactions . ReceiveTransaction

retainSnapshotDecommit :: tx -> Command tx
retainSnapshotDecommit = Transactions . RetainSnapshotDecommit

applyTransaction :: ValidatedTransaction tx -> Command tx
applyTransaction = Transactions . ApplyValidatedTransaction

markTransactionInvalid :: UTxOType tx -> tx -> ValidationError -> Command tx
markTransactionInvalid utxo tx err = Transactions $ MarkTransactionInvalid utxo tx err

adoptSnapshot :: VerifiedSnapshot tx -> Command tx
adoptSnapshot = Snapshots . AdoptSnapshot

clearLocalState :: SnapshotNumber -> Command tx
clearLocalState = Snapshots . ClearLocalState

recordCommitFinalization ::
  ChainStateType tx ->
  SnapshotVersion ->
  TxIdType tx ->
  UTxOType tx ->
  Command tx
recordCommitFinalization chainState version depositId deposited = Snapshots $ RecordCommitFinalization chainState version depositId deposited

recordDecommitFinalization ::
  ChainStateType tx ->
  SnapshotVersion ->
  UTxOType tx ->
  Command tx
recordDecommitFinalization chainState version distributed = Snapshots $ RecordDecommitFinalization chainState version distributed

recordDecommit :: ValidatedDecommit tx -> Command tx
recordDecommit = Transactions . RecordPending

acceptSnapshot :: ValidatedSnapshot tx -> Seq tx -> Command tx
acceptSnapshot snapshot txs = Snapshots $ AcceptSnapshot snapshot txs

recordSignature :: Candidate tx -> Party -> Signature (Snapshot tx) -> Command tx
recordSignature owned party signature = Snapshots $ RecordSignature owned party signature

confirmSnapshot :: VerifiedCandidate tx -> Command tx
confirmSnapshot = Snapshots . ConfirmSnapshot

requestSnapshot :: SnapshotRequest tx -> Command tx
requestSnapshot = Snapshots . RequestSnapshot

replayEvent ::
  IsChainState tx =>
  UTxOType tx ->
  StateChanged tx ->
  Runtime tx ->
  Runtime tx
replayEvent deposited event runtime@Runtime{headId, parties, snapshots} =
  case Replay.replay headId parties snapshots deposited event of
    Nothing -> runtime
    Just restored ->
      snd $ applyUpdates restored runtime{snapshotMachine = Executable $ Snapshots.machine headId parties restored.snapshots}
