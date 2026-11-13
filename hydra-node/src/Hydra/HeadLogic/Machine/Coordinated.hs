{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Compose snapshot coordination and transaction ownership. The command port
-- is separate from persistence; accepted commands produce the existing events.
module Hydra.HeadLogic.Machine.Coordinated (
  Runtime,
  Command,
  Candidate,
  candidate,
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

import Crem.BaseMachine (InitialState)
import Crem.StateMachine (StateMachineT (..), run, withoutRendering)
import Hydra.Chain.ChainState (IsChainState (ChainStateType), chainStateSlot)
import Hydra.HeadLogic.Error (LogicError (AssertionFailed))
import Hydra.HeadLogic.Machine.Coordinated.Candidate (Candidate)
import Hydra.HeadLogic.Machine.Coordinated.Commits qualified as Commits
import Hydra.HeadLogic.Machine.Coordinated.Snapshots qualified as Snapshots
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.Replay qualified as Replay
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State qualified as State
import Hydra.HeadLogic.Machine.Coordinated.Transactions (Output (..), TransactionState (..))
import Hydra.HeadLogic.Machine.Coordinated.Transactions qualified as Transactions
import Hydra.HeadLogic.Machine.Coordinated.Types (Command (..), SnapshotState (..), TransactionCommand (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotRequest (SnapshotRequest)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedCandidate, VerifiedSnapshot)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), OpenState (..))
import Hydra.Ledger (ValidationError)
import Hydra.Tx (HeadId, TxIdType, UTxOType)
import Hydra.Tx.Crypto (HydraKey, Signature, VerificationKey)
import Hydra.Tx.HeadParameters (HeadParameters (parties))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (Snapshot, SnapshotNumber, SnapshotVersion)

-- Staging can carry the continuation without reading its persisted projection.
-- Forcing that projection would repeat ledger work just to emit the next event.
-- Recovery reads the projection without running live commands, so constructing
-- the next live composition is also deferred until a command needs it.
data Runtime tx = Runtime HeadId [VerificationKey HydraKey] ~(CoordinatedHeadState tx) (InitialState (State.State tx)) ~(StateMachineT Identity (Command tx) (Commits.Output tx))

start :: IsChainState tx => OpenState tx -> Runtime tx
start os = Runtime os.headId parties os.coordinatedHeadState snapshots (withoutRendering $ compose os.headId parties snapshots os.coordinatedHeadState)
 where
  parties = vkey <$> os.parameters.parties
  snapshots = restoreSnapshots parties os

-- | Inspect the same composition used by live execution, with rendering enabled.
components :: IsChainState tx => OpenState tx -> StateMachineT Identity (Command tx) (Commits.Output tx)
components os = compose os.headId parties (restoreSnapshots parties os) os.coordinatedHeadState
 where
  parties = vkey <$> os.parameters.parties

compose :: IsChainState tx => HeadId -> [VerificationKey HydraKey] -> InitialState (State.State tx) -> CoordinatedHeadState tx -> StateMachineT Identity (Command tx) (Commits.Output tx)
compose headId parties snapshots chs =
  Basic (Snapshots.machine headId parties snapshots)
    `Sequential` Basic (Transactions.machine headId (State.viewInitial snapshots) (transactionState chs) chs.decommitTx)
    `Sequential` Basic (Commits.machine (State.viewInitial snapshots) chs.currentDepositTxId)

restoreSnapshots :: IsChainState tx => [VerificationKey HydraKey] -> OpenState tx -> InitialState (State.State tx)
restoreSnapshots parties os = State.restore parties $ SnapshotState chs.version (chainStateSlot os.chainState) chs.confirmedSnapshot chs.seenSnapshot chs.finalizedCommit chs.finalizedDecommit
 where
  chs = os.coordinatedHeadState

transactionState :: CoordinatedHeadState tx -> TransactionState tx
transactionState chs = TransactionState chs.localUTxO chs.localTxs chs.allTxs

currentState :: Runtime tx -> CoordinatedHeadState tx
currentState (Runtime _ _ current _ _) = current

-- | Obtain the owner's current candidate. Its constructor and cached bytes are
-- inaccessible to rule callers; a restored candidate derives its bytes afresh.
candidate :: Runtime tx -> Maybe (Candidate tx)
candidate (Runtime headId _ _ snapshots _) = Snapshots.candidate headId snapshots

step :: Runtime tx -> Command tx -> ([StateChanged tx], Runtime tx)
step runtime command = fromRight ([], runtime) $ advance runtime command

advance :: Runtime tx -> Command tx -> Either Text ([StateChanged tx], Runtime tx)
advance (Runtime headId parties _ _ machine) command =
  let (output@Commits.Output{transactions = Output{snapshots, events, refusal}}, next) = runIdentity $ run machine command
   in maybe (Right (events, Runtime headId parties (project output) snapshots next)) Left refusal

project :: Commits.Output tx -> CoordinatedHeadState tx
project Commits.Output{transactions = Output{snapshots = owned, transactions, decommit}, currentDepositTxId} =
  CoordinatedHeadState
    { version = snapshots.version
    , confirmedSnapshot = snapshots.confirmed
    , seenSnapshot = snapshots.seen
    , finalizedCommit = snapshots.finalizedCommit
    , finalizedDecommit = snapshots.finalizedDecommit
    , localUTxO = transactions.localUTxO
    , localTxs = transactions.localTxs
    , allTxs = transactions.allTxs
    , currentDepositTxId
    , decommitTx = decommit
    }
 where
  snapshots = State.viewInitial owned

-- | Staging preserves Program's transaction boundary: a later wait retains the
-- prefix; a rejection discards it. The returned continuation is only tentative.
stage :: Runtime tx -> Command tx -> Program tx (Runtime tx)
stage runtime command = case advance runtime command of
  Left message -> Program.reject $ AssertionFailed message
  Right (events, next) -> traverse_ Program.change events $> next

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

recordSignature :: Candidate tx -> Party -> Signature (Snapshot tx) -> Command tx
recordSignature = RecordSignature

-- | The token binds the signatures to the candidate verified by the rule.
confirmSnapshot :: VerifiedCandidate tx -> Command tx
confirmSnapshot = ConfirmSnapshot

requestSnapshot :: SnapshotRequest tx -> Command tx
requestSnapshot = RequestSnapshot

-- | Trusted replay adapter. The registry supplies the deposited UTxO before
-- consumption; protocol admission and cryptography have already happened.
-- Replay commands stay private: live rules cannot stage unverified evidence.
replayEvent :: IsChainState tx => UTxOType tx -> StateChanged tx -> Runtime tx -> Runtime tx
replayEvent deposited event runtime@(Runtime headId parties current snapshots _) =
  case Replay.replay headId parties snapshots deposited event of
    Nothing -> runtime
    Just restored ->
      let transactions =
            Executable (Transactions.machine headId (State.viewInitial snapshots) (transactionState current) current.decommitTx)
              `Sequential` Executable (Commits.machine (State.viewInitial snapshots) current.currentDepositTxId)
          (output, _) = runIdentity $ run transactions restored
          updated = project output
          nextSnapshots = output.transactions.snapshots
       in Runtime headId parties updated nextSnapshots (withoutRendering $ compose headId parties nextSnapshots updated)
