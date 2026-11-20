{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.State.Coordinated.Transactions (
  TransactionState (..),
  Output (..),
  DecommitPhase (..),
  DecommitTopology,
  NoDecommitSym0,
  DecommitPendingSym0,
  AwaitingDecommitSettlementSym0,
  machine,
) where

import Crem.BaseMachine (
  ActionResult,
  BaseMachineT (..),
  InitialState (..),
  pureResult,
  withStateInspector,
 )
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.StateInspection (describeOnly)
import Crem.Topology (AllowedTransition, Topology (Topology))
import Data.Map.Strict qualified as Map (
  delete,
  elems,
  insert,
  lookup,
 )
import Data.Sequence qualified as Seq (take, (|>))
import Data.Singletons.Base.TH (genSingletons)
import Hydra.API.ServerOutput (
  DecommitInvalidReason (DecommitTxInvalid),
 )
import Hydra.HeadLogic.Deposit (Deposit (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Snapshot.Request (maxTxsPerSnapshot)
import Hydra.HeadLogic.Snapshot.Settlement (
  finalizedDepositTxId,
  matchesSnapshotDecommit,
  settledUTxO,
  settlingDecommit,
 )
import Hydra.HeadLogic.State (SeenSnapshot (..))
import Hydra.HeadLogic.State.Coordinated.Snapshots.State (
  SnapshotOutput (..),
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.State qualified as Snapshots (
  State,
  viewInitial,
 )
import Hydra.HeadLogic.State.Types (
  CommitUpdate (..),
  LocalTransactionCommand (..),
  SnapshotState (..),
  TransactionUpdate (..),
  VersionChange (..),
 )
import Hydra.HeadLogic.Transaction.Outputs (absorbable)
import Hydra.Ledger (ValidationError (ValidationError))
import Hydra.Prelude hiding (State, local, state)
import Hydra.Tx (HeadId, IsTx (..))
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  getSnapshot,
 )

-- | An approved decommit must settle before a new pending one can replace it.
data DecommitPhase = NoDecommit | DecommitPending | AwaitingDecommitSettlement

$(genSingletons [''DecommitPhase])

type DecommitTopology =
  'Topology
    '[ '( 'NoDecommit, '[ 'DecommitPending, 'AwaitingDecommitSettlement])
     , '( 'DecommitPending, '[ 'NoDecommit, 'AwaitingDecommitSettlement])
     , '( 'AwaitingDecommitSettlement, '[ 'NoDecommit])
     ]

deriving stock instance Eq DecommitPhase
deriving stock instance Show DecommitPhase
deriving stock instance Enum DecommitPhase
deriving stock instance Bounded DecommitPhase
deriving via AllVertices DecommitPhase instance RenderableVertices DecommitPhase

data TransactionState tx = TransactionState
  { localUTxO :: UTxOType tx
  , localTxs :: Seq tx
  , allTxs :: Map (TxIdType tx) tx
  }

data State tx phase where
  Available :: ~(TransactionState tx) -> State tx 'NoDecommit
  Pending :: ~(TransactionState tx) -> tx -> State tx 'DecommitPending
  Settling :: ~(TransactionState tx) -> Maybe tx -> State tx 'AwaitingDecommitSettlement

data Output tx = Output
  { snapshots :: InitialState (Snapshots.State tx)
  , transactions :: ~(TransactionState tx)
  , decommit :: Maybe tx
  , commitUpdate :: CommitUpdate tx
  , events :: [StateChanged tx]
  , refusal :: Maybe Text
  }

statePhase :: State tx phase -> DecommitPhase
statePhase = \case
  Available{} -> NoDecommit
  Pending{} -> DecommitPending
  Settling{} -> AwaitingDecommitSettlement

machine ::
  forall tx m.
  (IsTx tx, Applicative m) =>
  HeadId ->
  SnapshotState tx ->
  TransactionState tx ->
  Maybe tx ->
  BaseMachineT m DecommitTopology (SnapshotOutput tx) (Output tx)
machine headId snapshots0 initial initialDecommit =
  BaseMachineT (withStateInspector (describeOnly (show . statePhase)) initialState) action
 where
  initialState
    | settling snapshots0 = InitialState $ Settling initial initialDecommit
    | otherwise = maybe (InitialState $ Available initial) (InitialState . Pending initial) initialDecommit

  action ::
    forall from.
    State tx from ->
    SnapshotOutput tx ->
    ActionResult m DecommitTopology (State tx) from (Output tx)
  action state input = case state of
    Available local -> case input.transactionUpdate of
      RunLocalTransactionCommand (RecordPending tx) -> recordPending local tx input
      ReplayPendingDecommit tx -> recordPending local tx input
      _ -> update local Nothing Available input
    Pending local tx -> case input.transactionUpdate of
      ReplayPendingDecommit recorded -> replayPending local recorded Pending input
      _ -> update local (Just tx) (`Pending` tx) input
    Settling local tx -> case input.transactionUpdate of
      ReplayPendingDecommit recorded ->
        replayPending local recorded (\next -> Settling next . Just) input
      _ -> update local tx (`Settling` tx) input

  replayPending ::
    TransactionState tx ->
    tx ->
    (TransactionState tx -> tx -> State tx from) ->
    SnapshotOutput tx ->
    ActionResult m DecommitTopology (State tx) from (Output tx)
  replayPending local tx keep input =
    let next = reserveUTxO local tx
     in output
          input.snapshots
          input.refusal
          next
          (Just tx)
          CommitUnchanged
          (keep next tx)
          (input.events <> [DecommitRecorded headId tx])

  reserveUTxO :: TransactionState tx -> tx -> TransactionState tx
  reserveUTxO local tx = local{localUTxO = applyTxTo tx local.localUTxO `withoutUTxO` utxoFromTx tx}

  recordPending ::
    TransactionState tx ->
    tx ->
    SnapshotOutput tx ->
    ActionResult m DecommitTopology (State tx) 'NoDecommit (Output tx)
  recordPending local tx input =
    let next = reserveUTxO local tx
     in output
          input.snapshots
          input.refusal
          next
          (Just tx)
          CommitUnchanged
          (Pending next tx)
          (input.events <> [DecommitRecorded headId tx])

  output ::
    AllowedTransition DecommitTopology from to =>
    InitialState (Snapshots.State tx) ->
    Maybe Text ->
    TransactionState tx ->
    Maybe tx ->
    CommitUpdate tx ->
    State tx to ->
    [StateChanged tx] ->
    ActionResult m DecommitTopology (State tx) from (Output tx)
  output snapshots refusal transactions decommit commitUpdate next events =
    pureResult Output{..} next

  update ::
    ( AllowedTransition DecommitTopology from 'NoDecommit
    , AllowedTransition DecommitTopology from 'AwaitingDecommitSettlement
    ) =>
    TransactionState tx ->
    Maybe tx ->
    (TransactionState tx -> State tx from) ->
    SnapshotOutput tx ->
    ActionResult m DecommitTopology (State tx) from (Output tx)
  update local reserved keep SnapshotOutput{snapshots = owned, transactionUpdate, events, refusal} =
    case transactionUpdate of
      TransactionsUnchanged -> unchanged local
      RebuildFromSnapshot snapshot txs deposit ->
        let next =
              local
                { localUTxO = reserveInputs $ rebuild snapshot txs
                , localTxs = txs
                , allTxs = foldr (Map.delete . txId) local.allTxs snapshot.confirmed
                }
         in changeCommit (SetDepositSelection deposit) next
      ReconcileConfirmedSnapshot -> case reserved of
        Nothing
          | settling snapshots ->
              output owned refusal confirmedLocal authorised RecomputeCommitPhase (Settling confirmedLocal authorised) events
          | otherwise ->
              changeCommit RecomputeCommitPhase confirmedLocal
        Just tx
          | carries tx -> output owned refusal confirmedLocal reserved RecomputeCommitPhase (Settling confirmedLocal reserved) events
          | not (settling snapshots) && txInputsAvailable tx rebuilt -> changeCommit RecomputeCommitPhase confirmedLocal
          | otherwise ->
              let next = confirmedLocal{localUTxO = rebuilt}
                  invalid =
                    DecommitInvalid headId tx $
                      DecommitTxInvalid rebuilt (ValidationError "decommit superseded by confirmed snapshot")
               in if settling snapshots
                    then output owned refusal next authorised RecomputeCommitPhase (Settling next authorised) (events <> [invalid])
                    else output owned refusal next Nothing RecomputeCommitPhase (Available next) (events <> [invalid])
      ResetTransactionState -> case snapshots.confirmed of
        InitialSnapshot{} -> unchanged local{localUTxO = mempty, localTxs = mempty, allTxs = mempty}
        ConfirmedSnapshot{snapshot} ->
          let next = TransactionState (settledUTxO snapshots.version snapshot) mempty mempty
           in if settling snapshots
                then output owned refusal next authorised (SetDepositSelection Nothing) (Settling next authorised) events
                else output owned refusal next Nothing (SetDepositSelection Nothing) (Available next) events
      TrySelectDeposit depositId deposit
        | deposit.headId == headId
        , isNothing reserved
        , finalizedDepositTxId snapshots.finalizedCommit /= Just depositId
        , absorbable requestUTxO (toList $ Seq.take maxTxsPerSnapshot local.localTxs) deposit.deposited ->
            changeCommit (SelectDepositIfUnselected depositId) local
        | otherwise -> unchanged local
      ClearDepositSelection depositId -> changeCommit (DeselectDeposit depositId) local
      CommitSettled VersionAdvanced _ deposited ->
        changeCommit (SetDepositSelection Nothing) local{localUTxO = local.localUTxO <> deposited}
      CommitSettled VersionNotAdvanced depositId _ ->
        changeCommit (DeselectDeposit depositId) local
      DecommitSettled VersionAdvanced ->
        output owned refusal local Nothing RecomputeCommitPhase (Available local) events
      DecommitSettled VersionNotAdvanced -> unchanged local
      ReplayTransactionApplication tx ->
        changed local{localUTxO = applyTxTo tx local.localUTxO, localTxs = local.localTxs Seq.|> tx} $ TransactionAppliedToLocalUTxO headId tx
      ReplayPendingDecommit{} -> unchanged local
      RunLocalTransactionCommand command -> case command of
        ReceiveTransaction tx ->
          changed local{allTxs = Map.insert (txId tx) tx local.allTxs} $ TransactionReceived tx
        RetainSnapshotDecommit tx
          | any ((== utxoFromTx tx) . utxoFromTx) reserved || Map.lookup (txId tx) local.allTxs == Just tx -> unchanged local
          | otherwise -> changed local{allTxs = Map.insert (txId tx) tx local.allTxs} $ TransactionReceived tx
        ApplyValidatedTransaction tx _ after ->
          changed local{localUTxO = after, localTxs = local.localTxs Seq.|> tx} $ TransactionAppliedToLocalUTxO headId tx
        MarkTransactionInvalid utxo tx err ->
          changed local{allTxs = Map.delete (txId tx) local.allTxs} $ TxInvalid headId utxo tx err
        RecordPending{} ->
          output owned (Just "a decommit is already pending or awaiting settlement") local reserved CommitUnchanged (keep local) []
   where
    snapshots = Snapshots.viewInitial owned

    -- A decommit can depend on a retained transaction. Rebuild first, then
    -- reserve its spending inputs, even when this round did not carry it.
    reserveInputs = maybe id withoutTxInputs reserved
    rebuild snapshot = foldl' (flip applyTxTo) (settledUTxO snapshots.version snapshot)
    rebuilt = rebuild (getSnapshot snapshots.confirmed) local.localTxs
    carries tx = settling snapshots && matchesSnapshotDecommit (getSnapshot snapshots.confirmed) tx

    -- Prefer the local reservation; a missing or different reservation needs a
    -- registry search. A later version retires the payload without promoting it.
    payload = case (getSnapshot snapshots.confirmed).utxoToDecommit of
      Just outputs
        | outputs /= mempty ->
            find (matchesSnapshotDecommit $ getSnapshot snapshots.confirmed) reserved
              <|> find (matchesSnapshotDecommit $ getSnapshot snapshots.confirmed) (Map.elems local.allTxs)
      _ -> Nothing

    authorised = payload <* guard (settling snapshots)

    confirmedLocal = maybe local (\tx -> local{allTxs = Map.delete (txId tx) local.allTxs}) payload

    requestUTxO = settledUTxO snapshots.version $ case snapshots.seen of
      SeenSnapshot{snapshot} -> snapshot
      _ -> getSnapshot snapshots.confirmed

    unchanged = changeCommit CommitUnchanged

    changeCommit commitUpdate next = output owned refusal next reserved commitUpdate (keep next) events

    changed next event = output owned refusal next reserved CommitUnchanged (keep next) (events <> [event])

-- | A fresh version retires the previous version's incremental certificate.
settling :: IsTx tx => SnapshotState tx -> Bool
settling snapshots =
  settlingDecommit snapshots.version snapshots.confirmed
