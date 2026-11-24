{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.State.Coordinated.Commits (Output (..), machine) where

import Crem.BaseMachine (
  BaseMachineT (..),
  InitialState (..),
  pureResult,
 )
import Crem.StateInspection (describeOnly)
import Crem.Topology (TrivialTopology)
import Hydra.HeadLogic.State.Coordinated.Snapshots.State qualified as Snapshots (
  viewInitial,
 )
import Hydra.HeadLogic.State.Coordinated.Transactions qualified as Transactions (
  Output (commitUpdate, snapshots),
 )
import Hydra.HeadLogic.State.Types (
  CommitUpdate (..),
  SnapshotState (..),
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, TxIdType)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data CommitState tx
  = Unselected
  | Selected (TxIdType tx)
  | Approved (Maybe (TxIdType tx))

data CommitPhase = NoCommit | CommitSelected | AwaitingCommitSettlement
  deriving stock (Eq, Show)

data Owned tx phase where
  Owned :: CommitState tx -> Owned tx '()

data Output tx = Output
  { transactions :: Transactions.Output tx
  , currentDepositTxId :: Maybe (TxIdType tx)
  }

phase :: CommitState tx -> CommitPhase
phase = \case
  Unselected -> NoCommit
  Selected{} -> CommitSelected
  Approved{} -> AwaitingCommitSettlement

selection :: CommitState tx -> Maybe (TxIdType tx)
selection = \case
  Unselected -> Nothing
  Selected depositId -> Just depositId
  Approved selected -> selected

machine ::
  (IsTx tx, Applicative m) =>
  SnapshotState tx ->
  Maybe (TxIdType tx) ->
  BaseMachineT m (TrivialTopology @()) (Transactions.Output tx) (Output tx)
machine snapshots selected =
  BaseMachineT (InspectableState (describeOnly $ \(Owned owned) -> show $ phase owned) $ Owned $ classify snapshots selected) $
    \(Owned owned) input ->
      let next = update (Snapshots.viewInitial input.snapshots) input.commitUpdate owned
       in pureResult (Output input $ selection next) (Owned next)

update :: IsTx tx => SnapshotState tx -> CommitUpdate tx -> CommitState tx -> CommitState tx
update snapshots change owned = case change of
  CommitUnchanged -> owned
  RecomputeCommitPhase -> classify snapshots selected
  SelectDepositIfUnselected depositId -> classify snapshots $ selected <|> Just depositId
  SetDepositSelection next -> classify snapshots next
  DeselectDeposit depositId -> classify snapshots $ mfilter (/= depositId) selected
 where
  selected = selection owned

classify :: IsTx tx => SnapshotState tx -> Maybe (TxIdType tx) -> CommitState tx
classify snapshots selected
  | snapshot.version == snapshots.version && isJust snapshot.utxoToCommit = Approved selected
  | otherwise = maybe Unselected Selected selected
 where
  snapshot = getSnapshot snapshots.confirmed
