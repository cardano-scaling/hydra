{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Own the selected deposit and its relation to the confirmed certificate.
-- Registry status and the settlement window determine request eligibility in
-- SnapshotRequest; neither expiry nor recovery erases a signed certificate.
module Hydra.HeadLogic.Machine.Coordinated.Commits (Output (..), machine) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (
  ActionResult,
  BaseMachineT (..),
  InitialState (..),
  pureResult,
  withStateInspector,
 )
import Crem.StateInspection (describeOnly)
import Crem.Topology (AllowedTransition)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State qualified as Snapshots
import Hydra.HeadLogic.Machine.Coordinated.Topology (CommitPhase (..), CommitTopology)
import Hydra.HeadLogic.Machine.Coordinated.Transactions qualified as Transactions
import Hydra.HeadLogic.Machine.Coordinated.Types (CommitUpdate (..), SnapshotState (..))
import Hydra.Tx (IsTx, TxIdType)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data State tx phase where
  -- | No deposit is selected and no current-version snapshot approves a commit.
  Unselected :: State tx 'NoCommit
  -- | A deposit is selected for snapshot processing; approval is still pending.
  Selected :: TxIdType tx -> State tx 'CommitSelected
  -- | The current-version confirmed snapshot approves a commit. The optional id
  -- records the local selection, which can be absent or name another deposit.
  -- Recovery can prevent settlement while the signed approval is still retained.
  Approved :: Maybe (TxIdType tx) -> State tx 'AwaitingCommitSettlement

data Output tx = Output
  { transactions :: Transactions.Output tx
  , currentDepositTxId :: Maybe (TxIdType tx)
  }

statePhase :: State tx phase -> CommitPhase
statePhase = \case
  Unselected -> NoCommit
  Selected{} -> CommitSelected
  Approved{} -> AwaitingCommitSettlement

selection :: State tx phase -> Maybe (TxIdType tx)
selection = \case
  Unselected -> Nothing
  Selected depositId -> Just depositId
  Approved selected -> selected

machine ::
  forall tx m.
  (IsTx tx, Applicative m) =>
  SnapshotState tx ->
  Maybe (TxIdType tx) ->
  BaseMachineT m CommitTopology (Transactions.Output tx) (Output tx)
machine initialSnapshots initialSelection =
  BaseMachineT (withStateInspector (describeOnly (show . statePhase)) initialState) action
 where
  initialState
    | hasCurrentCommitApproval initialSnapshots = InitialState $ Approved initialSelection
    | otherwise = maybe (InitialState Unselected) (InitialState . Selected) initialSelection

  -- Note: We need to help poor Haskell with type inference here, which is why
  -- this function can't be simply removed even though it's a trivial
  -- operation.
  --
  -- See also: "Total Type Classes"
  --   * https://spiral.imperial.ac.uk/server/api/core/bitstreams/156e9c88-3021-4886-b222-38f0c10c7857/content
  action ::
    State tx from ->
    Transactions.Output tx ->
    ActionResult m CommitTopology (State tx) from (Output tx)
  action state input = case state of
    Unselected -> update state input
    Selected{} -> update state input
    Approved{} -> update state input

  update ::
    forall from.
    ( AllowedTransition CommitTopology from 'NoCommit
    , AllowedTransition CommitTopology from 'CommitSelected
    , AllowedTransition CommitTopology from 'AwaitingCommitSettlement
    ) =>
    State tx from ->
    Transactions.Output tx ->
    ActionResult m CommitTopology (State tx) from (Output tx)
  update state input = case input.commitUpdate of
    CommitUnchanged -> output state
    RecomputeCommitPhase ->
      updatePhase updatedSnapshots currentSelection
    SelectDepositIfUnselected depositId ->
      updatePhase updatedSnapshots $ currentSelection <|> Just depositId
    SetDepositSelection nextSelection ->
      updatePhase updatedSnapshots nextSelection
    DeselectDeposit depositId ->
      updatePhase updatedSnapshots $ mfilter (/= depositId) currentSelection
   where
    currentSelection = selection state
    updatedSnapshots = Snapshots.viewInitial input.snapshots

    updatePhase ::
      SnapshotState tx ->
      Maybe (TxIdType tx) ->
      ActionResult m CommitTopology (State tx) from (Output tx)
    updatePhase snapshots nextSelection
      | hasCurrentCommitApproval snapshots = output $ Approved nextSelection
      | otherwise = maybe (output Unselected) (output . Selected) nextSelection

    output ::
      AllowedTransition CommitTopology from to =>
      State tx to ->
      ActionResult m CommitTopology (State tx) from (Output tx)
    output next = pureResult (Output input $ selection next) next

hasCurrentCommitApproval :: IsTx tx => SnapshotState tx -> Bool
hasCurrentCommitApproval snapshots =
  let snapshot = getSnapshot snapshots.confirmed
   in snapshot.version == snapshots.version && isJust snapshot.utxoToCommit
