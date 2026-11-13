{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Own the selected deposit and its relation to the confirmed certificate.
-- Registry status and the settlement window determine request eligibility in
-- SnapshotRequest; neither expiry nor recovery erases a signed certificate.
module Hydra.HeadLogic.Machine.Coordinated.Commits (Output (..), machine) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult, withStateInspector)
import Crem.StateInspection (describeOnly)
import Crem.Topology (AllowedTransition)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State qualified as Snapshots
import Hydra.HeadLogic.Machine.Coordinated.Topology (CommitPhase (..), CommitTopology)
import Hydra.HeadLogic.Machine.Coordinated.Transactions qualified as Transactions
import Hydra.HeadLogic.Machine.Coordinated.Types (CommitUpdate (..), SnapshotState (..))
import Hydra.Tx (IsTx, TxIdType)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data State tx phase where
  Unselected :: State tx 'NoCommit
  Selected :: TxIdType tx -> State tx 'CommitSelected
  -- The selected pointer may be absent after adoption, clearing or recovery,
  -- or name a different deposit queued while a certificate is outstanding.
  Authorised :: Maybe (TxIdType tx) -> State tx 'AwaitingCommitSettlement

data Output tx = Output
  { transactions :: Transactions.Output tx
  , currentDepositTxId :: Maybe (TxIdType tx)
  }

statePhase :: State tx phase -> CommitPhase
statePhase = \case
  Unselected -> NoCommit
  Selected{} -> CommitSelected
  Authorised{} -> AwaitingCommitSettlement

selection :: State tx phase -> Maybe (TxIdType tx)
selection = \case
  Unselected -> Nothing
  Selected depositId -> Just depositId
  Authorised selected -> selected

machine :: forall tx m. (IsTx tx, Applicative m) => SnapshotState tx -> Maybe (TxIdType tx) -> BaseMachineT m CommitTopology (Transactions.Output tx) (Output tx)
machine initialSnapshots initialSelection =
  BaseMachineT (withStateInspector (describeOnly (show . statePhase)) initialState) action
 where
  initialState
    | settling initialSnapshots = InitialState $ Authorised initialSelection
    | otherwise = maybe (InitialState Unselected) (InitialState . Selected) initialSelection

  action :: State tx from -> Transactions.Output tx -> ActionResult m CommitTopology (State tx) from (Output tx)
  action state input = case state of
    Unselected -> update state input
    Selected{} -> update state input
    Authorised{} -> update state input

  update ::
    ( AllowedTransition CommitTopology from 'NoCommit
    , AllowedTransition CommitTopology from 'CommitSelected
    , AllowedTransition CommitTopology from 'AwaitingCommitSettlement
    ) =>
    State tx from ->
    Transactions.Output tx ->
    ActionResult m CommitTopology (State tx) from (Output tx)
  update state input = case input.commitUpdate of
    KeepCommit -> output state
    RefreshCommit -> restore selected
    SelectCommit depositId -> restore $ selected <|> Just depositId
    SetCommitSelection next -> restore next
    ReleaseCommit depositId -> restore $ mfilter (/= depositId) selected
   where
    selected = selection state
    snapshots = Snapshots.viewInitial input.snapshots
    -- A newer version or a replacement certificate retires signed authority.
    -- A pending selection can survive either, so Authorised -> Selected is legal.
    restore next
      | settling snapshots = output $ Authorised next
      | otherwise = maybe (output Unselected) (output . Selected) next
    output :: AllowedTransition CommitTopology from to => State tx to -> ActionResult m CommitTopology (State tx) from (Output tx)
    output next = pureResult (Output input $ selection next) next

settling :: IsTx tx => SnapshotState tx -> Bool
settling snapshots =
  let snapshot = getSnapshot snapshots.confirmed
   in snapshot.version == snapshots.version && isJust snapshot.utxoToCommit
