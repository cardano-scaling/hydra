{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen replay with intentional corrections, stated with SimpleTx sets.
-- Keep the frozen reducer unchanged so the historical failure stays executable.
module Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix) where

import Hydra.Prelude

import Data.Set qualified as Set
import Hydra.HeadLogic.Outcome (StateChanged (SnapshotConfirmed, SnapshotRequested, headId, snapshot))
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (SeenSnapshot))
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Node.State (NodeState (..))
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)

replayWithReservationFix :: NodeState SimpleTx -> StateChanged SimpleTx -> NodeState SimpleTx
replayWithReservationFix before event = case (event, legacy.headState) of
  (SnapshotRequested{}, Open os)
    | Just tx <- os.coordinatedHeadState.decommitTx ->
        let chs = os.coordinatedHeadState
         in legacy{headState = Open os{coordinatedHeadState = chs{localUTxO = chs.localUTxO `Set.difference` tx.txInputs}}}
  (SnapshotConfirmed{headId, snapshot = adopted}, Open os)
    | headId == os.headId
    , isJust adopted || collecting
    , Just tx <- os.coordinatedHeadState.decommitTx ->
        let chs = os.coordinatedHeadState
            snapshot = getSnapshot chs.confirmedSnapshot
            settling = chs.version == snapshot.version && isJust snapshot.utxoToDecommit
            carried = settling && snapshot.utxoToDecommit == Just tx.txOutputs
            base = snapshot.utxo <> if chs.version > snapshot.version then fold snapshot.utxoToCommit else mempty
            available = foldl' (\utxo pending -> (utxo `Set.difference` pending.txInputs) <> pending.txOutputs) base chs.localTxs
         in if carried || (not settling && tx.txInputs `Set.isSubsetOf` available)
              then legacy
              else legacy{headState = Open os{coordinatedHeadState = chs{localUTxO = available, decommitTx = Nothing}}}
  _ -> legacy
 where
  legacy = legacyAggregateNodeState before event
  collecting = case before.headState of
    Open os -> case os.coordinatedHeadState.seenSnapshot of SeenSnapshot{} -> True; _ -> False
    _ -> False
