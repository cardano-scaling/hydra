{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen replay with intentional corrections, stated with SimpleTx sets.
-- Keep the frozen reducer unchanged so the historical failure stays executable.
module Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.HeadLogic.Outcome (StateChanged (LocalStateCleared, SnapshotConfirmed, SnapshotRequested, headId, snapshot))
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (SeenSnapshot))
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Node.State (NodeState (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)

replayWithReservationFix :: NodeState SimpleTx -> StateChanged SimpleTx -> NodeState SimpleTx
replayWithReservationFix before event = case (event, legacy.headState) of
  (SnapshotRequested{}, Open os)
    | Just tx <- os.coordinatedHeadState.decommitTx ->
        let chs = os.coordinatedHeadState
         in legacy{headState = Open os{coordinatedHeadState = chs{localUTxO = chs.localUTxO `Set.difference` tx.txInputs}}}
  (SnapshotConfirmed{headId, snapshot = adopted}, Open os)
    | headId == os.headId
    , isJust adopted || collecting ->
        let chs = os.coordinatedHeadState
            snapshot = getSnapshot chs.confirmedSnapshot
            settling = chs.version == snapshot.version && isJust snapshot.utxoToDecommit
            base = snapshot.utxo <> if chs.version > snapshot.version then fold snapshot.utxoToCommit else mempty
            available = foldl' (\utxo pending -> (utxo `Set.difference` pending.txInputs) <> pending.txOutputs) base chs.localTxs
            matches tx = not (Set.null tx.txOutputs) && snapshot.utxoToDecommit == Just tx.txOutputs
            payload = find matches chs.decommitTx <|> find matches (Map.elems chs.allTxs)
            keep tx = (settling && matches tx) || (not settling && tx.txInputs `Set.isSubsetOf` available)
            released = not $ all keep chs.decommitTx
            retained = if settling then payload else mfilter keep chs.decommitTx
            next = chs{localUTxO = if released then available else chs.localUTxO, decommitTx = retained, allTxs = maybe chs.allTxs (\tx -> Map.delete tx.txSimpleId chs.allTxs) payload}
         in legacy{headState = Open os{coordinatedHeadState = next}}
  (LocalStateCleared{headId}, Open os)
    | headId == os.headId
    , Open prior <- before.headState
    , ConfirmedSnapshot{} <- prior.coordinatedHeadState.confirmedSnapshot ->
        let chs = os.coordinatedHeadState
            old = prior.coordinatedHeadState
            snapshot = getSnapshot old.confirmedSnapshot
            matches tx = not (Set.null tx.txOutputs) && snapshot.utxoToDecommit == Just tx.txOutputs
            retained = (find matches old.decommitTx <|> find matches (Map.elems old.allTxs)) <* guard (old.version == snapshot.version)
         in legacy{headState = Open os{coordinatedHeadState = chs{decommitTx = retained}}}
  _ -> legacy
 where
  legacy = legacyAggregateNodeState before event
  collecting = case before.headState of
    Open os -> case os.coordinatedHeadState.seenSnapshot of SeenSnapshot{} -> True; _ -> False
    _ -> False
