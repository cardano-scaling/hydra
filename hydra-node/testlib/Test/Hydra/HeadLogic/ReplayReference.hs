{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen replay with one intentional correction, stated with SimpleTx sets.
-- Keep the frozen reducer unchanged so the historical failure stays executable.
module Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix) where

import Hydra.Prelude

import Data.Set qualified as Set
import Hydra.HeadLogic.Outcome (StateChanged (SnapshotRequested))
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..))
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Node.State (NodeState (..))
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)

replayWithReservationFix :: NodeState SimpleTx -> StateChanged SimpleTx -> NodeState SimpleTx
replayWithReservationFix before event = case (event, legacy.headState) of
  (SnapshotRequested{}, Open os)
    | Just tx <- os.coordinatedHeadState.decommitTx ->
        let chs = os.coordinatedHeadState
         in legacy{headState = Open os{coordinatedHeadState = chs{localUTxO = chs.localUTxO `Set.difference` tx.txInputs}}}
  _ -> legacy
 where
  legacy = legacyAggregateNodeState before event
