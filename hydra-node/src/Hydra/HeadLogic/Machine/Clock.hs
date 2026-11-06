{-# LANGUAGE OverloadedRecordDot #-}

-- | Own the node's synchronization status and observed chain clock.
module Hydra.HeadLogic.Machine.Clock (machine) where

import Hydra.Prelude

import Crem.BaseMachine (BaseMachineT (..), InitialState (..), pureResult)
import Crem.Topology (TrivialTopology)
import Hydra.Chain.ChainState (IsChainState (chainPointSlot), chainStateSlot)
import Hydra.HeadLogic.Machine.Types (HeadOutput (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..))

data Clock phase where
  Clock :: Bool -> ChainPointTime -> Clock '()

machine :: (IsChainState tx, Applicative m) => NodeState tx -> BaseMachineT m (TrivialTopology @()) (HeadOutput tx) (NodeState tx)
machine initial = BaseMachineT (InitialState $ Clock synced initial.chainPointTime) $ \(Clock wasSynced clock) HeadOutput{event, headState, deposits} ->
  let (isSynced, next) = case event of
        HeadOpened{chainState} -> (wasSynced, clock{currentSlot = chainStateSlot chainState})
        TickObserved{chainPoint, chainTime} -> (wasSynced, clock{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime})
        ChainRolledBack{chainState} -> (wasSynced, clock{currentSlot = chainStateSlot chainState})
        NodeSynced{chainSlot, chainTime, drift} -> (True, ChainPointTime chainSlot chainTime drift)
        NodeUnsynced{chainSlot, chainTime, drift} -> (False, ChainPointTime chainSlot chainTime drift)
        _ -> (wasSynced, clock)
      node = (if isSynced then NodeInSync else NodeCatchingUp) headState deposits next
   in pureResult node (Clock isSynced next)
 where
  synced = case initial of
    NodeInSync{} -> True
    NodeCatchingUp{} -> False
