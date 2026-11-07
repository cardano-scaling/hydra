{-# LANGUAGE OverloadedRecordDot #-}

-- | Compose the owners for live aggregation and recovery. Only NodeState is
-- persisted; checkpoints reconstruct every continuation at the same boundary.
module Hydra.HeadLogic.Machine (Runtime, start, step, currentState, apply, components) where

import Hydra.Prelude

import Crem.StateMachine (StateMachineT (..), run, withoutRendering)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Machine.Clock qualified as Clock
import Hydra.HeadLogic.Machine.DepositRegistry qualified as DepositRegistry
import Hydra.HeadLogic.Machine.Lifecycle qualified as Lifecycle
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State.Operations (eventHeadId, headIdOf)
import Hydra.Node.State (NodeState (..))

data Runtime tx = Runtime (NodeState tx) (StateMachineT Identity (StateChanged tx) (NodeState tx))

start :: IsChainState tx => NodeState tx -> Runtime tx
start node = Runtime node (withoutRendering $ components node)

currentState :: Runtime tx -> NodeState tx
currentState (Runtime node _) = node

apply :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
apply node = currentState . step (start node)

step :: IsChainState tx => Runtime tx -> StateChanged tx -> Runtime tx
step runtime@(Runtime node machine) event =
  case (headIdOf node.headState, eventHeadId event) of
    (Just ours, Just theirs) | ours /= theirs -> runtime
    _ -> case event of
      Checkpoint restored -> start restored
      _ ->
        let (updated, next) = runIdentity $ run machine event
         in Runtime updated next

-- | DepositOutput supplies the value before consumption. Lifecycle delegates
-- open-head changes to Snapshots → Transactions, and returns HeadOutput. Clock
-- assembles these projections with its own fields into the persisted NodeState.
-- Diagrams use this same composition; 'start' drops the rendering dictionaries.
components :: IsChainState tx => NodeState tx -> StateMachineT Identity (StateChanged tx) (NodeState tx)
components node =
  Basic (DepositRegistry.machine node.deposits)
    `Sequential` Basic (Lifecycle.machine node.headState)
    `Sequential` Basic (Clock.machine node)
