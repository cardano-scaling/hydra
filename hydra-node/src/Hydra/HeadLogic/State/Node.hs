{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.State.Node (Runtime, start, step, currentState, apply, applyEvents, components) where

import Crem.StateMachine (
  StateMachineT (..),
  run,
  withoutRendering,
 )
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged (..), eventHeadId)
import Hydra.HeadLogic.State (
  FinalizedState (chainState),
  HeadState (Finalized, Idle),
  IdleState (IdleState),
  canOpenHead,
  headIdOf,
 )
import Hydra.HeadLogic.State.Clock qualified as Clock (machine)
import Hydra.HeadLogic.State.Deposits qualified as DepositRegistry (
  machine,
 )
import Hydra.HeadLogic.State.Head qualified as Lifecycle (
  machine,
 )
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude

data Runtime tx
  = Runtime
      (NodeState tx)
      (StateMachineT Identity (StateChanged tx) (NodeState tx))

start :: IsChainState tx => NodeState tx -> Runtime tx
start node = Runtime node (withoutRendering $ components node)

currentState :: Runtime tx -> NodeState tx
currentState (Runtime node _) = node

apply :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
apply node = currentState . step (start node)

applyEvents :: IsChainState tx => NodeState tx -> [StateChanged tx] -> NodeState tx
applyEvents node [] = node
applyEvents node events = currentState $ foldl' step (start node) events

step :: IsChainState tx => Runtime tx -> StateChanged tx -> Runtime tx
step runtime@(Runtime node _) event@HeadOpened{headId}
  | not (canOpenHead node.headState headId) = runtime
  | Finalized previous <- node.headState =
      -- A different identity starts a new machine. The completed Head has no
      -- transition to Idle or Open, and keeps its identity in checkpoints.
      step (start node{headState = Idle $ IdleState previous.chainState}) event
step runtime@(Runtime node machine) event =
  case (headIdOf node.headState, eventHeadId event) of
    (Just ours, Just theirs) | ours /= theirs -> runtime
    _ -> case event of
      Checkpoint restored -> start restored
      _ ->
        let (updated, next) = runIdentity $ run machine event
         in Runtime updated next

-- | DepositOutput supplies the value before consumption. Lifecycle delegates
-- open-head changes to Snapshots → Transactions → Commits, and returns HeadOutput. Clock
-- assembles these projections with its own fields into the persisted NodeState.
-- Inspection uses this composition; 'start' drops the rendering dictionaries.
-- 'step' owns checkpoint restoration and replacement by a different Head.
components ::
  IsChainState tx =>
  NodeState tx ->
  StateMachineT Identity (StateChanged tx) (NodeState tx)
components node =
  Basic (DepositRegistry.machine node.deposits)
    `Sequential` Basic (Lifecycle.machine node.headState)
    `Sequential` Basic (Clock.machine node)
