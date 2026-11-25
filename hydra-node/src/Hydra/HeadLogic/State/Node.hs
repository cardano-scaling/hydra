{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Hydra.HeadLogic.State.Node (Runtime, start, step, currentState, apply, applyEvents, inspect) where

import Crem.Inspect qualified as Crem (inspectStates)
import Crem.StateMachine (
  StateMachineT (..),
  run,
 )
import Hydra.Chain.ChainState (IsChainState (chainPointSlot), chainStateSlot)
import Hydra.HeadLogic.Outcome (StateChanged (..), eventHeadId)
import Hydra.HeadLogic.State (
  FinalizedState (chainState),
  HeadState (Finalized, Idle),
  IdleState (IdleState),
  canOpenHead,
  headIdOf,
 )
import Hydra.HeadLogic.State.Deposits qualified as DepositRegistry (
  machine,
 )
import Hydra.HeadLogic.State.Head qualified as Lifecycle (
  machine,
 )
import Hydra.HeadLogic.State.Types qualified as State (DepositOutput (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..))
import Hydra.Prelude

data Runtime tx = Runtime
  { nodeState :: NodeState tx
  , depositMachine :: StateMachineT Identity (StateChanged tx) (State.DepositOutput tx)
  , headMachine :: StateMachineT Identity (State.DepositOutput tx) (HeadState tx)
  }

start :: IsChainState tx => NodeState tx -> Runtime tx
start node =
  Runtime
    { nodeState = node
    , depositMachine = Executable $ DepositRegistry.machine node.deposits
    , headMachine = Executable $ Lifecycle.machine node.headState
    }

currentState :: Runtime tx -> NodeState tx
currentState = (.nodeState)

apply :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
apply node = currentState . step (start node)

applyEvents :: IsChainState tx => NodeState tx -> [StateChanged tx] -> NodeState tx
applyEvents node [] = node
applyEvents node events = currentState $ foldl' step (start node) events

step :: IsChainState tx => Runtime tx -> StateChanged tx -> Runtime tx
step runtime@Runtime{nodeState = node} event@HeadOpened{headId}
  | not (canOpenHead node.headState headId) = runtime
  | Finalized previous <- node.headState =
      -- A different identity starts a new machine. The completed Head has no
      -- transition to Idle or Open, and keeps its identity in checkpoints.
      step (start node{headState = Idle $ IdleState previous.chainState}) event
step runtime@Runtime{nodeState = node, ..} event =
  case (headIdOf node.headState, eventHeadId event) of
    (Just ours, Just theirs) | ours /= theirs -> runtime
    _ -> case event of
      Checkpoint restored -> start restored
      _ ->
        -- Capture a settled deposit before consuming it, then update the Head.
        let (depositUpdate, nextDeposits) = runIdentity $ run depositMachine event
            (headState, nextHead) = runIdentity $ run headMachine depositUpdate
            updated = updateChainObservation node{headState, deposits = depositUpdate.deposits} event
         in Runtime updated nextDeposits nextHead

-- | Remember observed chain time and synchronisation status in the node state.
updateChainObservation :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
updateChainObservation node = \case
  HeadOpened{chainState} ->
    node{chainPointTime = observed{currentSlot = chainStateSlot chainState}}
  TickObserved{chainPoint, chainTime} ->
    node{chainPointTime = observed{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
  ChainRolledBack{chainState} ->
    node{chainPointTime = observed{currentSlot = chainStateSlot chainState}}
  NodeSynced{chainSlot, chainTime, drift} ->
    NodeInSync node.headState node.deposits (ChainPointTime chainSlot chainTime drift)
  NodeUnsynced{chainSlot, chainTime, drift} ->
    NodeCatchingUp node.headState node.deposits (ChainPointTime chainSlot chainTime drift)
  _ -> node
 where
  observed = node.chainPointTime

-- | Inspect the retained component states without processing an event.
inspect :: Runtime tx -> [(Text, Maybe Text)]
inspect Runtime{..} =
  [("Deposits", description) | (_, description) <- Crem.inspectStates depositMachine]
    <> [("Head", description) | (_, description) <- Crem.inspectStates headMachine]
