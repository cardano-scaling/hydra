{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Execute retained events using crem. Only NodeState is persisted; the
-- continuation can be reconstructed from it at any event boundary.
module Hydra.HeadLogic.Machine (
  Runtime,
  start,
  step,
  currentState,
  apply,
  components,
) where

import Hydra.Prelude

import Crem.StateMachine (StateMachineT (..), run, stateless)
import Hydra.Chain.ChainState (IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Machine.Incremental qualified as Incremental
import Hydra.HeadLogic.Machine.Lifecycle qualified as Lifecycle
import Hydra.HeadLogic.Machine.OffChain qualified as OffChain
import Hydra.HeadLogic.Machine.Types (Transition (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State.Operations (eventHeadId, headIdOf)
import Hydra.Node.State (
  ChainPointTime (ChainPointTime, currentChainTime, currentSlot),
  Deposit (Deposit, created, deadline, deposited, headId, status),
  DepositStatus (Inactive),
  NodeState (..),
  consumeDeposit,
  recordDeposit,
  rollbackDeposits,
  updateDeposit,
 )

-- | The constructor is private so callers cannot separate a continuation from
-- the state whose phases it tracks. Checkpoint restoration replaces both.
data Runtime tx = Runtime (NodeState tx) (StateMachineT Identity (Transition tx) (Transition tx))

start :: IsChainState tx => NodeState tx -> Runtime tx
start node = Runtime node (components node)

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
        let (result, next) = runIdentity $ run machine Transition{before = node, event, current = node}
         in Runtime result.current next

-- | Sequential composition is intentional: lifecycle changes the outer head
-- constructor; incremental settlement updates versions and pending actions;
-- snapshot coordination then uses those values. Deposit history changes last,
-- after settlement has read the pre-event pending deposit.
components :: IsChainState tx => NodeState tx -> StateMachineT Identity (Transition tx) (Transition tx)
components node =
  Basic (Lifecycle.machine node.headState)
    `Sequential` Basic (Incremental.machine node.headState)
    `Sequential` Basic (OffChain.machine node.headState)
    `Sequential` stateless metadata

metadata :: IsChainState tx => Transition tx -> Transition tx
metadata input = input{current = update input.current}
 where
  update node =
    let clock = node.chainPointTime
     in case input.event of
          HeadOpened{chainState} -> node{chainPointTime = clock{currentSlot = chainStateSlot chainState}}
          DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
            recordDeposit (chainStateSlot chainState) depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} node
          DepositActivated{depositTxId, deposit} -> updateDeposit depositTxId deposit node
          DepositExpired{depositTxId, deposit} -> updateDeposit depositTxId deposit node
          DepositRecovered{chainState, depositTxId} -> consumeDeposit (chainStateSlot chainState) depositTxId node
          CommitFinalized{chainState, depositTxId} -> consumeDeposit (chainStateSlot chainState) depositTxId node
          TickObserved{chainPoint, chainTime} -> node{chainPointTime = clock{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
          ChainRolledBack{chainState} -> rollbackDeposits (chainStateSlot chainState) node{chainPointTime = clock{currentSlot = chainStateSlot chainState}}
          NodeUnsynced{chainSlot, chainTime, drift} -> NodeCatchingUp{headState = node.headState, deposits = node.deposits, chainPointTime = ChainPointTime chainSlot chainTime drift}
          NodeSynced{chainSlot, chainTime, drift} -> NodeInSync{headState = node.headState, deposits = node.deposits, chainPointTime = ChainPointTime chainSlot chainTime drift}
          _ -> node
