{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Client.Fanout where

import Crem.Topology (Topology (Topology))
import Hydra.API.ClientInput qualified as Client
import Hydra.API.ServerOutput qualified as ServerOutput (ClientMessage (CommandFailed))
import Hydra.HeadLogic.Fanout.Plan (FanoutPlan, closedSelectionPlan, fullPlan, planChange, planTransaction, remainingSelectionPlan)
import Hydra.HeadLogic.Outcome (Effect (ClientEffect, OnChainEffect))
import Hydra.HeadLogic.Program (Program, change, decide, emit)
import Hydra.HeadLogic.State (ClosedState, HeadState (Closed, FanoutProgress), PartialFanoutState)
import Hydra.HeadLogic.Workflow (Transition, Workflow, choose, continue, finish, next)
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, UTxOType)

data Command tx
  = Drain (ClosedState tx)
  | SelectClosed (ClosedState tx) (UTxOType tx)
  | SelectRemaining (PartialFanoutState tx) (UTxOType tx)

data Stage
  = ValidateSelection
  | RecordIntent
  | PostStep
  | Finished

type FanoutTopology =
  'Topology
    '[ '( 'ValidateSelection, '[ 'RecordIntent, 'Finished])
     , '( 'RecordIntent, '[ 'PostStep])
     , '( 'PostStep, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  Validate :: State tx 'ValidateSelection
  Record :: FanoutPlan tx -> State tx 'RecordIntent
  Post :: FanoutPlan tx -> State tx 'PostStep
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  Validate -> ValidateSelection
  Record{} -> RecordIntent
  Post{} -> PostStep
  Done -> Finished

workflow :: forall tx. IsTx tx => Command tx -> Workflow tx FanoutTopology ()
workflow command = Workflow.workflow (show . statePhase) Validate step
 where
  step :: State tx from -> Transition tx FanoutTopology (State tx) from ()
  step = \case
    Validate -> choose $ do
      let planned = case command of
            Drain closed -> Just $ fullPlan closed
            SelectClosed closed selection -> closedSelectionPlan closed selection
            SelectRemaining progress selection -> remainingSelectionPlan progress selection
      void $ decide "fanout selection is valid" (isJust planned)
      case planned of
        Just plan -> pure $ next $ Record plan
        Nothing -> do
          let (input, headState) = case command of
                Drain closed -> (Client.Fanout, Closed closed)
                SelectClosed closed selection -> (Client.PartialFanout selection, Closed closed)
                SelectRemaining progress selection -> (Client.PartialFanout selection, FanoutProgress progress)
          emit $ ClientEffect $ ServerOutput.CommandFailed input headState
          pure $ next Done
    Record plan -> continue (const $ Post plan) $ change $ planChange plan
    Post plan -> continue (const Done) $ emit $ OnChainEffect $ planTransaction plan
    Done -> finish Done ()

onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  Program tx ()
onClosedClientFanout = Workflow.run . workflow . Drain

onClosedClientPartialFanout ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  Program tx ()
onClosedClientPartialFanout closed selection =
  Workflow.run $ workflow $ SelectClosed closed selection

onPartialFanoutClientPartialFanout ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  Program tx ()
onPartialFanoutClientPartialFanout progress selection =
  Workflow.run $ workflow $ SelectRemaining progress selection
