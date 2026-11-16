-- | A workflow consists of a Crem machine running a Program.
module Hydra.HeadLogic.Workflow where

import Crem.BaseMachine (
  ActionResult (..),
  BaseMachineT (..),
  InitialState (InspectableState),
 )
import Crem.BaseMachine qualified as Base (Transition (..))
import Crem.StateInspection (describeOnly)
import Crem.StateMachine qualified as Crem (
  StateMachineT (Executable),
  run,
 )
import Crem.Topology (AllowedTransition, Topology)
import Hydra.HeadLogic.Program (Program)
import Hydra.Prelude hiding (state)

type Workflow tx (topology :: Topology vertex) result =
  BaseMachineT (Program tx) topology () (Maybe result)

type Transition tx topology state from result =
  ActionResult (Program tx) topology state from (Maybe result)

type Destination topology state from result =
  Base.Transition topology state from (Maybe result)

-- | A workflow is a description, a starting state, and a function advancing
-- to the next state.
workflow ::
  (forall phase. state phase -> Text) ->
  state initial ->
  (forall from. state from -> Transition tx topology state from result) ->
  Workflow tx topology result
workflow describe initial step =
  BaseMachineT (InspectableState (describeOnly describe) initial) $ \state () -> step state

-- | Let a program decide which state to go to; i.e.
--
-- choose $ do
--    okay <- Program.decide "okay?" condition
--    if okay
--       then pure $ next NextThing
--       else Program.suspend waitReason
choose ::
  Program tx (Destination topology state from result) ->
  Transition tx topology state from result
choose = ChooseTransition

-- | Move to the next transition.
next :: AllowedTransition topology from to => state to -> Destination topology state from result
next = Base.Transition Nothing

-- | Perform some operation and head to the next state.
continue ::
  AllowedTransition topology from to =>
  (reply -> state to) ->
  Program tx reply ->
  Transition tx topology state from result
continue destination operation = ActionResult $ (\reply -> (Nothing, destination reply)) <$> operation

-- | Complete the state transformations.
finish ::
  state phase ->
  result ->
  Transition tx topology state phase result
finish state result = ActionResult $ pure (Just result, state)

-- | Run a Crem program and get a `Program` out.
run :: Workflow tx topology result -> Program tx result
run = drive . Crem.Executable
 where
  drive :: Monad m => Crem.StateMachineT m () (Maybe a) -> m a
  drive machine = do
    (result, remaining) <- Crem.run machine ()
    maybe (drive remaining) pure result
