-- | A workflow consists of a Crem machine running a Program.
module Hydra.HeadLogic.Workflow where

import Crem.BaseMachine (ActionResult (..), BaseMachineT (..), InitialState (InspectableState))
import Crem.BaseMachine qualified as Base
import Crem.StateInspection (describeOnly)
import Crem.StateMachine qualified as Crem
import Crem.Topology (AllowedTransition, Topology)
import Hydra.HeadLogic.Program (Program)
import Hydra.Prelude hiding (state)

type Workflow tx (topology :: Topology vertex) =
  BaseMachineT (Program tx) topology () (Maybe ())

type Transition tx topology state from =
  ActionResult (Program tx) topology state from (Maybe ())

type Destination topology state from =
  Base.Transition topology state from (Maybe ())

-- | A workflow is a description, a starting state, and a function advancing
-- to the next state.
workflow ::
  (forall phase. state phase -> Text) ->
  state initial ->
  (forall from. state from -> Transition tx topology state from) ->
  Workflow tx topology
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
  Program tx (Destination topology state from) ->
  Transition tx topology state from
choose = ChooseTransition

-- | Move to the next transition.
next :: AllowedTransition topology from to => state to -> Destination topology state from
next = Base.Transition Nothing

-- | Perform some operation and head to the next state.
continue ::
  AllowedTransition topology from to =>
  (reply -> state to) ->
  Program tx reply ->
  Transition tx topology state from
continue destination operation = ActionResult $ (\reply -> (Nothing, destination reply)) <$> operation

-- | Complete the state transformations.
finish ::
  state phase ->
  Transition tx topology state phase
finish state = ActionResult $ pure (Just (), state)

-- | Run a Crem program and get a `Program` out.
run :: Workflow tx topology -> Program tx ()
run = drive . Crem.Executable
 where
  drive :: Monad m => Crem.StateMachineT m () (Maybe a) -> m a
  drive machine = do
    (result, remaining) <- Crem.run machine ()
    maybe (drive remaining) pure result
