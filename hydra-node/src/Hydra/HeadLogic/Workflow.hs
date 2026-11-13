-- | A workflow consisted of a Crem machine running a Program.
module Hydra.HeadLogic.Workflow where

import Crem.BaseMachine (ActionResult (..), BaseMachineT (..), InitialState (InspectableState))
import Crem.BaseMachine qualified as Base
import Crem.StateInspection (describeOnly)
import Crem.StateMachine qualified as Crem
import Crem.Topology (AllowedTransition, Topology)
import Hydra.HeadLogic.Program (Program)
import Hydra.Prelude hiding (state)

type Workflow tx ledger signing (topology :: Topology vertex) =
  BaseMachineT (Program tx ledger signing) topology () (Maybe ())

type Transition tx ledger signing topology state from =
  ActionResult (Program tx ledger signing) topology state from (Maybe ())

type Destination topology state from =
  Base.Transition topology state from (Maybe ())

-- | A workflow is a description, a starting state, and a function advancing
-- to the next state.
workflow ::
  (forall phase. state phase -> Text) ->
  state initial ->
  (forall from. state from -> Transition tx ledger signing topology state from) ->
  Workflow tx ledger signing topology
workflow describe initial step =
  BaseMachineT (InspectableState (describeOnly describe) initial) $ \state () -> step state

-- | Let's a program decide which state to go to; i.e.

-- choose $ do
--    okay <- Program.decide "okay?" condition
--    if okay then
--       then pure $ next NextThing
--       else Project.reject "All done"
choose ::
  Program tx ledger signing (Destination topology state from) ->
  Transition tx ledger signing topology state from
choose = ChooseTransition

-- | Move to the next transition.
next :: AllowedTransition topology from to => state to -> Destination topology state from
next = Base.Transition Nothing

-- | Perform some opteration and head to the next state.
continue ::
  AllowedTransition topology from to =>
  (reply -> state to) ->
  Program tx ledger signing reply ->
  Transition tx ledger signing topology state from
continue destination operation = ActionResult $ (\reply -> (Nothing, destination reply)) <$> operation

-- | Complete the state transformations.
finish ::
  state phase ->
  Transition tx ledger signing topology state phase
finish state = ActionResult $ pure (Just (), state)

-- | Run a Crem program and get a `Program` out.
run :: Workflow tx ledger signing topology -> Program tx ledger signing ()
run = drive . Crem.Executable
 where
  drive :: Monad m => Crem.StateMachineT m () (Maybe a) -> m a
  drive machine = do
    (result, remaining) <- Crem.run machine ()
    maybe (drive remaining) pure result
