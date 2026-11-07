-- | Crem selects rule phases; programs describe their work.
-- Running a workflow exposes the same instructions to the existing interpreters.
module Hydra.HeadLogic.Workflow where

import Crem.BaseMachine (ActionResult (..), BaseMachineT (..), InitialState (InspectableState))
import Crem.BaseMachine qualified as Base
import Crem.StateInspection (describeOnly)
import Crem.StateMachine qualified as Crem
import Crem.Topology (AllowedTransition, Topology)
import Hydra.HeadLogic.Program (Program)
import Hydra.Prelude hiding (state)

type Workflow tx ledger signing (topology :: Topology vertex) = BaseMachineT (Program tx ledger signing) topology () (Maybe ())

type Transition tx ledger signing topology state from = ActionResult (Program tx ledger signing) topology state from (Maybe ())

-- | Phase descriptions inspect constructors, never retained payloads. Workflow
-- state may contain opaque continuations, so inspection makes no strictness claim.
workflow :: (forall phase. state phase -> Text) -> state initial -> (forall from. state from -> Transition tx ledger signing topology state from) -> Workflow tx ledger signing topology
workflow describe initial step = BaseMachineT (InspectableState (describeOnly describe) initial) $ \state () -> step state

-- | A reply selects a destination with evidence that its edge is allowed.
type Destination topology state from = Base.Transition topology state from (Maybe ())

choose :: Program tx ledger signing (Destination topology state from) -> Transition tx ledger signing topology state from
choose = ChooseTransition

next :: AllowedTransition topology from to => state to -> Destination topology state from
next = Base.Transition Nothing

-- | A primitive reply enters a fixed, typed destination before the next branch.
continue :: AllowedTransition topology from to => (reply -> state to) -> Program tx ledger signing reply -> Transition tx ledger signing topology state from
continue destination operation = ActionResult $ (\reply -> (Nothing, destination reply)) <$> operation

-- | Completion is explicit. Waiting and rejection instead terminate in Program.
finish :: state phase -> Transition tx ledger signing topology state phase
finish state = ActionResult $ pure (Just (), state)

run :: Workflow tx ledger signing topology -> Program tx ledger signing ()
run = drive . Crem.Executable
 where
  drive :: Monad m => Crem.StateMachineT m () (Maybe a) -> m a
  drive machine = do
    (result, remaining) <- Crem.run machine ()
    maybe (drive remaining) pure result
