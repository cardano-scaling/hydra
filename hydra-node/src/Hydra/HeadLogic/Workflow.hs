-- | Crem selects rule phases; programs describe their work.
-- Running a workflow exposes the same instructions to the existing interpreters.
module Hydra.HeadLogic.Workflow where

import Crem.BaseMachine (ActionResult (..), BaseMachineT)
import Crem.Render.RenderableVertices (RenderableVertices)
import Crem.StateMachine qualified as Crem
import Crem.Topology (AllowedTransition, Topology)
import Data.Singletons.Base.TH (SingI, SingKind (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.Prelude hiding (state)

type Workflow tx ledger signing (topology :: Topology vertex) = BaseMachineT (Program tx ledger signing) topology () (Maybe ())

type Transition tx ledger signing topology state from = ActionResult (Program tx ledger signing) topology state from (Maybe ())

-- | A primitive reply enters a fixed, typed destination before the next branch.
continue :: AllowedTransition topology from to => (reply -> state to) -> Program tx ledger signing reply -> Transition tx ledger signing topology state from
continue next operation = ActionResult $ (\reply -> (Nothing, next reply)) <$> operation

-- | Completion is explicit. Waiting and rejection instead terminate in Program.
finish :: state phase -> Transition tx ledger signing topology state phase
finish state = ActionResult $ pure (Just (), state)

run :: (SingKind vertex, Demote vertex ~ vertex, Eq vertex, Show vertex, RenderableVertices vertex, SingI topology) => Workflow tx ledger signing (topology :: Topology vertex) -> Program tx ledger signing ()
run = drive . Crem.Basic
 where
  drive :: Monad m => Crem.StateMachineT m () (Maybe a) -> m a
  drive machine = do
    (result, next) <- Crem.run machine ()
    maybe (drive next) pure result
