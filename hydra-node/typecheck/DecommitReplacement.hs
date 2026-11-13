{-# LANGUAGE DataKinds #-}

module DecommitReplacement where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.Machine.Coordinated.Topology (DecommitPhase (..), DecommitTopology)

data At phase = At

replace :: ActionResult Identity DecommitTopology At 'AwaitingDecommitSettlement ()
replace = ChooseTransition $ pure $ Transition () (At @'DecommitPending)
