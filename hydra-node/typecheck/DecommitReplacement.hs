{-# LANGUAGE DataKinds #-}

module DecommitReplacement where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.Machine.Coordinated.Topology (TransactionPhase (..), TransactionTopology)

data At phase = At

replace :: ActionResult Identity TransactionTopology At 'AwaitingSettlement ()
replace = ChooseTransition $ pure $ Transition () (At @'DecommitPending)
