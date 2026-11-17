{-# LANGUAGE DataKinds #-}

module ClosedReopens where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.State.Head (LifecycleTopology, Phase (..))

data At phase = At

invalid :: ActionResult Identity LifecycleTopology At 'Closed ()
invalid = ChooseTransition $ pure $ Transition () (At @'Open)
