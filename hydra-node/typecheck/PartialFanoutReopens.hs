{-# LANGUAGE DataKinds #-}

module PartialFanoutReopens where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.State.Head (LifecycleTopology, Phase (..))

data At phase = At

invalid :: ActionResult Identity LifecycleTopology At 'PartialFanout ()
invalid = ChooseTransition $ pure $ Transition () (At @'Open)
