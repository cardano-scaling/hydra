{-# LANGUAGE DataKinds #-}

module SnapshotRestart where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.State.Coordinated.Snapshots.Topology (SnapshotPhase (..), SnapshotTopology)

data At phase = At

restart :: ActionResult Identity SnapshotTopology At 'CollectingSignatures ()
restart = ChooseTransition $ pure $ Transition () (At @'RequestSent)
