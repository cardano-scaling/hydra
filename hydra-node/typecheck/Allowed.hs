{-# LANGUAGE DataKinds #-}

module Allowed where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.Snapshot.Verification (VerifiedCandidate, VerifiedSnapshot)
import Hydra.HeadLogic.State.Coordinated (Command, DecommitPhase (..), DecommitTopology, adoptSnapshot, confirmSnapshot)
import Hydra.HeadLogic.State.Coordinated.Snapshots.Topology (SnapshotPhase (..), SnapshotTopology)

import Hydra.HeadLogic.State.Head (LifecycleTopology, Phase (..))

data At phase = At

request :: ActionResult Identity SnapshotTopology At 'AwaitingRequest ()
request = ChooseTransition $ pure $ Transition () (At @'RequestSent)

confirm :: ActionResult Identity SnapshotTopology At 'CollectingSignatures ()
confirm = ChooseTransition $ pure $ Transition () (At @'AwaitingRequest)

recordPending :: ActionResult Identity DecommitTopology At 'NoDecommit ()
recordPending = ChooseTransition $ pure $ Transition () (At @'DecommitPending)

settle :: ActionResult Identity DecommitTopology At 'AwaitingDecommitSettlement ()
settle = ChooseTransition $ pure $ Transition () (At @'NoDecommit)

confirmOwned :: VerifiedCandidate tx -> Command tx
confirmOwned = confirmSnapshot

adoptCertificate :: VerifiedSnapshot tx -> Command tx
adoptCertificate = adoptSnapshot

openHead :: ActionResult Identity LifecycleTopology At 'Idle ()
openHead = ChooseTransition $ pure $ Transition () (At @'Open)

contest :: ActionResult Identity LifecycleTopology At 'Closed ()
contest = ChooseTransition $ pure $ Transition () (At @'Closed)

observePartialFanout :: ActionResult Identity LifecycleTopology At 'Closed ()
observePartialFanout = ChooseTransition $ pure $ Transition () (At @'PartialFanout)

continuePartialFanout :: ActionResult Identity LifecycleTopology At 'PartialFanout ()
continuePartialFanout = ChooseTransition $ pure $ Transition () (At @'PartialFanout)

fullFanout :: ActionResult Identity LifecycleTopology At 'Closed ()
fullFanout = ChooseTransition $ pure $ Transition () (At @'Finalized)

finishPartialFanout :: ActionResult Identity LifecycleTopology At 'PartialFanout ()
finishPartialFanout = ChooseTransition $ pure $ Transition () (At @'Finalized)
