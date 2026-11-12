{-# LANGUAGE DataKinds #-}

module Allowed where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.Machine.Coordinated (Command, adoptSnapshot, confirmSnapshot)
import Hydra.HeadLogic.Machine.Coordinated.Topology (SnapshotPhase (..), SnapshotTopology, TransactionPhase (..), TransactionTopology)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedCandidate, VerifiedSnapshot)

data At phase = At

request :: ActionResult Identity SnapshotTopology At 'AwaitingRequest ()
request = ChooseTransition $ pure $ Transition () (At @'RequestSent)

confirm :: ActionResult Identity SnapshotTopology At 'CollectingSignatures ()
confirm = ChooseTransition $ pure $ Transition () (At @'AwaitingRequest)

reserve :: ActionResult Identity TransactionTopology At 'NoDecommit ()
reserve = ChooseTransition $ pure $ Transition () (At @'DecommitPending)

settle :: ActionResult Identity TransactionTopology At 'AwaitingSettlement ()
settle = ChooseTransition $ pure $ Transition () (At @'NoDecommit)

confirmOwned :: VerifiedCandidate tx -> Command tx
confirmOwned = confirmSnapshot

adoptCertificate :: VerifiedSnapshot tx -> Command tx
adoptCertificate = adoptSnapshot
