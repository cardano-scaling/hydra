{-# LANGUAGE DataKinds #-}

module Allowed where

import Crem.BaseMachine (ActionResult (ChooseTransition), Transition (Transition))
import Data.Functor.Identity (Identity)
import Hydra.HeadLogic.Machine.Coordinated (Command, adoptSnapshot, confirmSnapshot)
import Hydra.HeadLogic.Machine.Coordinated.Topology (CommitPhase (..), CommitTopology, DecommitPhase (..), DecommitTopology, SnapshotPhase (..), SnapshotTopology)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedCandidate, VerifiedSnapshot)

data At phase = At

request :: ActionResult Identity SnapshotTopology At 'AwaitingRequest ()
request = ChooseTransition $ pure $ Transition () (At @'RequestSent)

confirm :: ActionResult Identity SnapshotTopology At 'CollectingSignatures ()
confirm = ChooseTransition $ pure $ Transition () (At @'AwaitingRequest)

reserve :: ActionResult Identity DecommitTopology At 'NoDecommit ()
reserve = ChooseTransition $ pure $ Transition () (At @'DecommitPending)

settle :: ActionResult Identity DecommitTopology At 'AwaitingDecommitSettlement ()
settle = ChooseTransition $ pure $ Transition () (At @'NoDecommit)

selectCommit :: ActionResult Identity CommitTopology At 'NoCommit ()
selectCommit = ChooseTransition $ pure $ Transition () (At @'CommitSelected)

confirmCommit :: ActionResult Identity CommitTopology At 'CommitSelected ()
confirmCommit = ChooseTransition $ pure $ Transition () (At @'AwaitingCommitSettlement)

adoptCommit :: ActionResult Identity CommitTopology At 'NoCommit ()
adoptCommit = ChooseTransition $ pure $ Transition () (At @'AwaitingCommitSettlement)

settleCommit :: ActionResult Identity CommitTopology At 'AwaitingCommitSettlement ()
settleCommit = ChooseTransition $ pure $ Transition () (At @'NoCommit)

retireWithSelection :: ActionResult Identity CommitTopology At 'AwaitingCommitSettlement ()
retireWithSelection = ChooseTransition $ pure $ Transition () (At @'CommitSelected)

confirmOwned :: VerifiedCandidate tx -> Command tx
confirmOwned = confirmSnapshot

adoptCertificate :: VerifiedSnapshot tx -> Command tx
adoptCertificate = adoptSnapshot
