{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Client.SideLoad where

import Control.Monad.Trans.Except (throwE)
import Crem.Topology (Topology (Topology))
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (
  ClientMessage (
    SideLoadSnapshotRejected,
    clientInput,
    requirementFailure
  ),
 )
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (SideLoadRequirementFailure (..))
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program (Program, emit, traceB)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Snapshot.Verification (
  VerifiedSnapshot,
  verifySignedSnapshot,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (confirmedSnapshot),
  OpenState (OpenState, coordinatedHeadState, parameters),
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  Runtime,
  adoptSnapshot,
  clearLocalState,
  start,
 )
import Hydra.HeadLogic.Workflow (Transition, Workflow, choose, continue, finish, next)
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Prelude hiding (State, state)
import Hydra.Tx.Crypto (MultiSignature, VerificationError (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  getSnapshot,
 )

data Stage
  = CheckCompatibility
  | VerifyCertificate
  | AdoptSnapshot
  | ClearLocalState
  | ReportRefusal
  | Finished

type SideLoadTopology =
  'Topology
    '[ '( 'CheckCompatibility, '[ 'VerifyCertificate, 'ClearLocalState, 'ReportRefusal])
     , '( 'VerifyCertificate, '[ 'AdoptSnapshot, 'ReportRefusal])
     , '( 'AdoptSnapshot, '[ 'ClearLocalState])
     , '( 'ClearLocalState, '[ 'Finished])
     , '( 'ReportRefusal, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  Check :: State tx 'CheckCompatibility
  Verify :: Snapshot tx -> MultiSignature (Snapshot tx) -> State tx 'VerifyCertificate
  Adopt :: VerifiedSnapshot tx -> State tx 'AdoptSnapshot
  Clear :: Coordinated.Runtime tx -> State tx 'ClearLocalState
  Refuse :: SideLoadRequirementFailure tx -> State tx 'ReportRefusal
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  Check -> CheckCompatibility
  Verify{} -> VerifyCertificate
  Adopt{} -> AdoptSnapshot
  Clear{} -> ClearLocalState
  Refuse{} -> ReportRefusal
  Done -> Finished

workflow ::
  forall tx.
  IsChainState tx =>
  OpenState tx ->
  ConfirmedSnapshot tx ->
  Workflow tx SideLoadTopology ()
workflow os@OpenState{parameters, coordinatedHeadState} requested = Workflow.workflow (show . statePhase) Check step
 where
  step :: State tx from -> Transition tx SideLoadTopology (State tx) from ()
  step = \case
    Check -> choose $ case requested of
      InitialSnapshot{} -> do
        result <- runExceptT $ require "initial snapshot equals the current snapshot" (requestedSnapshot == current) SideLoadInitialSnapshotMismatch
        pure $ either (next . Refuse) (const $ next $ Clear runtime) result
      ConfirmedSnapshot{snapshot, signatures} -> do
        result <- runExceptT $ do
          require
            "side-loaded snapshot number does not go backwards"
            (requestedSn >= lastSeenSn)
            SideLoadSnNumberInvalid{requestedSn, lastSeenSn}

          require
            "side-loaded version matches"
            (requestedSv == lastSeenSv)
            SideLoadSvNumberInvalid{requestedSv, lastSeenSv}

          require
            "side-loaded commit matches"
            (requestedSc == lastSeenSc)
            SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc}

          require
            "side-loaded deposit matches"
            (requestedDeposit == lastSeenDeposit)
            SideLoadDepositTxIdInvalid{requestedDeposit, lastSeenDeposit}

          require
            "side-loaded decommit matches"
            (requestedSd == lastSeenSd)
            SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}

        pure $ either (next . Refuse) (const $ next $ Verify snapshot signatures) result
    Verify snapshot signatures -> choose $ do
      verified <- verifySignedSnapshot vkeys snapshot signatures
      pure $ case verified of
        Left (InvalidSignatures failures) ->
          next $ Refuse SideLoadInvalidMultisignature{multisig = show signatures, vkeys = toList failures}
        Left _ ->
          next $ Refuse SideLoadInvalidMultisignature{multisig = show signatures, vkeys}
        Right evidence -> next $ Adopt evidence
    Adopt evidence ->
      continue Clear $ Staging.stage runtime $ Coordinated.adoptSnapshot evidence
    Clear adopted ->
      continue (const Done) $ void $ Staging.stage adopted $ Coordinated.clearLocalState requestedSn
    Refuse requirementFailure ->
      continue (const Done) $ emit $ ClientEffect SideLoadSnapshotRejected{clientInput = SideLoadSnapshot requested, requirementFailure}
    Done -> finish Done ()

  runtime = Coordinated.start os
  vkeys = vkey <$> parameters.parties

  current@Snapshot
    { version = lastSeenSv
    , number = lastSeenSn
    , utxoToCommit = lastSeenSc
    , depositTxId = lastSeenDeposit
    , utxoToDecommit = lastSeenSd
    } = getSnapshot coordinatedHeadState.confirmedSnapshot

  requestedSnapshot@Snapshot
    { version = requestedSv
    , number = requestedSn
    , utxoToCommit = requestedSc
    , depositTxId = requestedDeposit
    , utxoToDecommit = requestedSd
    } = getSnapshot requested

onOpenClientSideLoadSnapshot ::
  IsChainState tx =>
  OpenState tx ->
  ConfirmedSnapshot tx ->
  Program tx ()
onOpenClientSideLoadSnapshot os requested = Workflow.run $ workflow os requested

require ::
  Text ->
  Bool ->
  SideLoadRequirementFailure tx ->
  ExceptT (SideLoadRequirementFailure tx) (Program tx) ()
require decision condition failure = do
  accepted <- lift $ traceB decision condition
  unless accepted $ throwE failure
