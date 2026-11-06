{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Side-loading changes local state only. Failed requirements report to the
-- client; they are not protocol rejection. Input size is checked before routing.
module Hydra.HeadLogic.Rules.SideLoad where

import Control.Monad.Trans.Except (throwE)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (SideLoadRequirementFailure (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program (
  Program,
  decide,
  emit,
 )
import Hydra.HeadLogic.Rules.SnapshotVerification (verifySignedSnapshot)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (confirmedSnapshot),
  OpenState (OpenState, coordinatedHeadState, parameters),
 )
import Hydra.Prelude
import Hydra.Tx.Crypto (Verified (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)

onOpenClientSideLoadSnapshot :: IsChainState tx => OpenState tx -> ConfirmedSnapshot tx -> Program tx ledger signing ()
onOpenClientSideLoadSnapshot os@OpenState{parameters, coordinatedHeadState} requested = do
  result <- runExceptT $ case requested of
    InitialSnapshot{} -> do
      require "initial snapshot equals the current snapshot" (requestedSnapshot == current) SideLoadInitialSnapshotMismatch
      lift $ void $ Coordinated.stage runtime $ Coordinated.clearLocalState requestedSn
    ConfirmedSnapshot{snapshot, signatures} -> do
      require "side-loaded snapshot number does not go backwards" (requestedSn >= lastSeenSn) SideLoadSnNumberInvalid{requestedSn, lastSeenSn}
      require "side-loaded version matches" (requestedSv == lastSeenSv) SideLoadSvNumberInvalid{requestedSv, lastSeenSv}
      require "side-loaded commit matches" (requestedSc == lastSeenSc) SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc}
      require "side-loaded deposit matches" (requestedDeposit == lastSeenDeposit) SideLoadDepositTxIdInvalid{requestedDeposit, lastSeenDeposit}
      require "side-loaded decommit matches" (requestedSd == lastSeenSd) SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}
      verified <- lift $ verifySignedSnapshot vkeys snapshot signatures
      case verified of
        Left (FailedKeys failures) -> throwE SideLoadInvalidMultisignature{multisig = show signatures, vkeys = failures}
        Left _ -> throwE SideLoadInvalidMultisignature{multisig = show signatures, vkeys}
        Right evidence -> lift $ do
          adopted <- Coordinated.stage runtime $ Coordinated.adoptSnapshot evidence
          void $ Coordinated.stage adopted $ Coordinated.clearLocalState requestedSn

  case result of
    Left requirementFailure -> emit $ ClientEffect ServerOutput.SideLoadSnapshotRejected{clientInput = SideLoadSnapshot requested, requirementFailure}
    Right () -> pure ()
 where
  runtime = Coordinated.start os
  vkeys = vkey <$> parameters.parties
  current@Snapshot{version = lastSeenSv, number = lastSeenSn, utxoToCommit = lastSeenSc, depositTxId = lastSeenDeposit, utxoToDecommit = lastSeenSd} = getSnapshot coordinatedHeadState.confirmedSnapshot
  requestedSnapshot@Snapshot{version = requestedSv, number = requestedSn, utxoToCommit = requestedSc, depositTxId = requestedDeposit, utxoToDecommit = requestedSd} = getSnapshot requested

require :: Text -> Bool -> SideLoadRequirementFailure tx -> ExceptT (SideLoadRequirementFailure tx) (Program tx ledger signing) ()
require decision condition failure = do
  accepted <- lift $ decide decision condition
  unless accepted $ throwE failure
