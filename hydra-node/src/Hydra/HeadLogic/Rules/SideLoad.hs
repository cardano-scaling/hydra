{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Side-loading changes local state only. Failed requirements report to the
-- client; they are not protocol rejection. Input size is checked before routing.
module Hydra.HeadLogic.Rules.SideLoad where

import Control.Monad.Trans.Except (throwE)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.HeadLogic.Error (SideLoadRequirementFailure (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (
  Program,
  change,
  decide,
  emit,
  verifySnapshot,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (confirmedSnapshot),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
 )
import Hydra.Prelude
import Hydra.Tx (IsTx)
import Hydra.Tx.Crypto (Verified (..), getSignableRepresentation)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)

onOpenClientSideLoadSnapshot :: IsTx tx => OpenState tx -> ConfirmedSnapshot tx -> Program tx ledger signing ()
onOpenClientSideLoadSnapshot OpenState{headId, parameters, coordinatedHeadState} requested = do
  result <- runExceptT $ case requested of
    InitialSnapshot{} -> do
      require "initial snapshot equals the current snapshot" (requestedSnapshot == current) SideLoadInitialSnapshotMismatch
      lift $ change LocalStateCleared{headId, snapshotNumber = requestedSn}
    ConfirmedSnapshot{snapshot, signatures} -> do
      require "side-loaded snapshot number does not go backwards" (requestedSn >= lastSeenSn) SideLoadSnNumberInvalid{requestedSn, lastSeenSn}
      require "side-loaded version matches" (requestedSv == lastSeenSv) SideLoadSvNumberInvalid{requestedSv, lastSeenSv}
      require "side-loaded commit matches" (requestedSc == lastSeenSc) SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc}
      require "side-loaded deposit matches" (requestedDeposit == lastSeenDeposit) SideLoadDepositTxIdInvalid{requestedDeposit, lastSeenDeposit}
      require "side-loaded decommit matches" (requestedSd == lastSeenSd) SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}
      verified <- lift $ verifySnapshot vkeys signatures (getSignableRepresentation snapshot)
      case verified of
        Verified -> pure ()
        FailedKeys failures -> throwE SideLoadInvalidMultisignature{multisig = show signatures, vkeys = failures}
        KeyNumberMismatch -> throwE SideLoadInvalidMultisignature{multisig = show signatures, vkeys}
      lift $ do
        change SnapshotConfirmed{headId, snapshot = Just snapshot, signatures}
        change LocalStateCleared{headId, snapshotNumber = requestedSn}
  case result of
    Left requirementFailure -> emit $ ClientEffect ServerOutput.SideLoadSnapshotRejected{clientInput = SideLoadSnapshot requested, requirementFailure}
    Right () -> pure ()
 where
  vkeys = vkey <$> parameters.parties
  current@Snapshot{version = lastSeenSv, number = lastSeenSn, utxoToCommit = lastSeenSc, depositTxId = lastSeenDeposit, utxoToDecommit = lastSeenSd} = getSnapshot coordinatedHeadState.confirmedSnapshot
  requestedSnapshot@Snapshot{version = requestedSv, number = requestedSn, utxoToCommit = requestedSc, depositTxId = requestedDeposit, utxoToDecommit = requestedSd} = getSnapshot requested

require :: Text -> Bool -> SideLoadRequirementFailure tx -> ExceptT (SideLoadRequirementFailure tx) (Program tx ledger signing) ()
require decision condition failure = do
  accepted <- lift $ decide decision condition
  unless accepted $ throwE failure
