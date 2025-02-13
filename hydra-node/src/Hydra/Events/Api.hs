{-# LANGUAGE RecordWildCards #-}

module Hydra.Events.Api where

import Hydra.Prelude

import Conduit (mapMC, (.|))
import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..))
import Hydra.Events.FileBased (PersistedStateChange (..))
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.Outcome qualified as StateChanged
import Hydra.Node (HydraNode (..))
import Hydra.Persistence (PersistenceIncremental (..))
import Hydra.Tx (HeadId (UnsafeHeadId), IsTx, txId)

wireApiEvents ::
  (IsChainState tx, MonadSTM m) =>
  Server tx m ->
  PersistenceIncremental (PersistedStateChange tx) m ->
  m (EventSink (StateEvent tx) m)
wireApiEvents server PersistenceIncremental{append, source} = do
  let putEvent StateEvent{stateChanged} =
        maybe (pure ()) sendOutput $ mapStateChangedToServerOutput stateChanged
  pure EventSink{putEvent}
 where
  Server{sendOutput} = server

mapStateChangedToServerOutput :: IsTx tx => StateChanged.StateChanged tx -> Maybe (ServerOutput tx)
mapStateChangedToServerOutput = \case
  -- StateChanged.PeerConnected{..} -> Just $ PeerConnected{..}
  -- StateChanged.PeerDisconnected{..} -> Just $ PeerDisconnected{..}
  -- StateChanged.PeerHandshakeFailure{..} -> Just $ PeerHandshakeFailure{..}
  StateChanged.HeadInitialized{..} -> Just $ HeadIsInitializing{..}
  StateChanged.CommittedUTxO{..} -> Just $ Committed{headId, party, utxo = committedUTxO}
  StateChanged.HeadOpened{headId, initialUTxO} -> Just $ HeadIsOpen{headId, utxo = initialUTxO}
  StateChanged.HeadClosed{..} ->
    Just $
      HeadIsClosed{..}
  StateChanged.HeadContested{..} ->
    Just $
      HeadIsContested{..}
  -- StateChanged.HeadIsReadyToFanout{..} -> Just $ ReadyToFanout{..}
  StateChanged.HeadAborted{..} -> Just $ HeadIsAborted{..}
  StateChanged.HeadFannedOut{..} -> Just $ HeadIsFinalized{..}
  -- StateChanged.CommandFailed{..} -> Just $ CommandFailed{..}
  StateChanged.TransactionAppliedToLocalUTxO{..} -> Just $ TxValid{headId, transactionId = txId tx, transaction = tx}
  -- StateChanged.TxInvalid{..} -> Just $ TxInvalid{..}
  StateChanged.SnapshotConfirmed{..} -> Just $ SnapshotConfirmed{..}
  -- StateChanged.GetUTxOResponse{..} -> Just $ GetUTxOResponse{..}
  -- StateChanged.InvalidInput{..} -> Just $ InvalidInput{..}
  -- StateChanged.Greetings{me, headStatus, hydraHeadId, snapshotUtxo, hydraNodeVersion} -> Just $ Greetings{me, headStatus, hydraHeadId, snapshotUtxo, hydraNodeVersion}
  -- StateChanged.PostTxOnChainFailed{..} -> Just $ PostTxOnChainFailed{..}
  -- StateChanged.IgnoredHeadInitializing{..} -> Just $ IgnoredHeadInitializing{..}
  -- StateChanged.DecommitRequested{..} -> Just $ DecommitRequested{..}
  -- StateChanged.DecommitInvalid{..} -> Just $ DecommitInvalid{..}
  -- StateChanged.DecommitApproved{..} -> Just $ DecommitApproved{..}
  StateChanged.DecommitFinalized{..} -> Just $ DecommitFinalized{..}
  StateChanged.CommitRecorded{..} ->
    Just $
      CommitRecorded{..}
  -- StateChanged.CommitApproved{..} -> Just $ CommitApproved{..}
  StateChanged.CommitFinalized{headId, depositTxId} -> Just $ CommitFinalized{headId, theDeposit = depositTxId}
  StateChanged.CommitRecovered{..} ->
    Just $
      CommitRecovered{..}
  -- StateChanged.CommitIgnored{..} -> Just $ CommitIgnored{..}
  StateChanged.TransactionReceived{} -> Nothing
  StateChanged.DecommitRecorded{} -> Nothing
  StateChanged.SnapshotRequested{} -> Nothing
  StateChanged.SnapshotRequestDecided{} -> Nothing
  StateChanged.PartySignedSnapshot{} -> Nothing
  StateChanged.ChainRolledBack{} -> Nothing
  StateChanged.TickObserved{} -> Nothing

addApiEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
addApiEventSink sink node = node{eventSinks = sink : eventSinks node}
