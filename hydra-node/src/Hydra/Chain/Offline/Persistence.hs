{-# LANGUAGE DisambiguateRecordFields #-}
module Hydra.Chain.Offline.Persistence (
  initializeStateIfOffline
, createPersistenceWithUTxOWriteBack
, createStateChangePersistence) where

import Hydra.Prelude

import Hydra.Chain (
  ChainStateHistory, ChainEvent (observedTx, Observation), newChainState, committed, party, initHistory, OnChainTx (OnInitTx, OnCommitTx, headId, contestationPeriod, parties),
 )
import Hydra.Ledger (IsTx(UTxOType))
import Hydra.Cardano.Api (Tx)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.ContestationPeriod (fromChain)
import Hydra.HeadId (HeadId)
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental(PersistenceIncremental, append, loadAll), createPersistenceIncremental)
import Hydra.HeadLogic (StateChanged(SnapshotConfirmed, snapshot))
import Hydra.Snapshot (Snapshot(Snapshot, utxo))
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)
import qualified Data.Aeson as Aeson

initializeStateIfOffline :: ChainStateHistory Tx
  -> UTxOType Tx
  -> HeadId
  -> Party
  -> (ChainEvent Tx -> IO ())
  -> IO ()
initializeStateIfOffline chainStateHistory initialUTxO ownHeadId ownParty callback = do
    let emptyChainStateHistory = initHistory initialChainState
    
    -- if we don't have a chainStateHistory to restore from disk from, start a new one
    when (chainStateHistory /= emptyChainStateHistory) $ do
      callback $ Observation { newChainState = initialChainState, observedTx =
        OnInitTx
        { headId = ownHeadId
        , parties = [ownParty]
        , contestationPeriod = fromChain $ contestationPeriodFromDiffTime (10) --TODO(Elaine): we should be able to set this to 0
        } }

      --NOTE(Elaine): should be no need to update the chain state, that's L1, there's nothing relevant there
      -- observation events are to construct the L2 we want, with the initial utxo
      callback $ Observation { newChainState = initialChainState, observedTx =
        OnCommitTx 
        { party = ownParty
        , committed = initialUTxO
        } }

      -- TODO(Elaine): I think onInitialChainCommitTx in update will take care of posting a collectcom transaction since we shouldn't have any peers
    -- callback $ Observation { newChainState = initialChainState, observedTx = OnCollectComTx }

createPersistenceWithUTxOWriteBack ::
  (MonadIO m, MonadThrow m) =>
  FilePath ->
  FilePath ->
  m (PersistenceIncremental (StateChanged Tx) m)
createPersistenceWithUTxOWriteBack persistenceFilePath utxoFilePath = do
  PersistenceIncremental{append, loadAll} <- createPersistenceIncremental persistenceFilePath
  pure PersistenceIncremental { loadAll, append = \stateChange -> do
    append stateChange
    case stateChange of 
      SnapshotConfirmed { snapshot = Snapshot{utxo} } ->
        writeBinaryFileDurableAtomic utxoFilePath . toStrict $ Aeson.encode utxo
      _ -> pure ()
  }

createStateChangePersistence :: (MonadIO m, MonadThrow m) => FilePath -> Maybe FilePath -> m (PersistenceIncremental (StateChanged Tx) m)
createStateChangePersistence persistenceFilePath = \case
  Just utxoWriteBackFilePath -> createPersistenceWithUTxOWriteBack persistenceFilePath utxoWriteBackFilePath
  _ -> createPersistenceIncremental persistenceFilePath