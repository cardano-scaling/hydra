{-# LANGUAGE DisambiguateRecordFields #-}

module Hydra.Chain.Offline.Persistence (
  initializeStateIfOffline,
  createPersistenceWithUTxOWriteBack,
  createStateChangePersistence,
) where

import Hydra.Prelude

import Hydra.Ledger (IsTx(UTxOType))
import Data.Aeson qualified as Aeson
import Hydra.Cardano.Api (Tx)
import Hydra.Chain (
  ChainEvent (Observation, observedTx),
  ChainStateHistory,
  OnChainTx (OnCommitTx, OnInitTx, contestationPeriod, headId, parties),
  committed,
  initHistory,
  newChainState,
  party,
 )
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadId (HeadId)
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental(PersistenceIncremental, append, loadAll), createPersistenceIncremental)
import Hydra.HeadLogic (StateChanged(SnapshotConfirmed, snapshot))
import Hydra.Snapshot (Snapshot(Snapshot, utxo))
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)
import qualified Data.Aeson as Aeson
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.HeadLogic (StateChanged (SnapshotConfirmed, snapshot))
import Hydra.Ledger (IsTx (UTxOType))
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (PersistenceIncremental, append, loadAll), createPersistenceIncremental)
import Hydra.Snapshot (Snapshot (Snapshot, utxo))
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

initializeStateIfOffline ::
  ChainStateHistory Tx ->
  UTxOType Tx ->
  HeadId ->
  Party ->
  ContestationPeriod ->
  (ChainEvent Tx -> IO ()) ->
  IO ()
initializeStateIfOffline chainStateHistory initialUTxO ownHeadId ownParty contestationPeriod callback = do
  let emptyChainStateHistory = initHistory initialChainState

  -- if we don't have a chainStateHistory to restore from disk from, start a new one
  when (chainStateHistory /= emptyChainStateHistory) $ do
    callback $
      Observation
        { newChainState = initialChainState
        , observedTx =
            OnInitTx
              { headId = ownHeadId
              , parties = [ownParty]
              , contestationPeriod = contestationPeriod
              }
        }

    -- NOTE(Elaine): should be no need to update the chain state, that's L1, there's nothing relevant there
    -- observation events are to construct the L2 we want, with the initial utxo
    callback $
      Observation
        { newChainState = initialChainState
        , observedTx =
            OnCommitTx
              { party = ownParty
              , committed = initialUTxO
              }
        }

createPersistenceWithUTxOWriteBack ::
  (MonadIO m, MonadThrow m) =>
  -- The filepath to write the main state change event persistence to
  FilePath ->
  -- The filepath to write UTxO to. UTxO is written after every confirmed snapshot.
  FilePath ->
  m (PersistenceIncremental (StateChanged Tx) m)
createPersistenceWithUTxOWriteBack persistenceFilePath utxoFilePath = do
  PersistenceIncremental{append, loadAll} <- createPersistenceIncremental persistenceFilePath
  pure
    PersistenceIncremental
      { loadAll
      , append = \stateChange -> do
          append stateChange
          case stateChange of
            SnapshotConfirmed{snapshot = Snapshot{utxo}} ->
              writeBinaryFileDurableAtomic utxoFilePath . toStrict $ Aeson.encode utxo
            _ -> pure ()
      }

createStateChangePersistence ::
  (MonadIO m, MonadThrow m) =>
  -- The filepath to write the main state change event persistence to
  FilePath ->
  -- The optional filepath to write UTxO to. UTxO is written after every confirmed snapshot.
  Maybe FilePath ->
  m (PersistenceIncremental (StateChanged Tx) m)
createStateChangePersistence persistenceFilePath = \case
  Just utxoWriteBackFilePath -> createPersistenceWithUTxOWriteBack persistenceFilePath utxoWriteBackFilePath
  _ -> createPersistenceIncremental persistenceFilePath
