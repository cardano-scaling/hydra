-- | A file-based event source and sink using JSON encoding.
--
-- This is currently used as the main event source and sink in the hydra-node.
module Hydra.Events.FileBased where

import Hydra.Prelude

import Conduit (mapMC, (.|))
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import Hydra.Events.Rotation (EventStore)
import Hydra.Persistence (PersistenceIncremental (..))

-- | A basic file based event source and sink defined using a rotated
-- 'PersistenceIncremental' handle.
mkFileBasedEventStore ::
  (ToJSON e, FromJSON e, HasEventId e) =>
  FilePath ->
  (FilePath -> IO (PersistenceIncremental e IO)) ->
  IO (EventStore e IO)
mkFileBasedEventStore fp mkPersistenceIncremental = do
  persistenceV <- newTVarIO =<< mkPersistenceIncremental fp
  eventIdV <- newTVarIO Nothing
  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId evt = do
      writeTVar eventIdV (Just $ getEventId evt)

    -- Keep track of the last seen event id when loading
    sourceEvents = do
      persistence <- liftIO (readTVarIO persistenceV)
      source persistence
        .| mapMC
          ( \event -> lift . atomically $ do
              setLastSeenEventId event
              pure event
          )

    -- Filter events that are already stored
    putEvent evt = do
      atomically getLastSeenEventId >>= \case
        Nothing -> store evt
        Just lastSeenEventId
          | getEventId evt > lastSeenEventId -> store evt
          | otherwise -> pure ()

    store e = do
      persistence <- readTVarIO persistenceV
      append persistence e
      atomically $ setLastSeenEventId e

    rotate nextLogId checkpointEvt = do
      let fp' = fp <> "-" <> show nextLogId
      persistence' <- mkPersistenceIncremental fp'
      append persistence' checkpointEvt
      atomically $ writeTVar persistenceV persistence'

  pure (EventSource{sourceEvents}, EventSink{putEvent, rotate})
