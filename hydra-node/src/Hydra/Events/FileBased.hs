-- | A file-based event source and sink using JSON encoding.
--
-- This is currently used as the emain event source and sink in the hydra-node.
module Hydra.Events.FileBased where

import Hydra.Prelude

import Conduit (mapMC, (.|))
import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import Hydra.Persistence (PersistenceIncremental (..))

-- | A basic file based event source and sink defined using an
-- 'PersistenceIncremental' handle.
eventPairFromPersistenceIncremental ::
  (ToJSON e, FromJSON e, HasEventId e, MonadSTM m) =>
  PersistenceIncremental e m ->
  m (EventSource e m, EventSink e m)
eventPairFromPersistenceIncremental PersistenceIncremental{append, source} = do
  eventIdV <- newTVarIO Nothing
  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId evt = do
      writeTVar eventIdV (Just $ getEventId evt)

    -- Keep track of the last seen event id when loading
    sourceEvents =
      source
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
      append e
      atomically $ setLastSeenEventId e

  pure (EventSource{sourceEvents}, EventSink{putEvent})
