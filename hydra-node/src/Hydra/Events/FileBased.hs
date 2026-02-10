-- | A file-based event source and sink using JSON encoding.
--
-- This is currently used as the main event source and sink in the hydra-node.
module Hydra.Events.FileBased where

import "hydra-prelude" Hydra.Prelude

import "conduit" Conduit (mapMC, (.|))
import "directory" System.Directory (renameFile)
import "hydra-node" Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import "hydra-node" Hydra.Events.Rotation (EventStore (..))
import "hydra-node" Hydra.Persistence (PersistenceIncremental (..))
import "io-classes" Control.Concurrent.Class.MonadSTM (writeTVar)

-- | A basic file based event source and sink defined using a rotated
-- 'PersistenceIncremental' handle.
mkFileBasedEventStore ::
  (ToJSON e, FromJSON e, HasEventId e) =>
  FilePath ->
  PersistenceIncremental e IO ->
  IO (EventStore e IO)
mkFileBasedEventStore stateDir persistence = do
  eventIdV <- newLabelledTVarIO "file-based-event-store-event-id" Nothing
  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId evt = do
      writeTVar eventIdV (Just $ getEventId evt)

    -- Keep track of the last seen event id when loading
    sourceEvents = do
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
      append persistence e
      atomically $ setLastSeenEventId e

    rotate nextLogId checkpointEvent = do
      let rotatedPath = stateDir <> "-" <> show nextLogId
      ( do
          renameFile stateDir rotatedPath
          append persistence checkpointEvent
        )
        `catch` \(_ :: SomeException) -> do
          -- Attempt to revert the rename,
          -- ignoring errors during rollback.
          renameFile rotatedPath stateDir

  pure
    EventStore
      { eventSource = EventSource{sourceEvents}
      , eventSink = EventSink{putEvent}
      , rotate
      }
