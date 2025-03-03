-- | A file-based event source and sink using JSON encoding.
--
-- This serves as an example of how to create an 'EventSource' and 'EventSink'.
module Hydra.Events.FileBased where

import Hydra.Prelude

import Conduit (mapMC, (.|))
import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..))
import Hydra.Persistence (PersistenceIncremental (..))

-- | A basic file based event source and sink defined using an
-- 'PersistenceIncremental' handle.
--
-- A new implementation for an 'EventSource' with a compatible 'EventSink' could
-- be defined more generically with constraints:
--
-- (ToJSON e, FromJSON e, HasEventId) e => (EventSource e m, EventSink e m)
eventPairFromPersistenceIncremental ::
  (IsChainState tx, MonadSTM m) =>
  PersistenceIncremental (StateEvent tx) m ->
  m (EventSource (StateEvent tx) m, EventSink (StateEvent tx) m)
eventPairFromPersistenceIncremental PersistenceIncremental{append, source} = do
  eventIdV <- newTVarIO Nothing
  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId StateEvent{eventId} = do
      writeTVar eventIdV (Just eventId)

    -- Keep track of the last seen event id when loading
    sourceEvents =
      source
        .| mapMC
          ( \event -> lift . atomically $ do
              setLastSeenEventId event
              pure event
          )

    -- Filter events that are already stored
    putEvent e@StateEvent{eventId} = do
      atomically getLastSeenEventId >>= \case
        Nothing -> store e
        Just lastSeenEventId
          | eventId > lastSeenEventId -> store e
          | otherwise -> pure ()

    store e = do
      append e
      atomically $ setLastSeenEventId e

  pure (EventSource{sourceEvents}, EventSink{putEvent})
