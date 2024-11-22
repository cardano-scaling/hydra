-- | Event source and sink using JSON encoding.
--
-- This serves as an example of how to create an 'EventSource' and 'EventSink'.
module Hydra.Events.FileBased where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..))
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.SqlLitePersistence (PersistenceIncremental (..))

-- | A basic event source and sink defined using an
-- 'PersistenceIncremental' handle.
--
-- The complexity in this implementation mostly stems from the fact that we want
-- to be backward-compatible with the old, plain format of storing
-- 'StateChanged' items directly to disk using 'PersistenceIncremental'.
--
-- If any 'Legacy StateChanged' items are discovered, a running index is used
-- for the 'eventId', while the 'New StateEvent' values are just stored as is.
--
-- A new implementation for an 'EventSource' with a compatible 'EventSink' could
-- be defined more generically with constraints:
--
-- (ToJSON e, FromJSON e, HasEventId) e => (EventSource e m, EventSink e m)
eventPairFromPersistenceIncremental ::
  (IsChainState tx, MonadSTM m) =>
  PersistenceIncremental (PersistedStateChange tx) m ->
  m (EventSource (StateEvent tx) m, EventSink (StateEvent tx) m)
eventPairFromPersistenceIncremental PersistenceIncremental{append, loadAll} = do
  eventIdV <- newTVarIO Nothing
  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId StateEvent{eventId} = do
      writeTVar eventIdV (Just eventId)

    getNextEventId =
      maybe 0 (+ 1) <$> readTVar eventIdV

    -- Keep track of the last seen event id when loading
    getEvents = do
      items <- loadAll
      atomically . forM items $ \i -> do
        event <- case i of
          New e -> pure e
          Legacy sc -> do
            eventId <- getNextEventId
            pure $ StateEvent eventId sc

        setLastSeenEventId event
        pure event

    -- Filter events that are already stored
    putEvent e@StateEvent{eventId} = do
      atomically getLastSeenEventId >>= \case
        Nothing -> store e
        Just lastSeenEventId
          | eventId > lastSeenEventId -> store e
          | otherwise -> pure ()

    store e = do
      append (New e)
      atomically $ setLastSeenEventId e

  pure (EventSource{getEvents}, EventSink{putEvent})

-- | Internal data type used by 'createJSONFileEventSourceAndSink' to be
-- compatible with plain usage of 'PersistenceIncrementa' using plain
-- 'StateChanged' items to the new 'StateEvent' persisted items.
data PersistedStateChange tx
  = Legacy (StateChanged tx)
  | New (StateEvent tx)
  deriving stock (Generic, Show, Eq)

instance IsChainState tx => ToJSON (PersistedStateChange tx) where
  toJSON = \case
    Legacy sc -> toJSON sc
    New e -> toJSON e

instance IsChainState tx => FromJSON (PersistedStateChange tx) where
  parseJSON v =
    New <$> parseJSON v
      <|> Legacy <$> parseJSON v
