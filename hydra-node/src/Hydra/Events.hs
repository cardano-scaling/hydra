{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the types and functions for creating 'EventSource' and
-- 'EventSink' instances and is intended to be used as an extension point.
--
-- A single 'EventSource' and zero or more 'EventSink' handles are used by the
-- main 'HydraNode' handle to load and send out events.
--
-- See 'Hydra.Events.FileBased' for an example implementation and
-- 'Hydra.Events.FileBasedSpec' for the corresponding test suite.
--
-- Custom implementations should be located under Hydra.Events to avoid
-- conflicts.
module Hydra.Events where

import "hydra-prelude" Hydra.Prelude
import "conduit" Conduit (ConduitT, MonadUnliftIO, ResourceT, runResourceT, sourceToList)

type EventId = Word64

class HasEventId a where
  getEventId :: a -> EventId

instance HasEventId Word64 where
  getEventId = id

newtype EventSource e m = EventSource
  { sourceEvents :: HasEventId e => ConduitT () e (ResourceT m) ()
  -- ^ Stream all events from the event source.
  }

-- | Retrieve all events from the event source as a list.
getEvents :: (HasEventId e, MonadUnliftIO m) => EventSource e m -> m [e]
getEvents EventSource{sourceEvents} = runResourceT $ sourceToList sourceEvents

newtype EventSink e m = EventSink
  { putEvent :: HasEventId e => e -> m ()
  -- ^ Send a single event to the event sink.
  }

-- | Put a list of events to a list of event sinks in a round-robin fashion.
putEventsToSinks :: (Monad m, HasEventId e) => [EventSink e m] -> [e] -> m ()
putEventsToSinks sinks events =
  forM_ events $ \event ->
    forM_ sinks $ \sink ->
      putEvent sink event
