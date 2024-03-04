-- | This module defines the types and functions for creating 'EventSource' and
-- 'EventSink' instances and is intended to be used as an extension point.
--
-- A single 'EventSource' and zero or more 'EventSink' handles are used by the
-- main 'HydraNode' handle to load and send out events.
--
-- TODO: add an example event source sink (on top of the persistence one)
module Hydra.Events where

import Hydra.Prelude

-- FIXME(Elaine): we have to figure out a better taxonomy/nomenclature for the events/statechange stuff
-- the eventID here is not the same as the eventID in Queued, that one is more fickle and influenced by non state change events
-- this one is only incremented when we have a new state change event
type EventId = Word64

class HasEventId a where
  getEventId :: a -> EventId

instance HasEventId (EventId, a) where
  getEventId = fst

newtype EventSource e m = EventSource
  { getEvents :: HasEventId e => m [e]
  -- ^ Retrieve all events from the event source.
  }

newtype EventSink e m = EventSink
  { putEvent :: HasEventId e => e -> m ()
  -- ^ Send a single event to the event sink.
  }

putEventToSinks :: (Monad m, HasEventId e) => [EventSink e m] -> e -> m ()
putEventToSinks sinks e = forM_ sinks $ \sink -> putEvent sink e

putEventsToSinks :: (Monad m, HasEventId e) => [EventSink e m] -> [e] -> m ()
putEventsToSinks sinks = mapM_ (putEventToSinks sinks)
