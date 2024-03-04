-- | This module defines the types and functions for creating Hydra
-- 'EventSource' and 'EventSink' instances and is intended to be used as an
-- extension point. A single 'EventSource' and zero or more 'EventSink' handles
-- are used by the main 'HydraNode' handle to load and send out events.
module Hydra.Events where

import Hydra.Prelude

-- TODO: Need to add a concept of identification to 'e' as used in these
-- definitions to enable deduplication etc.

newtype EventSource e m = EventSource {getEvents' :: m [e]}

newtype EventSink e m = EventSink {putEvent' :: e -> m ()}

putEventToSinks :: Monad m => [EventSink e m] -> e -> m ()
putEventToSinks sinks e = forM_ sinks (\sink -> putEvent' sink e)

putEventsToSinks :: Monad m => [EventSink e m] -> [e] -> m ()
putEventsToSinks sinks es = forM_ es (\e -> putEventToSinks sinks e)

type EventID = Word64

-- FIXME(Elaine): we have to figure out a better taxonomy/nomenclature for the events/statechange stuff
-- the eventID here is not the same as the eventID in Queued, that one is more fickle and influenced by non state change events
-- this one is only incremented when we have a new state change event
