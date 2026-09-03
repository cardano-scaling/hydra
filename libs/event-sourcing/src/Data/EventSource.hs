{-# LANGUAGE UndecidableInstances #-}

-- | The types and functions for creating 'EventSource' and 'EventSink'
-- handles, intended to be used as an extension point.
--
-- A single 'EventSource' and zero or more 'EventSink' handles let an
-- application load and emit an ordered, identified event stream.
-- 'Data.EventSource.SQLite' is a ready-made implementation.
module Data.EventSource where

import Conduit (ConduitT, MonadUnliftIO, ResourceT, runResourceT, sourceToList)
import Control.Monad (forM_)
import Data.Word (Word64)

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

data EventSink e m = EventSink
  { putEvent :: HasEventId e => e -> m ()
  -- ^ Send a single event to the event sink.
  , putEvents :: HasEventId e => [e] -> m ()
  -- ^ Send a batch of events to the event sink.
  }

-- | Create an 'EventSink' from a single-event function, with a default
-- sequential batch implementation.
mkEventSink :: Monad m => (HasEventId e => e -> m ()) -> EventSink e m
mkEventSink putOne =
  EventSink
    { putEvent = putOne
    , putEvents = mapM_ putOne
    }

-- | Put a list of events to a list of event sinks, batching per sink.
putEventsToSinks :: (Monad m, HasEventId e) => [EventSink e m] -> [e] -> m ()
putEventsToSinks sinks events =
  forM_ sinks $ \sink ->
    putEvents sink events
