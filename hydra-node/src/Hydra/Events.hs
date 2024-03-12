-- | This module defines the types and functions for creating 'EventSource' and
-- 'EventSink' instances and is intended to be used as an extension point.
--
-- A single 'EventSource' and zero or more 'EventSink' handles are used by the
-- main 'HydraNode' handle to load and send out events.
--
-- TODO: add an example event source sink (on top of the persistence one)
module Hydra.Events where

import Hydra.Prelude

import Hydra.Chain (IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged)

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

-- * State change events as used by Hydra.Node

-- TODO: Move 'StateChanged' here as well?

data StateEvent tx = StateEvent
  { eventId :: EventId
  , stateChanged :: StateChanged tx
  }
  deriving (Generic)

instance HasEventId (StateEvent tx) where
  getEventId = eventId

deriving instance IsChainState tx => Show (StateEvent tx)
deriving instance IsChainState tx => Eq (StateEvent tx)

instance IsChainState tx => Arbitrary (StateEvent tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink

genStateEvent :: StateChanged tx -> Gen (StateEvent tx)
genStateEvent sc = StateEvent <$> arbitrary <*> pure sc
