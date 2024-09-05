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

import Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.Tx.IsTx (ArbitraryIsTx)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

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

-- | Put a list of events to a list of event sinks in a round-robin fashion.
putEventsToSinks :: (Monad m, HasEventId e) => [EventSink e m] -> [e] -> m ()
putEventsToSinks sinks events =
  forM_ events $ \event ->
    forM_ sinks $ \sink ->
      putEvent sink event

-- * State change events as used by Hydra.Node

-- | A state change event with an event id that is the common entity to be
-- loaded from an 'EventSource' and sent to 'EventSink's.
data StateEvent tx = StateEvent
  { eventId :: EventId
  , stateChanged :: StateChanged tx
  }
  deriving (Generic)

instance HasEventId (StateEvent tx) where
  getEventId = eventId

deriving instance IsChainState tx => Show (StateEvent tx)
deriving instance IsChainState tx => Eq (StateEvent tx)
deriving instance IsChainState tx => ToJSON (StateEvent tx)
deriving instance IsChainState tx => FromJSON (StateEvent tx)

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (StateEvent tx) where
  arbitrary = arbitrary >>= genStateEvent
  shrink = genericShrink

instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (StateEvent tx)

genStateEvent :: StateChanged tx -> Gen (StateEvent tx)
genStateEvent sc = StateEvent <$> arbitrary <*> pure sc
