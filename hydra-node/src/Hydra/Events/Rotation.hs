module Hydra.Events.Rotation where

import Hydra.Prelude

import Conduit (MonadUnliftIO, runConduit, runResourceT, (.|))
import Control.Concurrent.Class.MonadSTM (MonadLabelledSTM, modifyTVar', readTVarIO, writeTVar)
import Data.Conduit.Combinators qualified as C
import Hydra.Events (EventId, EventSink (..), EventSource (..), HasEventId (..))

newtype RotationConfig = RotateAfter Natural

type LogId = EventId

-- | An EventSource and EventSink combined
data EventStore e m
  = EventStore
  { eventSource :: EventSource e m
  , eventSink :: EventSink e m
  , rotate :: LogId -> e -> m ()
  -- ^ Rotate existing events into a given log id and start a new log from given e.
  }

-- | Creates an event store that rotates according to given config and 'StateAggregate'.
newRotatedEventStore ::
  (HasEventId e, MonadUnliftIO m, MonadTime m, MonadLabelledSTM m) =>
  RotationConfig ->
  -- | Starting state of aggregate
  s ->
  -- | Update aggregate state
  (s -> e -> s) ->
  -- | Create a checkpoint event
  (s -> EventId -> UTCTime -> e) ->
  EventStore e m ->
  m (EventStore e m)
newRotatedEventStore config s0 aggregator checkpointer eventStore = do
  (currentNumberOfEvents, lastEventId, currentAggregateState) <-
    runResourceT . runConduit $
      sourceEvents eventSource .| C.foldl aggregateEvents (0, 0, s0)
  aggregateStateV <- newLabelledTVarIO "rotated-event-store-aggregate-state" currentAggregateState
  numberOfEventsV <- newLabelledTVarIO "rotated-event-store-number-of-events" currentNumberOfEvents
  -- check rotation on startup
  whenM (shouldRotate numberOfEventsV) $ do
    rotateEventLog numberOfEventsV aggregateStateV lastEventId
  pure
    EventStore
      { eventSource
      , eventSink =
          EventSink
            { putEvent = rotatedPutEvent numberOfEventsV aggregateStateV
            }
      , -- NOTE: Don't allow rotation on-demand
        rotate = const . const $ pure ()
      }
 where
  RotateAfter rotateAfterX = config

  aggregateEvents (!n, !_evId, !acc) e = (n + 1, getEventId e, aggregator acc e)

  shouldRotate numberOfEventsV = do
    currentNumberOfEvents <- readTVarIO numberOfEventsV
    pure $ currentNumberOfEvents >= rotateAfterX

  rotatedPutEvent numberOfEventsV aggregateStateV event = do
    putEvent event
    atomically $ do
      -- aggregate new state
      modifyTVar' aggregateStateV (`aggregator` event)
      -- bump numberOfEvents
      numberOfEvents <- readTVar numberOfEventsV
      let numberOfEvents' = numberOfEvents + 1
      writeTVar numberOfEventsV numberOfEvents'
    -- check rotation
    whenM (shouldRotate numberOfEventsV) $ do
      let eventId = getEventId event
      rotateEventLog numberOfEventsV aggregateStateV eventId

  rotateEventLog numberOfEventsV aggregateStateV lastEventId = do
    -- build checkpoint event
    now <- getCurrentTime
    aggregateState <- readTVarIO aggregateStateV
    let checkpoint = checkpointer aggregateState (lastEventId + 1) now
    -- rotate with checkpoint
    rotate lastEventId checkpoint
    -- clear numberOfEvents + bump logId
    atomically $ do
      writeTVar numberOfEventsV 0

  EventStore{eventSource, eventSink = EventSink{putEvent}, rotate} = eventStore
