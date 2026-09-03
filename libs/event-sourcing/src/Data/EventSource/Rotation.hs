-- | Log rotation for an 'EventStore': fold the existing events into an
-- aggregate, and once a configured number of events has accumulated, archive
-- them under a log id and start a fresh log from a checkpoint event that
-- captures the aggregate so far.
module Data.EventSource.Rotation (
  RotationConfig (..),
  LogId,
  EventStore (..),
  newRotatedEventStore,
) where

import Conduit (MonadUnliftIO, runConduit, runResourceT, (.|))
import Control.Concurrent.Class.Labelled (newLabelledTVarIO)
import Control.Concurrent.Class.MonadSTM (MonadLabelledSTM, atomically, modifyTVar', readTVarIO, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.Class.MonadTime.SI (MonadTime, getCurrentTime)
import Data.Conduit.Combinators qualified as C
import Data.EventSource (EventId, EventSink (..), EventSource (..), HasEventId (..))
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (UTCTime)
import Numeric.Natural (Natural)

-- | Rotate after this many events have been appended. Must be positive; a
-- value of 0 would trigger a rotation immediately after every checkpoint.
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

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= \b -> when b action

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
            , putEvents = rotatedPutEvents numberOfEventsV aggregateStateV
            }
      , -- NOTE: Don't allow rotation on-demand
        rotate = const . const $ pure ()
      }
 where
  RotateAfter rotateAfterX = config

  aggregateEvents (!n, !_evId, !acc) e = (n + 1, getEventId e, aggregator acc e)

  shouldRotate numberOfEventsV = do
    currentNumberOfEvents <- readTVarIO numberOfEventsV
    -- since rotateAfterX can be any positive number (including 1),
    -- we use (>) instead of (>=) to avoid triggering a rotation immediately after a checkpoint,
    -- which would lead to an infinite loop
    pure $ currentNumberOfEvents > rotateAfterX

  rotatedPutEvent numberOfEventsV aggregateStateV event = do
    putEvent event
    atomically $ do
      -- aggregate new state
      modifyTVar' aggregateStateV (`aggregator` event)
      -- bump numberOfEvents
      modifyTVar' numberOfEventsV (+ 1)
    -- check rotation
    whenM (shouldRotate numberOfEventsV) $ do
      let eventId = getEventId event
      rotateEventLog numberOfEventsV aggregateStateV eventId

  rotatedPutEvents numberOfEventsV aggregateStateV evts = do
    putEvents evts
    atomically $ do
      forM_ evts $ \evt -> do
        modifyTVar' aggregateStateV (`aggregator` evt)
        modifyTVar' numberOfEventsV (+ 1)
    whenM (shouldRotate numberOfEventsV) $ do
      case nonEmpty evts of
        Just ne -> rotateEventLog numberOfEventsV aggregateStateV (getEventId $ NE.last ne)
        Nothing -> pure ()

  rotateEventLog numberOfEventsV aggregateStateV lastEventId = do
    -- build the checkpoint event
    now <- getCurrentTime
    aggregateState <- readTVarIO aggregateStateV
    -- the checkpoint has the same event id as the last event persisted
    let checkpoint = checkpointer aggregateState lastEventId now
    -- the rotated log file name suffix (logId) matches the last event persisted,
    -- while the checkpoint event is appended to the new (current) state log file
    rotate lastEventId checkpoint
    -- reset `numberOfEvents` to 1 because
    -- the checkpoint event was just appended during rotation
    -- and will be sourced from the event store on restart
    atomically $ do
      writeTVar numberOfEventsV 1

  EventStore{eventSource, eventSink = EventSink{putEvent, putEvents}, rotate} = eventStore
