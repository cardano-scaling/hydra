module Hydra.Events.Rotation where

import Hydra.Prelude

import Conduit (ConduitT, MonadUnliftIO, ResourceT, runConduit, runResourceT, (.|))
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import Data.Conduit (await)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventId, EventSink (..), EventSource (..), HasEventId (..), LogId, StateEvent (..))
import Hydra.HeadLogic (StateChanged (..), aggregate)
import Hydra.HeadLogic.State (HeadState)

newtype RotationConfig = RotateAfter Natural

-- | An EventSource and EventSink combined
data EventStore e m
  = EventStore
  { eventSource :: EventSource e m
  , eventSink :: EventSink e m
  , rotate :: LogId -> e -> m ()
  -- ^ Rotate existing events into a given log id and start a new log from given e.
  }

type StateAggregate s e = s -> e -> s

type StateCheckpointer s e = s -> EventId -> UTCTime -> e

mkAggregator :: IsChainState tx => StateAggregate (HeadState tx) (StateEvent tx)
mkAggregator s StateEvent{stateChanged} = aggregate s stateChanged

mkCheckpointer :: StateCheckpointer (HeadState tx) (StateEvent tx)
mkCheckpointer headState eventId time =
  StateEvent
    { eventId
    , stateChanged = Checkpoint headState
    , time
    }

foldEvents ::
  (Monad m, HasEventId e) =>
  StateAggregate s e ->
  s ->
  ConduitT e Void (ResourceT m) (Natural, EventId, s)
foldEvents aggregator = go 0 0
 where
  go !n !evId !acc =
    await >>= \case
      Nothing -> pure (n, evId, acc)
      Just e ->
        go (n + 1) (getEventId e) (aggregator acc e)

-- | Creates an event store that rotates according to given config and 'StateAggregate'.
newRotatedEventStore ::
  (HasEventId e, MonadSTM m, MonadUnliftIO m, MonadTime m) =>
  RotationConfig ->
  s ->
  StateAggregate s e ->
  StateCheckpointer s e ->
  EventStore e m ->
  m (EventStore e m)
newRotatedEventStore config s0 aggregator checkpointer eventStore = do
  (currentNumberOfEvents, lastEventId, currentAggregateState) <-
    runResourceT . runConduit $
      sourceEvents eventSource .| foldEvents aggregator s0
  aggregateStateV <- newTVarIO currentAggregateState
  numberOfEventsV <- newTVarIO currentNumberOfEvents
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

  shouldRotate numberOfEventsV = do
    currentNumberOfEvents <- readTVarIO numberOfEventsV
    pure $ currentNumberOfEvents >= rotateAfterX

  rotatedPutEvent numberOfEventsV aggregateStateV event = do
    putEvent event
    atomically $ do
      -- aggregate new state
      aggregateState <- readTVar aggregateStateV
      let aggregateState' = aggregator aggregateState event
      writeTVar aggregateStateV aggregateState'
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
    -- FIXME! use timestamp instead
    let nextLogId = lastEventId + 1
    rotate nextLogId checkpoint
    -- clear numberOfEvents + bump logId
    atomically $ do
      writeTVar numberOfEventsV 0

  EventStore{eventSource, eventSink = EventSink{putEvent}, rotate} = eventStore
