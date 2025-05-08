module Hydra.Events.Rotation where

import Hydra.Prelude

import Conduit (MonadUnliftIO)
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import Hydra.Chain.ChainState (ChainStateType, IsChainState)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId, LogId, StateEvent (..), getEvents)
import Hydra.HeadLogic (StateChanged (Checkpoint), aggregate)
import Hydra.HeadLogic.State (HeadState (..), IdleState (..))

newtype RotationConfig = RotateAfter Natural

-- | An EventSource and EventSink combined
type EventStore e m = (EventSource e m, EventSink e m)

type Checkpointer e = [e] -> e

-- | Creates an event store that rotates according to given config and 'Checkpointer'.
mkRotatedEventStore ::
  (HasEventId e, MonadSTM m, MonadUnliftIO m) =>
  RotationConfig ->
  Checkpointer e ->
  LogId ->
  EventStore e m ->
  m (EventStore e m)
mkRotatedEventStore config checkpointer logId eventStore = do
  logIdV <- newTVarIO logId
  -- Rules for any event store:
  -- - sourceEvents will be called in the beginning of the application and whenever the api server wans to load history
  --   -> might be called multiple times!!
  -- - putEvent will be called on application start with all events returned by sourceEvents and during processing
  currentEvents <- getEvents eventSource
  let currentNumberOfEvents = toInteger $ length currentEvents
  numberOfEventsV <- newTVarIO currentNumberOfEvents
  -- XXX: check rotation on startup
  when (currentNumberOfEvents >= toInteger rotateAfterX) $ do
    rotateEventLog logIdV numberOfEventsV
  pure
    ( EventSource
        { sourceEvents = rotatedSourceEvents
        }
    , EventSink
        { putEvent = rotatedPutEvent logIdV numberOfEventsV
        , -- NOTE: Don't allow rotation on-demand
          rotate = const . const $ pure ()
        }
    )
 where
  RotateAfter rotateAfterX = config
  -- TODO: if this turns out to be equal to sourceEvents, then the whole algorithm can just work on each 'EventSink'
  rotatedSourceEvents = sourceEvents eventSource

  rotatedPutEvent logIdV numberOfEventsV event = do
    putEvent event
    -- XXX: bump numberOfEvents
    numberOfEvents' <- atomically $ do
      numberOfEvents <- readTVar numberOfEventsV
      let numberOfEvents' = numberOfEvents + 1
      writeTVar numberOfEventsV numberOfEvents'
      pure numberOfEvents'
    -- XXX: check rotation
    when (numberOfEvents' >= toInteger rotateAfterX) $ do
      rotateEventLog logIdV numberOfEventsV

  rotateEventLog logIdV numberOfEventsV = do
    -- XXX: build checkpoint event
    history <- getEvents eventSource
    let checkpoint = checkpointer history
    -- XXX: rotate with checkpoint
    currentLogId <- readTVarIO logIdV
    let currentLogId' = currentLogId + 1
    rotate currentLogId' checkpoint
    -- XXX: clear numberOfEvents + bump logId
    atomically $ do
      writeTVar numberOfEventsV 0
      writeTVar logIdV currentLogId'

  (eventSource, EventSink{putEvent, rotate}) = eventStore

prepareRotatedEventStore ::
  (IsChainState tx, MonadTime m, MonadSTM m, MonadUnliftIO m) =>
  RotationConfig ->
  ChainStateType tx ->
  EventStore (StateEvent tx) m ->
  m (EventStore (StateEvent tx) m)
prepareRotatedEventStore rotationConfig initialChainState eventStore = do
  now <- getCurrentTime
  let checkpointer = mkChechpointer initialChainState now
  -- FIXME!
  let logId = 0
  mkRotatedEventStore rotationConfig checkpointer logId eventStore

mkChechpointer :: IsChainState tx => ChainStateType tx -> UTCTime -> Checkpointer (StateEvent tx)
mkChechpointer initialChainState time events =
  StateEvent
    { eventId = maybe 0 (succ . last) (nonEmpty $ (\StateEvent{eventId} -> eventId) <$> events)
    , stateChanged =
        Checkpoint . foldl' aggregate initialState $
          (\StateEvent{stateChanged} -> stateChanged) <$> events
    , time
    }
 where
  initialState = Idle IdleState{chainState = initialChainState}
