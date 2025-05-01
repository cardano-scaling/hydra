module Hydra.Events.Rotation where

import Control.Concurrent.Class.MonadSTM (newTVarIO)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId)
import Hydra.Prelude

newtype RotationConfig = RotateAfter Natural

-- | An EventSource and EventSink combined
type EventStore e m = (EventSource e m, EventSink e m)

type Checkpointer e = [e] -> e

-- | Creates an event store that rotates according to given config and 'Checkpointer'.
-- FIXME: this is not rotating obviously
newRotatedEventStore ::
  (HasEventId e, MonadSTM m) =>
  RotationConfig -> Checkpointer e -> EventStore e m -> m (EventStore e m)
newRotatedEventStore config checkpoint eventStore = do
  -- Rules for any event store:
  -- - sourceEvents will be called in the beginning of the application and whenever the api server wans to load history
  --   -> might be called multiple times!!
  -- - putEvent will be called on application start with all events returned by sourceEvents and during processing
  numberOfEvents <- newTVarIO 0
  pure
    ( EventSource
        { sourceEvents = rotatedSourceEvents
        }
    , EventSink
        { putEvent = rotatedPutEvent numberOfEvents
        , -- NOTE: Don't allow rotation on-demand
          rotate = const $ pure ()
        }
    )
 where
  -- TODO: if this turns out to be equal to sourceEvents, then the whole algorithm can just work on each 'EventSink'
  rotatedSourceEvents = do
    undefined sourceEvents

  rotatedPutEvent numberOfEvents event = do
    -- TODO: bump up numberOfEvents
    undefined putEvent
    -- TODO: sometimes rotate
    undefined rotate
    -- TODO: then clear numberOfEvents
    -- TODO: write checkpoint into sink (or make rotate take a checkpoint)
    pure ()

  (EventSource{sourceEvents}, EventSink{putEvent, rotate}) = eventStore
