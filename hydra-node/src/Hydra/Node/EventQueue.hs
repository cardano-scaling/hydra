-- | The general event queue from which the Hydra head is fed with events.
module Hydra.Node.EventQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  isEmptyTQueue,
  labelTQueueIO,
  labelTVarIO,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  writeTQueue,
 )
import Control.Monad.Class.MonadAsync (async)

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , putEventAfter :: DiffTime -> Queued e -> m ()
  , nextEvent :: m (Queued e)
  , isEmpty :: m Bool
  }

data Queued e = Queued {eventId :: Word64, queuedEvent :: e}

createEventQueue ::
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadLabelledSTM m
  ) =>
  m (EventQueue m e)
createEventQueue = do
  numThreads <- newTVarIO (0 :: Integer)
  nextId <- newTVarIO 0
  labelTVarIO numThreads "num-threads"
  q <- atomically newTQueue
  labelTQueueIO q "event-queue"
  pure
    EventQueue
      { putEvent = \queuedEvent ->
          atomically $ do
            eventId <- readTVar nextId
            writeTQueue q Queued{eventId, queuedEvent}
            modifyTVar' nextId succ
      , putEventAfter = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . async $ do
            threadDelay delay
            atomically $ do
              modifyTVar' numThreads pred
              writeTQueue q e
      , nextEvent =
          atomically $ readTQueue q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTQueue q
            pure (isEmpty' && n == 0)
      }
