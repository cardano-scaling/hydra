-- | The general input queue from which the Hydra head is fed with inputs.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  isEmptyTBQueue,
  isFullTBQueue,
  modifyTVar',
  readTBQueue,
  writeTBQueue,
 )

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data InputQueue m e = InputQueue
  { enqueue :: e -> m ()
  , tryEnqueue :: e -> m Bool
  -- ^ Non-blocking variant of 'enqueue'. Returns 'False' if the queue is
  -- full and the item was dropped. Use this for periodic/timer inputs that
  -- are safe to skip: a full queue means the main loop is busy processing
  -- other events (chain observations, network messages), so the timer tick
  -- is effectively replaced by the next one that fires after the queue drains.
  , reenqueue :: DiffTime -> Queued e -> m ()
  , asyncTracked :: m () -> m ()
  -- ^ Run an action asynchronously. Use this for
  -- background work that will eventually 'enqueue' an item.
  , dequeue :: m (Queued e)
  , isEmpty :: m Bool
  }

data Queued a = Queued {queuedId :: Word64, queuedItem :: a}

createInputQueue ::
  ( MonadDelay m
  , MonadAsync m
  , MonadLabelledSTM m
  , MonadThrow m
  ) =>
  m (InputQueue m e)
createInputQueue = do
  numThreads <- newLabelledTVarIO "num-threads" (0 :: Integer)
  nextId <- newLabelledTVarIO "nex-id" 0
  -- XXX: We bound the _input_ queue by the _logging_ queue size! This is a
  -- hack; but we do it because it seems that the logging queue blocking
  -- prevents further processing, _unless_ the input queue is also bounded.
  -- In truth it probably makes sense for this queue to be bounded anyway.
  -- See: <https://github.com/cardano-scaling/hydra/issues/2442>
  q <- newLabelledTBQueueIO "input-queue" 100
  pure
    InputQueue
      { enqueue = \queuedItem ->
          atomically $ do
            queuedId <- readTVar nextId
            writeTBQueue q Queued{queuedId, queuedItem}
            modifyTVar' nextId succ
      , tryEnqueue = \queuedItem ->
          atomically $ do
            full <- isFullTBQueue q
            if full
              then pure False
              else do
                queuedId <- readTVar nextId
                writeTBQueue q Queued{queuedId, queuedItem}
                modifyTVar' nextId succ
                pure True
      , reenqueue = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . asyncLabelled "input-queue-reenqueue" $ do
            threadDelay delay
            atomically $ do
              modifyTVar' numThreads pred
              writeTBQueue q e
      , asyncTracked = \action -> do
          atomically $ modifyTVar' numThreads succ
          void . asyncLabelled "input-queue-async-tracked" $
            action `finally` atomically (modifyTVar' numThreads pred)
      , dequeue =
          atomically $ readTBQueue q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTBQueue q
            pure (isEmpty' && n == 0)
      }
