-- | The general input queue from which the Hydra head is fed with inputs.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  isEmptyTBQueue,
  isFullTBQueue,
  modifyTVar',
  readTBQueue,
  retry,
  writeTBQueue,
  writeTVar,
 )

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data InputQueue m e = InputQueue
  { enqueue :: e -> m ()
  , tryEnqueue :: e -> m Bool
  -- ^ Non-blocking, coalescing variant of 'enqueue' for periodic/timer
  -- inputs. Returns 'False' (drops the item) if the queue is full OR if the
  -- last enqueued item was also via 'tryEnqueue'. The latter ensures at most
  -- one timer tick is ever pending: consecutive ticks carry no new
  -- information, and keeping only one prevents timer ticks from crowding out
  -- chain/network events. The coalescing resets whenever 'enqueue' is called.
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
  lastWasTimer <- newLabelledTVarIO "last-was-timer" False
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
            writeTVar lastWasTimer False
            queuedId <- readTVar nextId
            writeTBQueue q Queued{queuedId, queuedItem}
            modifyTVar' nextId succ
      , tryEnqueue = \queuedItem ->
          atomically $ do
            alreadyPending <- readTVar lastWasTimer
            full <- isFullTBQueue q
            if alreadyPending || full
              then pure False
              else do
                writeTVar lastWasTimer True
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
            -- When queue is empty but async threads are still running,
            -- block (retry) until threads complete. This prevents
            -- runToCompletion from blocking forever in dequeue when an
            -- OnChainEffect spawns an asyncTracked thread that finishes
            -- without adding items to the queue.
            if isEmpty' && n /= 0
              then retry
              else pure (isEmpty' && n == 0)
      }
