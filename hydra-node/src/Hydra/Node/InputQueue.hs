-- | The general input queue from which the Hydra head is fed with inputs.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  isEmptyTBQueue,
  modifyTVar',
  readTBQueue,
  swapTVar,
  writeTBQueue,
 )

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data InputQueue m e = InputQueue
  { enqueue :: e -> m ()
  , reenqueue :: DiffTime -> Queued e -> m ()
  , park :: Queued e -> m ()
  -- ^ Hold an item aside without re-enqueuing it, until 'releaseParked' moves
  -- it back onto the queue. Used for inputs that can't be processed until the
  -- node is in sync. Parked items do not count towards 'isEmpty'.
  , releaseParked :: m ()
  -- ^ Move all parked items back onto the queue, preserving their order.
  , dequeue :: m (Queued e)
  , isEmpty :: m Bool
  }

data Queued a = Queued {queuedId :: Word64, queuedItem :: a}

createInputQueue ::
  ( MonadDelay m
  , MonadAsync m
  , MonadLabelledSTM m
  ) =>
  m (InputQueue m e)
createInputQueue = do
  numThreads <- newLabelledTVarIO "num-threads" (0 :: Integer)
  nextId <- newLabelledTVarIO "nex-id" 0
  parked <- newLabelledTVarIO "parked-queue" []
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
      , reenqueue = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . asyncLabelled "input-queue-reenqueue" $ do
            threadDelay delay
            atomically $ do
              modifyTVar' numThreads pred
              writeTBQueue q e
      , park = \e -> atomically $ modifyTVar' parked (<> [e])
      , releaseParked = do
          ps <- atomically $ swapTVar parked []
          -- Write the items back from a separate thread (as 'reenqueue' does),
          -- and one at a time, so a full queue can't deadlock the single
          -- consumer that calls this. 'numThreads' keeps them visible to
          -- 'isEmpty' until they are all back on the queue.
          unless (null ps) $ do
            atomically $ modifyTVar' numThreads succ
            void . asyncLabelled "input-queue-release-parked" $ do
              forM_ ps $ atomically . writeTBQueue q
              atomically $ modifyTVar' numThreads pred
      , dequeue =
          atomically $ readTBQueue q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTBQueue q
            pure (isEmpty' && n == 0)
      }
