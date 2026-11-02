-- | The general input queue from which the Hydra head is fed with inputs.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  isEmptyTBQueue,
  isFullTBQueue,
  lengthTBQueue,
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
  , tryEnqueue :: e -> m Bool
  -- ^ Admit an advisory input only if there is room, without blocking its producer.
  , queueStats :: m (Natural, Int)
  -- ^ Queued items and parked items, including those awaiting readmission.
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

data Queued a = Queued {queuedId :: Word64, queuedItem :: a, queuedAt :: Time}

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
  releasing <- newLabelledTVarIO "releasing-parked-count" 0
  -- XXX: We bound the _input_ queue by the _logging_ queue size! This is a
  -- hack; but we do it because it seems that the logging queue blocking
  -- prevents further processing, _unless_ the input queue is also bounded.
  -- In truth it probably makes sense for this queue to be bounded anyway.
  -- See: <https://github.com/cardano-scaling/hydra/issues/2442>
  q <- newLabelledTBQueueIO "input-queue" 100
  let write queuedAt queuedItem = do
        queuedId <- readTVar nextId
        writeTBQueue q Queued{queuedId, queuedItem, queuedAt}
        modifyTVar' nextId succ
  pure
    InputQueue
      { enqueue = \item -> do
          now <- getMonotonicTime
          atomically $ write now item
      , tryEnqueue = \item -> do
          now <- getMonotonicTime
          atomically $ do
            full <- isFullTBQueue q
            if full then pure False else True <$ write now item
      , queueStats = atomically $ do
          queued <- lengthTBQueue q
          held <- readTVar parked
          returning <- readTVar releasing
          pure (queued, length held + returning)
      , reenqueue = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . asyncLabelled "input-queue-reenqueue" $ do
            threadDelay delay
            now <- getMonotonicTime
            atomically $ do
              modifyTVar' numThreads pred
              writeTBQueue q e{queuedAt = now}
      , park = \e -> atomically $ modifyTVar' parked (<> [e])
      , releaseParked = do
          ps <- atomically $ do
            items <- swapTVar parked []
            modifyTVar' releasing (+ length items)
            pure items
          -- Write the items back from a separate thread (as 'reenqueue' does),
          -- and one at a time, so a full queue can't deadlock the single
          -- consumer that calls this. 'numThreads' keeps them visible to
          -- 'isEmpty' until they are all back on the queue.
          unless (null ps) $ do
            atomically $ modifyTVar' numThreads succ
            void . asyncLabelled "input-queue-release-parked" $ do
              forM_ ps $ \item -> do
                now <- getMonotonicTime
                atomically $ do
                  writeTBQueue q item{queuedAt = now}
                  modifyTVar' releasing pred
              atomically $ modifyTVar' numThreads pred
      , dequeue =
          atomically $ readTBQueue q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTBQueue q
            pure (isEmpty' && n == 0)
      }
