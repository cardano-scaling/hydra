-- | The general input queue from which the Hydra head is fed with inputs.
-- This implementation uses a priority queue system to ensure protocol messages
-- (ReqSn, AckSn) are processed before transaction messages under high load.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  isEmptyTQueue,
  modifyTVar',
  readTQueue,
  tryReadTQueue,
  writeTQueue,
 )
import Hydra.HeadLogic.Input (MessagePriority (..))

-- | The input queue system with priority support. High priority messages
-- (protocol messages like ReqSn, AckSn) are processed before low priority
-- messages (transaction requests) to ensure snapshot progress under high load.
--
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data InputQueue m e = InputQueue
  { enqueue :: MessagePriority -> e -> m ()
  , reenqueue :: MessagePriority -> DiffTime -> Queued e -> m ()
  , dequeue :: m (Queued e)
  , isEmpty :: m Bool
  }

data Queued a = Queued {queuedId :: Word64, queuedItem :: a}

-- | Create an input queue with priority support. The queue maintains two
-- internal queues: one for high priority messages (protocol) and one for
-- low priority messages (transactions). Dequeue always tries high priority
-- first before falling back to low priority.
createInputQueue ::
  ( MonadDelay m
  , MonadAsync m
  , MonadLabelledSTM m
  ) =>
  m (InputQueue m e)
createInputQueue = do
  numThreads <- newLabelledTVarIO "num-threads" (0 :: Integer)
  nextId <- newLabelledTVarIO "next-id" 0
  -- Two separate queues for priority handling
  highPriorityQueue <- newLabelledTQueueIO "input-queue-high"
  lowPriorityQueue <- newLabelledTQueueIO "input-queue-low"
  pure
    InputQueue
      { enqueue = \priority queuedItem ->
          atomically $ do
            queuedId <- readTVar nextId
            let queued = Queued{queuedId, queuedItem}
            case priority of
              HighPriority -> writeTQueue highPriorityQueue queued
              LowPriority -> writeTQueue lowPriorityQueue queued
            modifyTVar' nextId succ
      , reenqueue = \priority delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . asyncLabelled "input-queue-reenqueue" $ do
            threadDelay delay
            atomically $ do
              modifyTVar' numThreads pred
              case priority of
                HighPriority -> writeTQueue highPriorityQueue e
                LowPriority -> writeTQueue lowPriorityQueue e
      , dequeue =
          -- Always try high priority first, then fall back to low priority
          atomically $ do
            mHigh <- tryReadTQueue highPriorityQueue
            case mHigh of
              Just item -> pure item
              Nothing -> readTQueue lowPriorityQueue
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isHighEmpty <- isEmptyTQueue highPriorityQueue
            isLowEmpty <- isEmptyTQueue lowPriorityQueue
            pure (isHighEmpty && isLowEmpty && n == 0)
      }
