-- | The general input queue from which the Hydra head is fed with inputs.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  isEmptyTQueue,
  modifyTVar',
  readTQueue,
  writeTQueue,
 )

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data InputQueue m e = InputQueue
  { enqueue :: e -> m ()
  , reenqueue :: DiffTime -> Queued e -> m ()
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
  q <- newLabelledTQueueIO "input-queue"
  pure
    InputQueue
      { enqueue = \queuedItem ->
          atomically $ do
            queuedId <- readTVar nextId
            writeTQueue q Queued{queuedId, queuedItem}
            modifyTVar' nextId succ
      , reenqueue = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . asyncLabelled "input-queue-reenqueue" $ do
            threadDelay delay
            atomically $ do
              modifyTVar' numThreads pred
              writeTQueue q e
      , dequeue =
          atomically $ readTQueue q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTQueue q
            pure (isEmpty' && n == 0)
      }
