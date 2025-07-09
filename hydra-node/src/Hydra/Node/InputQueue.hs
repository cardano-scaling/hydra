-- | The general input queue from which the Hydra head is fed with inputs.
module Hydra.Node.InputQueue where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  isEmptyTBQueue,
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
import Hydra.PersistentQueue (
  PersistentQueue (..),
  newPersistentQueue,
  peekPersistentQueue,
  popPersistentQueue,
  writePersistentQueue,
 )
import System.FilePath ((</>))

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data InputQueue m e = InputQueue
  { enqueue :: e -> m ()
  , reenqueue :: DiffTime -> Queued e -> m ()
  , dequeue :: m (Queued e)
  , isEmpty :: m Bool
  , done :: e -> m ()
  }

data Queued a = Queued {queuedId :: Word64, queuedItem :: a}

createInputQueue ::
  ( MonadDelay m
  , MonadAsync m
  , MonadLabelledSTM m
  ) =>
  m (InputQueue m e)
createInputQueue = do
  numThreads <- newTVarIO (0 :: Integer)
  nextId <- newTVarIO 0
  labelTVarIO numThreads "num-threads"
  q <- atomically newTQueue
  labelTQueueIO q "input-queue"
  pure
    InputQueue
      { enqueue = \queuedItem ->
          atomically $ do
            queuedId <- readTVar nextId
            writeTQueue q Queued{queuedId, queuedItem}
            modifyTVar' nextId succ
      , reenqueue = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . async $ do
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
      , done = \_ -> pure ()
      }

createPersistentInputQueue ::
  ( MonadDelay m
  , MonadIO m
  , MonadSTM m
  , MonadCatch m
  , MonadFail m
  , Eq e
  , ToCBOR e
  , FromCBOR e
  ) =>
  FilePath ->
  m (InputQueue m e)
createPersistentInputQueue persistenceDir = do
  q <- newPersistentQueue (persistenceDir </> "input-queue") 1000
  pure
    InputQueue
      { enqueue = writePersistentQueue q
      , reenqueue = \delay e -> do
          threadDelay delay
          writePersistentQueue q (queuedItem e)
      , dequeue = do
          (n, item) <- peekPersistentQueue q
          pure $ Queued{queuedId = fromIntegral n, queuedItem = item}
      , isEmpty = atomically $ isEmptyTBQueue (queue q)
      , done = popPersistentQueue q
      }
