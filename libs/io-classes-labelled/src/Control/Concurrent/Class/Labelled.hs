-- | Thin wrappers over @io-classes@ that attach a label to the STM
-- primitive or thread they create.
--
-- The labels surface in @io-sim@ traces and as GHC thread labels, which
-- makes deadlock and liveness diagnosis tractable. Applying them everywhere
-- a 'TVar'\/'TMVar'\/'TQueue'\/'TBQueue' is allocated or a thread is spawned
-- is a discipline worth keeping; these helpers make it the path of least
-- resistance.
module Control.Concurrent.Class.Labelled (
  -- * Labelling STM variables
  newLabelledTVar,
  newLabelledTVarIO,
  newLabelledEmptyTMVar,
  newLabelledEmptyTMVarIO,
  newLabelledTQueue,
  newLabelledTQueueIO,
  newLabelledTBQueue,
  newLabelledTBQueueIO,

  -- * Labelling threads
  asyncLabelled,
  raceLabelled,
  raceLabelled_,
  withAsyncLabelled,
  concurrentlyLabelled,
  concurrentlyLabelled_,
) where

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  STM,
  TBQueue,
  TMVar,
  TQueue,
  TVar,
  atomically,
  labelTBQueue,
  labelTMVar,
  labelTQueue,
  labelTVar,
  newEmptyTMVar,
  newTBQueue,
  newTQueue,
  newTVar,
 )
import Control.Monad (void)
import Control.Monad.Class.MonadAsync (Async, MonadAsync, async, concurrently, race, withAsync)
import Control.Monad.Class.MonadFork (labelThisThread)
import Numeric.Natural (Natural)

-- * Helpers for labeling TVar

newLabelledTVar :: MonadLabelledSTM m => String -> a -> STM m (TVar m a)
newLabelledTVar lbl val = do
  tv <- newTVar val
  labelTVar tv lbl
  pure tv

newLabelledTVarIO :: MonadLabelledSTM m => String -> a -> m (TVar m a)
newLabelledTVarIO = (atomically .) . newLabelledTVar

-- * Helpers for labeling TMVar

newLabelledEmptyTMVar :: MonadLabelledSTM m => String -> STM m (TMVar m a)
newLabelledEmptyTMVar lbl = do
  tmv <- newEmptyTMVar
  labelTMVar tmv lbl
  pure tmv

newLabelledEmptyTMVarIO :: MonadLabelledSTM m => String -> m (TMVar m a)
newLabelledEmptyTMVarIO = atomically . newLabelledEmptyTMVar

-- * Helpers for labeling TQueue

newLabelledTQueue :: MonadLabelledSTM m => String -> STM m (TQueue m a)
newLabelledTQueue lbl = do
  q <- newTQueue
  labelTQueue q lbl
  pure q

newLabelledTQueueIO :: MonadLabelledSTM m => String -> m (TQueue m a)
newLabelledTQueueIO = atomically . newLabelledTQueue

-- * Helpers for labeling TBQueue

newLabelledTBQueue :: MonadLabelledSTM m => String -> Natural -> STM m (TBQueue m a)
newLabelledTBQueue lbl capacity = do
  bq <- newTBQueue capacity
  labelTBQueue bq lbl
  pure bq

newLabelledTBQueueIO :: MonadLabelledSTM m => String -> Natural -> m (TBQueue m a)
newLabelledTBQueueIO = (atomically .) . newLabelledTBQueue

-- * Helpers for labeling Threads

raceLabelled :: MonadAsync m => (String, m a) -> (String, m b) -> m (Either a b)
raceLabelled (lblA, mA) (lblB, mB) =
  race
    (labelThisThread lblA >> mA)
    (labelThisThread lblB >> mB)

raceLabelled_ :: MonadAsync m => (String, m a) -> (String, m b) -> m ()
raceLabelled_ = (void .) . raceLabelled

withAsyncLabelled :: MonadAsync m => (String, m a) -> (Async m a -> m b) -> m b
withAsyncLabelled (lbl, ma) = withAsync (labelThisThread lbl >> ma)

concurrentlyLabelled :: MonadAsync m => (String, m a) -> (String, m b) -> m (a, b)
concurrentlyLabelled (lblA, mA) (lblB, mB) =
  concurrently
    (labelThisThread lblA >> mA)
    (labelThisThread lblB >> mB)

concurrentlyLabelled_ :: MonadAsync m => (String, m a) -> (String, m b) -> m ()
concurrentlyLabelled_ = (void .) . concurrentlyLabelled

asyncLabelled :: MonadAsync m => String -> m a -> m (Async m a)
asyncLabelled lbl mA = async $ labelThisThread lbl >> mA
