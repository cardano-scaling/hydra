-- | A crash-safe, file-backed, bounded FIFO queue.
--
-- Each item is written to its own file (named by a monotonic index) before it
-- enters the in-memory 'TBQueue', and removed from disk only after it has been
-- popped, so a restart reloads exactly the items that were enqueued but not yet
-- consumed (at-least-once delivery). Items carry their CBOR serialization,
-- produced once on write and reused, and can be drained in byte- and
-- count-bounded batches with an in-flight pin for safe retries.
module Control.Concurrent.PersistentQueue (
  PersistentQueue,
  PersistentQueueLog (..),
  newPersistentQueue,
  writePersistentQueue,
  peekPersistentQueue,
  tryPeekPersistentQueue,
  peekBatchPersistentQueue,
  popBatchPersistentQueue,
  nextPendingBatch,
) where

import Cardano.Binary (FromCBOR, ToCBOR, decodeFull', serialize')
import Control.Concurrent.Class.Labelled (newLabelledTBQueueIO, newLabelledTVarIO)
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  MonadSTM,
  TBQueue,
  TVar,
  atomically,
  isFullTBQueue,
  modifyTVar',
  peekTBQueue,
  readTBQueue,
  readTVar,
  readTVarIO,
  tryPeekTBQueue,
  tryReadTBQueue,
  unGetTBQueue,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Class.MonadThrow (MonadCatch, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

-- | Events emitted while operating the queue.
data PersistentQueueLog
  = PersistentQueueLoadFailed {reason :: Text}
  | PersistentQueueFull
  | PersistentQueueDeleteFailed {index :: Natural, reason :: Text}
  deriving stock (Eq, Show)

-- | Queue elements carry the item's CBOR serialization, produced once on
-- write and reused for both the on-disk file and downstream consumers, so
-- consuming does not serialize twice.
data PersistentQueue m a = PersistentQueue
  { queue :: TBQueue m (Natural, a, ByteString)
  , nextIx :: TVar m Natural
  , directory :: FilePath
  }

readFileBS :: MonadIO m => FilePath -> m ByteString
readFileBS = liftIO . BS.readFile

writeFileBS :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBS path = liftIO . BS.writeFile path

-- | Create a new persistent queue at file path and given capacity.
newPersistentQueue ::
  (MonadLabelledSTM m, MonadIO m, FromCBOR a, MonadCatch m, MonadFail m) =>
  Tracer IO PersistentQueueLog ->
  FilePath ->
  Natural ->
  m (PersistentQueue m a)
newPersistentQueue tracer path capacity = do
  paths <- liftIO $ do
    createDirectoryIfMissing True path
    sort . mapMaybe readMaybe <$> listDirectory path
  queue <- newLabelledTBQueueIO "persistent-queue" $ max (fromIntegral $ length paths) capacity
  highestId <-
    try (loadExisting queue paths) >>= \case
      Left (e :: IOException) -> do
        liftIO $ do
          traceWith tracer PersistentQueueLoadFailed{reason = Text.pack (show e)}
          createDirectoryIfMissing True path
        pure 0
      Right highest -> pure highest
  nextIx <- newLabelledTVarIO "persistent-next-ix" $ highestId + 1
  pure PersistentQueue{queue, nextIx, directory = path}
 where
  loadExisting queue = \case
    [] -> pure 0
    idxs -> do
      forM_ idxs $ \(idx :: Natural) -> do
        bs <- readFileBS (path </> show idx)
        case decodeFull' bs of
          Left err ->
            fail $ "Failed to decode item: " <> show err
          Right item ->
            atomically $ writeTBQueue queue (idx, item, bs)
      pure $ last idxs

-- | Write a value to the queue, blocking if the queue is full.
writePersistentQueue :: (ToCBOR a, MonadSTM m, MonadIO m) => Tracer IO PersistentQueueLog -> PersistentQueue m a -> a -> m ()
writePersistentQueue tracer PersistentQueue{queue, nextIx, directory} item = do
  next <- atomically $ do
    next <- readTVar nextIx
    modifyTVar' nextIx (+ 1)
    pure next
  let !bytes = serialize' item
  writeFileBS (directory </> show next) bytes
  full <- atomically $ isFullTBQueue queue
  when full $ liftIO $ traceWith tracer PersistentQueueFull
  atomically $ writeTBQueue queue (next, item, bytes)

-- | Get the next value from the queue without removing it, blocking if the
-- queue is empty.
peekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m a
peekPersistentQueue PersistentQueue{queue} = do
  (\(_, item, _) -> item) <$> atomically (peekTBQueue queue)

-- | Like 'peekPersistentQueue', but returns 'Nothing' instead of blocking
-- when the queue is empty.
tryPeekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m (Maybe a)
tryPeekPersistentQueue PersistentQueue{queue} = do
  fmap (\(_, item, _) -> item) <$> atomically (tryPeekTBQueue queue)

-- | Get all pending values and their serializations, up to the given count
-- and total byte limits, blocking until at least one is available. Values
-- are not removed; use 'popBatchPersistentQueue' after they were sent.
peekBatchPersistentQueue :: MonadSTM m => PersistentQueue m a -> Int -> Int -> m [(a, ByteString)]
peekBatchPersistentQueue PersistentQueue{queue} maxCount maxBytes = atomically $ do
  first' <- readTBQueue queue
  -- Collected in reverse consumption order
  rest <- go (remainingAfter first') []
  -- Restore everything we consumed: 'unGetTBQueue' pushes to the front, so
  -- restoring newest-first re-establishes the original queue order.
  forM_ (rest <> [first']) $ unGetTBQueue queue
  pure $ (\(_, item, bytes) -> (item, bytes)) <$> (first' : reverse rest)
 where
  go budget acc
    | length acc >= maxCount - 1 = pure acc
    | otherwise =
        tryReadTBQueue queue >>= \case
          Nothing -> pure acc
          Just next@(_, _, bytes)
            | BS.length bytes > budget -> do
                unGetTBQueue queue next
                pure acc
            | otherwise -> go (budget - BS.length bytes) (next : acc)

  remainingAfter (_, _, bytes) = maxBytes - BS.length bytes

-- | Get the batch to broadcast next: the batch already in flight if there is
-- one, otherwise a fresh one peeked from the queue (and recorded as in
-- flight). Returns 'Nothing' when nothing is pending. The caller must clear
-- the in-flight var after popping a successfully sent batch.
--
-- Pinning the in-flight batch across transient retries matters for consumers
-- with an at-least-once send that can commit while the caller sees a
-- transient failure: the retry must send (and afterwards pop) exactly the
-- content of the committed attempt. Re-peeking on retry could pick up
-- messages enqueued in the meantime, and a dedup that declared the grown
-- batch delivered would pop and lose the never-sent tail.
nextPendingBatch ::
  MonadSTM m =>
  TVar m (Maybe [(a, ByteString)]) ->
  PersistentQueue m a ->
  Int ->
  Int ->
  m (Maybe [(a, ByteString)])
nextPendingBatch inFlightVar queue maxCount maxBytes =
  readTVarIO inFlightVar >>= \case
    Just batch -> pure (Just batch)
    Nothing ->
      tryPeekPersistentQueue queue >>= \case
        Nothing -> pure Nothing
        Just _ -> do
          batch <- peekBatchPersistentQueue queue maxCount maxBytes
          atomically $ writeTVar inFlightVar (Just batch)
          pure (Just batch)

-- | Remove a batch previously returned by 'peekBatchPersistentQueue'. Pops
-- unconditionally, one item per batch entry: the caller is expected to be the
-- sole consumer, so the queue head still holds exactly the peeked items (an
-- item-matching guard could only silently no-op and wedge the queue).
popBatchPersistentQueue :: (MonadSTM m, MonadIO m) => Tracer IO PersistentQueueLog -> PersistentQueue m a -> [(a, ByteString)] -> m ()
popBatchPersistentQueue tracer PersistentQueue{queue, directory} batch = do
  indices <- atomically $ forM batch $ \_ -> (\(ix, _, _) -> ix) <$> readTBQueue queue
  forM_ indices $ removeQueueFile tracer directory

-- | Delete the backing file of a popped queue item. Failing to delete is
-- traced but not fatal: the item was already consumed, so a leftover file
-- only means it may be re-delivered after a restart (at-least-once delivery,
-- same as the crash-recovery path).
removeQueueFile :: MonadIO m => Tracer IO PersistentQueueLog -> FilePath -> Natural -> m ()
removeQueueFile tracer directory ix =
  liftIO $
    removeFile (directory </> show ix) `catch` \e ->
      unless (isDoesNotExistError e) $
        traceWith tracer PersistentQueueDeleteFailed{index = ix, reason = Text.pack (show e)}
