module Hydra.PersistentQueue where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Control.Concurrent.Class.MonadSTM (
  modifyTVar',
  newTBQueueIO,
  newTVarIO,
  peekTBQueue,
  readTBQueue,
  writeTBQueue,
 )
import Control.Exception (IOException)
import Data.Aeson (eitherDecode', encode)
import Data.List qualified as List
import System.Directory (createDirectoryIfMissing, doesPathExist, listDirectory, removeFile)
import System.FilePath ((</>))
import UnliftIO.IO.File (writeBinaryFileDurable)

-- * Persistent queue

data PersistentQueue m a = PersistentQueue
  { queue :: TBQueue m (Natural, a)
  , nextIx :: TVar m Natural
  , directory :: FilePath
  }

-- | Create a new persistent queue at file path and given capacity.
newPersistentQueue ::
  (MonadSTM m, MonadIO m, FromCBOR a, MonadCatch m, MonadFail m) =>
  FilePath ->
  Natural ->
  m (PersistentQueue m a)
newPersistentQueue path capacity = do
  queue <- newTBQueueIO capacity
  highestId <-
    try (loadExisting queue) >>= \case
      Left (_ :: IOException) -> do
        liftIO $ createDirectoryIfMissing True path
        pure 0
      Right highest -> pure highest
  nextIx <- newTVarIO $ highestId + 1
  pure PersistentQueue{queue, nextIx, directory = path}
 where
  loadExisting queue = do
    paths <- liftIO $ listDirectory path
    case sort $ mapMaybe readMaybe paths of
      [] -> pure 0
      idxs -> do
        forM_ idxs $ \(idx :: Natural) -> do
          bs <- readFileBS (path </> show idx)
          case decodeFull' bs of
            Left err ->
              fail $ "Failed to decode item: " <> show err
            Right item ->
              atomically $ writeTBQueue queue (idx, item)
        pure $ List.last idxs

-- | Create a new persistent queue at file path and given capacity.
newPersistentQueueJson ::
  (MonadSTM m, MonadIO m, FromJSON a, MonadCatch m, MonadFail m) =>
  FilePath ->
  Natural ->
  m (PersistentQueue m a)
newPersistentQueueJson path capacity = do
  queue <- newTBQueueIO capacity
  highestId <-
    try (loadExisting queue) >>= \case
      Left (_ :: IOException) -> do
        liftIO $ createDirectoryIfMissing True path
        pure 0
      Right highest -> pure highest
  nextIx <- newTVarIO $ highestId + 1
  pure PersistentQueue{queue, nextIx, directory = path}
 where
  loadExisting queue = do
    paths <- liftIO $ listDirectory path
    case sort $ mapMaybe readMaybe paths of
      [] -> pure 0
      idxs -> do
        forM_ idxs $ \(idx :: Natural) -> do
          bs <- readFileLBS (path </> show idx)
          case eitherDecode' bs of
            Left err ->
              fail $ "Failed to decode item: " <> show err
            Right item ->
              atomically $ writeTBQueue queue (idx, item)
        pure $ List.last idxs

-- | Write a value to the queue, blocking if the queue is full.
writeDurablePersistentQueue :: (ToCBOR a, MonadSTM m, MonadIO m) => PersistentQueue m a -> a -> m ()
writeDurablePersistentQueue PersistentQueue{queue, nextIx, directory} item = do
  next <- atomically $ do
    next <- readTVar nextIx
    modifyTVar' nextIx (+ 1)
    pure next
  writeBinaryFileDurable (directory </> show next) $ serialize' item
  atomically $ writeTBQueue queue (next, item)


-- | Write a value to the queue, blocking if the queue is full.
writePersistentQueueJson :: (ToJSON a, MonadSTM m, MonadIO m) => PersistentQueue m a -> a -> m ()
writePersistentQueueJson PersistentQueue{queue, nextIx, directory} item = do
  next <- atomically $ do
    next <- readTVar nextIx
    modifyTVar' nextIx (+ 1)
    pure next

  liftIO $ createDirectoryIfMissing True directory
  writeFileBS (directory </> show next) $
    toStrict (encode item)
  atomically $ writeTBQueue queue (next, item)

-- | Get the next value from the queue without removing it, blocking if the
-- queue is empty.
peekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m (Natural, a)
peekPersistentQueue PersistentQueue{queue} = do
  atomically (peekTBQueue queue)

-- | Remove an element from the queue if it matches the given item. Use
-- 'peekPersistentQueue' to wait for next items before popping it.
popPersistentQueue :: (MonadSTM m, MonadIO m, Eq a) => PersistentQueue m a -> a -> m ()
popPersistentQueue PersistentQueue{queue, directory} item = do
  popped <- atomically $ do
    (ix, next) <- peekTBQueue queue
    if next == item
      then readTBQueue queue $> Just ix
      else pure Nothing
  case popped of
    Nothing -> pure ()
    Just index -> do
      let path = directory </> show index
      fileExists <- liftIO $ doesPathExist path
      when fileExists $
        liftIO $
          removeFile path
