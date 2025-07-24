module Hydra.PersistentQueue where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Control.Concurrent.Class.MonadSTM (
  isEmptyTBQueue,
  isFullTBQueue,
  modifyTVar',
  newTBQueueIO,
  newTVarIO,
  peekTBQueue,
  readTBQueue,
  readTVarIO,
  writeTBQueue,
 )
import Control.Exception (IOException)
import Data.List qualified as List
import System.Directory (createDirectoryIfMissing, doesPathExist, listDirectory, removeFile, renameFile)
import System.FilePath ((</>))
import System.Random (randomIO)
import UnliftIO.IO.File (writeBinaryFileDurable)

-- * Persistent queue

data PersistentQueue m a = PersistentQueue
  { queue :: TBQueue m (Natural, a)
  , nextIx :: TVar m Natural
  , directory :: FilePath
  , encode :: a -> ByteString
  , decode :: ByteString -> Either String a
  }

-- | Create a new persistent queue at file path and given capacity.
newPersistentQueue ::
  (MonadSTM m, MonadIO m, FromCBOR a, MonadFail m) =>
  -- | encode message
  (a -> ByteString) ->
  -- | decode message
  (ByteString -> Either String a) ->
  FilePath ->
  m (PersistentQueue m a)
newPersistentQueue encode decode path = do
  pathExists <- liftIO $ doesPathExist path
  let defaultCapacity :: Int = 100
  (paths, capacity) <-
    if pathExists
      then do
        paths <- liftIO $ listDirectory path
        pure (paths, max (length paths) defaultCapacity)
      else do
        liftIO $ createDirectoryIfMissing True path
        pure ([], defaultCapacity)
  queue <- newTBQueueIO $ fromIntegral capacity
  highestId <- loadExisting queue paths
  nextIx <- newTVarIO $ highestId + 1
  pure PersistentQueue{queue, nextIx, directory = path, encode, decode}
 where
  loadExisting queue paths = do
    case sort $ mapMaybe readMaybe paths of
      [] -> pure 0
      idxs -> do
        forM_ idxs $ \(idx :: Natural) -> do
          let filePath = path </> show idx
          bs <- readFileBS filePath
          case decodeFull' bs of
            Left err ->
              fail $ "Failed to decode item: " <> show err <> " Path: " <> filePath
            Right item ->
              atomically $ writeTBQueue queue (idx, item)
        pure $ List.last idxs

-- | Write a value to the queue, blocking if the queue is full.
-- TODO: refactor this with writePersistentQueue
writeDurablePersistentQueue :: (ToCBOR a, MonadSTM m, MonadIO m, MonadDelay m) => PersistentQueue m a -> a -> m ()
writeDurablePersistentQueue pq@PersistentQueue{queue, nextIx, directory} item = do
  liftIO $ createDirectoryIfMissing True directory
  next <- readTVarIO nextIx
  tempId :: Int <- liftIO $ abs <$> randomIO
  let tempFilePath = directory </> ("temp-" ++ show tempId)
      finalFilePath = directory </> show next
  liftIO $ writeBinaryFileDurable tempFilePath $ serialize' item
  success <- atomically $ do
    full <- isFullTBQueue queue
    if full
      then return False
      else do
        modifyTVar' nextIx (+ 1)
        writeTBQueue queue (next, item)
        return True
  if success
    then liftIO $ renameFile tempFilePath finalFilePath
    else do
      liftIO $ removeFile tempFilePath
      threadDelay 1
      writeDurablePersistentQueue pq item

-- | Writes an item to a persistent queue, ensuring durability by writing to disk
-- before adding to the in-memory queue.
--
-- The item is first written to a temporary file, then atomically added to the
-- 'TBQueue' with its index. If the queue is full, the temporary file is removed,
-- and the operation retries after a short delay. The temporary file is renamed to
-- its final name (based on the index) only after the queue operation succeeds,
-- avoiding file-locking conflicts.
writePersistentQueue :: (ToCBOR a, MonadSTM m, MonadIO m, MonadDelay m) => PersistentQueue m a -> a -> m ()
writePersistentQueue pq@PersistentQueue{queue, nextIx, directory} item = do
  liftIO $ createDirectoryIfMissing True directory
  next <- readTVarIO nextIx
  tempId :: Int <- liftIO $ abs <$> randomIO
  let tempFilePath = directory </> ("temp-" ++ show tempId)
      finalFilePath = directory </> show next
  liftIO $ writeFileBS tempFilePath $ serialize' item
  success <- atomically $ do
    full <- isFullTBQueue queue
    if full
      then return False
      else do
        modifyTVar' nextIx (+ 1)
        writeTBQueue queue (next, item)
        return True
  if success
    then liftIO $ renameFile tempFilePath finalFilePath
    else do
      liftIO $ removeFile tempFilePath
      threadDelay 1
      writePersistentQueue pq item

-- | Get the next value from the queue without removing it, blocking if the
-- queue is empty.
peekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m (Natural, a)
peekPersistentQueue PersistentQueue{queue} = do
  atomically (peekTBQueue queue)

-- | Attempts to remove a specific item from the head of a persistent queue and
-- deletes its corresponding file from disk.
--
-- Checks if the item at the head of the 'TBQueue' matches the provided item. If it
-- matches, the item is atomically removed from the queue, and its file is deleted.
-- If the queue is empty or the item does not match, no action is taken. All queue
-- operations (checking emptiness, peeking, and popping) are performed in a single
-- atomic transaction to ensure consistency.
popPersistentQueue :: (MonadSTM m, MonadIO m, Eq a) => PersistentQueue m a -> a -> m ()
popPersistentQueue PersistentQueue{queue, directory} item = do
  popped <- atomically $ do
    emptyQueue <- isEmptyTBQueue queue
    if emptyQueue
      then pure Nothing
      else do
        (ix, next) <- peekTBQueue queue
        if next == item
          then do
            _ <- readTBQueue queue
            pure (Just ix)
          else pure Nothing
  case popped of
    Nothing -> pure ()
    Just index -> do
      let path = directory </> show index
      result <- liftIO $ try $ removeFile path
      case result of
        Left (e :: IOException) -> putStrLn $ "Error removing file " ++ path ++ ": " ++ show e
        Right () -> pure ()
