-- | Handles to save/load files across the hydra-node using sql-lite.
-- We use a simple JSON encoding and two modes of operation to store things: Full and Incremental.
module Hydra.SqlLitePersistence where

import Hydra.Prelude

import Conduit (ConduitT, ResourceT, yield)
import Control.Concurrent.Class.MonadSTM (
  newTMVarIO,
  newTVarIO,
  putTMVar,
  readTVarIO,
  takeTMVar,
  writeTVar,
 )
import Data.Aeson qualified as Aeson
import Data.Char (isDigit)
import Database.SQLite.Simple (
  Connection,
  Only (..),
  close,
  execute,
  execute_,
  open,
  query_,
 )
import System.Directory (createDirectoryIfMissing, getFileSize, removeFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension)

data PersistenceException
  = PersistenceException String
  | IncorrectAccessException String
  deriving stock (Eq, Show)

instance Exception PersistenceException

-- | Handle to save and load files to/from db using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  , closeDb :: m ()
  , dropDb :: m ()
  }

-- TODO! split read-write connection
createConnectionV :: FilePath -> IO (TMVar IO Connection)
createConnectionV fp = do
  createDirectoryIfMissing True $ takeDirectory fp
  conn <- open fp
  execute_ conn "PRAGMA journal_mode = WAL;"
  execute_ conn "PRAGMA busy_timeout = 5000;"
  execute_ conn "PRAGMA synchronous = normal;"
  execute_ conn "PRAGMA journal_size_limit = 6144000;"
  execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB);"
  newTMVarIO conn

withConn :: TMVar IO Connection -> (Connection -> IO b) -> IO b
withConn connVar action = do
  bracket
    (atomically $ takeTMVar connVar)
    (atomically . putTMVar connVar)
    action

checkpointDb :: FilePath -> IO ()
checkpointDb fp = do
  connVar <- liftIO $ createConnectionV fp
  withConn connVar $ \c ->
    execute_ c "PRAGMA wal_checkpoint(TRUNCATE);"
  withConn connVar close

createPersistence ::
  MonadIO m =>
  FilePath ->
  m (Persistence a m)
createPersistence fp = do
  connVar <- liftIO $ createConnectionV fp
  pure $
    Persistence
      { save = \a -> liftIO $ withConn connVar $ \c ->
          execute c "INSERT INTO items (msg) VALUES (?)" (Only $ Aeson.encode a)
      , load = liftIO $ withConn connVar $ \c -> do
          r <- query_ c "SELECT msg FROM items ORDER BY id DESC LIMIT 1"
          case r of
            [] -> pure Nothing
            (Only result : _) -> pure $ Aeson.decode result
      , closeDb =
          liftIO $
            withConn connVar close
      , dropDb = liftIO $ do
          withConn connVar close
          removeFile fp
      }

-- | Handle to save incrementally and load files to/from db using JSON encoding.
data PersistenceIncremental a m = PersistenceIncremental
  { append :: ToJSON a => a -> m ()
  , appendMany :: ToJSON a => [a] -> m ()
  , loadAll :: FromJSON a => m [a]
  , countAll :: m Int
  , source :: FromJSON a => ConduitT () a (ResourceT m) ()
  -- ^ Stream all elements.
  , closeDb :: m ()
  , dropDb :: m ()
  }

createPersistenceIncremental ::
  forall a m.
  MonadIO m =>
  FilePath ->
  m (PersistenceIncremental a IO)
createPersistenceIncremental fp = do
  connVar <- liftIO $ createConnectionV fp
  pure $
    PersistenceIncremental
      { append = \a -> liftIO $
          withConn connVar $ \c ->
            execute c "INSERT INTO items (msg) VALUES (?)" (Only $ Aeson.encode a)
      , appendMany = \items -> liftIO $
          withConn connVar $ \c -> do
            execute_ c "BEGIN;"
            forM_ items $ \item ->
              execute c "INSERT INTO items (msg) VALUES (?)" (Only $ Aeson.encode item)
            execute_ c "COMMIT;"
      , loadAll = liftIO $
          withConn connVar $ \c -> do
            rows <- query_ c "SELECT msg FROM items ORDER BY id ASC;"
            pure $ mapMaybe (Aeson.decode . (\(Only b) -> b)) rows
      , countAll = liftIO $
          withConn connVar $ \c -> do
            [Only n] <- query_ c "SELECT COUNT(*) FROM items;"
            pure n
      , source = do
          rows <- liftIO $
            withConn connVar $ \c ->
              query_ c "SELECT msg FROM items ORDER BY id ASC;"
          mapM_ (maybe (pure ()) yield . Aeson.decode . (\(Only b) -> b)) rows
      , closeDb =
          liftIO $
            withConn connVar close
      , dropDb = liftIO $ do
          withConn connVar close
          removeFile fp
      }

data Checkpointer a m = Checkpointer
  { countRate :: Int
  , fileCondition :: FilePath -> m Bool
  , checkpoint :: [a] -> m a
  }

withPersistenceIncremental ::
  (FromJSON a, ToJSON a) =>
  FilePath ->
  ([a] -> IO a) ->
  (PersistenceIncremental a IO -> IO b) ->
  IO ()
withPersistenceIncremental fp checkpointer action = do
  eventLog@PersistenceIncremental{closeDb} <- createRotatedEventLog fp checkpointerHandle
  void $ action eventLog
  closeDb
 where
  checkpointerHandle =
    Checkpointer
      { countRate = 3000
      , checkpoint = checkpointer
      , fileCondition = \filePath -> do
          fileSize <- liftIO $ getFileSize filePath
          -- XXX: 100MB threshold
          pure (fileSize > 100 * 1024 * 1024)
      }

createRotatedEventLog ::
  (FromJSON a, ToJSON a) =>
  FilePath ->
  Checkpointer a IO ->
  IO (PersistenceIncremental a IO)
createRotatedEventLog fpInitial checkpointer = do
  fpV <- newTVarIO fpInitial
  eventLogV <- newTVarIO =<< createPersistenceIncremental fpInitial
  PersistenceIncremental{countAll} <- readTVarIO eventLogV
  initialCount <- countAll
  eventCountV <- newTVarIO initialCount
  pure
    PersistenceIncremental
      { append = \e -> do
          PersistenceIncremental{append = append'} <- readTVarIO eventLogV
          append' e
          nextCount eventCountV
          checkRotation fpV eventCountV eventLogV checkpointer
      , appendMany = \es -> do
          PersistenceIncremental{appendMany = appendMany'} <- readTVarIO eventLogV
          appendMany' es
          forM_ es (const $ nextCount eventCountV)
          checkRotation fpV eventCountV eventLogV checkpointer
      , loadAll = do
          PersistenceIncremental{loadAll = loadAll'} <- readTVarIO eventLogV
          loadAll'
      , source = do
          PersistenceIncremental{source = source'} <- liftIO (readTVarIO eventLogV)
          source'
      , countAll = do
          PersistenceIncremental{countAll = countAll'} <- readTVarIO eventLogV
          countAll'
      , closeDb = do
          PersistenceIncremental{closeDb = closeDb'} <- readTVarIO eventLogV
          closeDb'
      , dropDb = do
          PersistenceIncremental{dropDb = dropDb'} <- readTVarIO eventLogV
          dropDb'
      }

checkRotation ::
  (FromJSON a, ToJSON a, MonadIO m, MonadSTM m) =>
  TVar m FilePath ->
  TVar m Int ->
  TVar m (PersistenceIncremental a IO) ->
  Checkpointer a IO ->
  m ()
checkRotation fpV eventCountV eventLogV Checkpointer{countRate, fileCondition, checkpoint} = do
  -- XXX: every `countRate` events we trigger rotation if needed.
  eventCount' <- readTVarIO eventCountV
  when (eventCount' > countRate) $ do
    fp <- readTVarIO fpV
    triggerRotation <- liftIO $ fileCondition fp
    when triggerRotation $ do
      rotateEventLog fpV eventLogV checkpoint
    -- XXX: we reset the counter to avoid checking on every new event after `countRate`.
    atomically $ writeTVar eventCountV 0

nextCount :: MonadSTM m => TVar m Int -> m ()
nextCount countV = atomically $ do
  count <- readTVar countV
  let count' = count + 1
  writeTVar countV count'

-- Rotate the event log to a new database file starting from a checkpoint event,
-- derived from known history.
--
-- Then, perform a WAL checkpoint to flush and truncate the write-ahead log,
-- ensuring all changes are persisted to the main database file and the WAL file is reset.
rotateEventLog ::
  (FromJSON a, ToJSON a, MonadIO m, MonadSTM m) =>
  TVar m FilePath ->
  TVar m (PersistenceIncremental a IO) ->
  ([a] -> IO a) ->
  m ()
rotateEventLog fpV eventLogV checkpointer = do
  PersistenceIncremental{closeDb, loadAll} <- readTVarIO eventLogV
  fp <- readTVarIO fpV
  fp' <- rotateFp fpV
  eventLog' <- createPersistenceIncremental fp'
  -- XXX: checkpoint on new event log before rotation
  liftIO $ do
    history <- loadAll
    -- FIXME! swap with above
    closeDb
    checkpoint <- checkpointer history
    append eventLog' checkpoint
  -- XXX: rotate event log
  atomically $ writeTVar eventLogV eventLog'
  -- XXX: By default, SQLite automatically triggers a checkpoint when the WAL file
  -- reaches 1000 pages or when the last connection to the database is closed.
  -- However, in scenarios with custom 'journal_size_limit' settings or
  -- long-lived connections, relying solely on automatic checkpoints may not
  -- suffice. Therefore, an explicit 'PRAGMA wal_checkpoint(TRUNCATE);' is
  -- executed to force a checkpoint and truncate the WAL file, ensuring that
  -- the WAL does not grow indefinitely and that the database remains
  -- consistent.
  liftIO $ checkpointDb fp

rotateFp :: MonadSTM m => TVar m FilePath -> m FilePath
rotateFp fpV = atomically $ do
  fp <- readTVar fpV
  let baseName = takeBaseName fp
      extension = takeExtension fp
      baseName' = rotateIndex baseName
      fp' = baseName' ++ extension
  writeTVar fpV fp'
  pure fp'

rotateIndex :: String -> String
rotateIndex baseName =
  if rotationIndex == 0
    then extractPrefix baseName ++ "-" ++ show (rotationIndex + 1)
    else extractPrefix baseName ++ show (rotationIndex + 1)
 where
  extractSuffix = takeWhile (/= '-') . reverse
  extractRotationIndex = fromMaybe (0 :: Int) . readMaybe . extractSuffix
  rotationIndex = extractRotationIndex baseName
  extractPrefix = reverse . dropWhile isDigit . reverse
