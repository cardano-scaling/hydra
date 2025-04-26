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
import System.FilePath (takeBaseName, takeDirectory)

data PersistenceException
  = PersistenceException String
  | IncorrectAccessException String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Handle to save and load files to/from db using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  , closeDb :: m ()
  , dropDb :: m ()
  }

-- FIXME!
withConn :: TMVar IO Connection -> (Connection -> IO b) -> IO b
withConn connVar action = do
  conn' <- atomically $ takeTMVar connVar
  result <- action conn'
  atomically $ putTMVar connVar conn'
  pure result

createPersistence ::
  MonadIO m =>
  FilePath ->
  m (Persistence a m)
createPersistence fp = do
  connVar <- liftIO $ do
    createDirectoryIfMissing True $ takeDirectory fp
    conn <- open fp
    execute_ conn "pragma journal_mode = WAL;"
    execute_ conn "pragma synchronous = normal;"
    execute_ conn "pragma journal_size_limit = 6144000;"
    execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB);"
    newTMVarIO conn
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
  connVar <- liftIO $ do
    createDirectoryIfMissing True $ takeDirectory fp
    conn <- open fp
    execute_ conn "pragma journal_mode = WAL;"
    execute_ conn "pragma synchronous = normal;"
    execute_ conn "pragma journal_size_limit = 6144000;"
    execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB);"
    newTMVarIO conn
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

withRotatedEventLog ::
  (FromJSON a, ToJSON a) =>
  FilePath ->
  ([a] -> IO a) ->
  (PersistenceIncremental a IO -> IO b) ->
  IO ()
withRotatedEventLog fp checkpointer action = do
  eventLog@PersistenceIncremental{closeDb} <- createRotatedEventLog fp checkpointer
  void $ action eventLog
  closeDb

createRotatedEventLog ::
  (FromJSON a, ToJSON a) =>
  FilePath ->
  ([a] -> IO a) ->
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
          checkRotation fpV eventCountV eventLogV checkpointer
      , appendMany = \es -> do
          PersistenceIncremental{appendMany = appendMany'} <- readTVarIO eventLogV
          appendMany' es
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
  ([a] -> IO a) ->
  m ()
checkRotation fpV eventCountV eventLogV checkpointer = do
  -- XXX: every 10k events we trigger rotation if needed.
  eventCount <- nextCount eventCountV
  when (eventCount > 10000) $ do
    triggerRotation <- shouldRotate fpV
    when triggerRotation $ do
      rotateEventLog fpV eventLogV checkpointer
    -- XXX: we reset the counter to avoid checking on every new event after 10k.
    atomically $ writeTVar eventCountV 0

nextCount :: MonadSTM m => TVar m Int -> m Int
nextCount countV = atomically $ do
  count <- readTVar countV
  let count' = count + 1
  writeTVar countV count'
  pure count'

shouldRotate :: (MonadIO m, MonadSTM m) => TVar m FilePath -> m Bool
shouldRotate fpV = do
  fp <- readTVarIO fpV
  fileSize <- liftIO $ getFileSize fp
  -- XXX: 100MB threshold
  pure (fileSize > 100 * 1024 * 1024)

extractRotationIndex :: FilePath -> Int
extractRotationIndex fp =
  let baseName = takeBaseName fp
      rotationIndex = takeWhile (/= '-') (reverse baseName)
   in fromMaybe 0 (readMaybe rotationIndex)

rotateFp :: MonadSTM m => TVar m FilePath -> m FilePath
rotateFp fpV = atomically $ do
  fp <- readTVar fpV
  let rotationIndex = extractRotationIndex fp
  let fp' = fp ++ "-" ++ show rotationIndex ++ ".db"
  writeTVar fpV fp'
  pure fp'

rotateEventLog ::
  (FromJSON a, ToJSON a, MonadIO m, MonadSTM m) =>
  TVar m FilePath ->
  TVar m (PersistenceIncremental a IO) ->
  ([a] -> IO a) ->
  m ()
rotateEventLog fpV eventLogV checkpointer = do
  -- XXX: rotate event log
  PersistenceIncremental{closeDb, loadAll} <- readTVarIO eventLogV
  fp' <- rotateFp fpV
  eventLog' <- createPersistenceIncremental fp'
  atomically $ writeTVar eventLogV eventLog'
  -- XXX: append checkpoint
  liftIO $ do
    events <- loadAll
    closeDb
    checkpoint <- checkpointer events
    append eventLog' checkpoint
