-- | Handles to save/load files across the hydra-node using sql-lite.
-- We use a simple JSON encoding and two modes of operation to store things: Full and Incremental.
module Hydra.SqlLitePersistence where

import Hydra.Prelude

import Conduit (ConduitT, ResourceT, yield)
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (newTMVarIO, putTMVar, takeTMVar),
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
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory)

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
  liftIO $ createDirectoryIfMissing True $ takeDirectory fp
  conn <- liftIO $ open fp
  liftIO $ do
    execute_ conn "pragma journal_mode = WAL;"
    execute_ conn "pragma synchronous = normal;"
    execute_ conn "pragma journal_size_limit = 6144000;"
    execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB);"
  connVar <- liftIO $ newTMVarIO conn
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
  , source :: FromJSON a => ConduitT () a (ResourceT m) ()
  -- ^ Stream all elements.
  , closeDb :: m ()
  , dropDb :: m ()
  }

createPersistenceIncremental ::
  forall a m.
  MonadIO m =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory fp
  conn <- liftIO $ open fp
  liftIO $ do
    execute_ conn "pragma journal_mode = WAL;"
    execute_ conn "pragma synchronous = normal;"
    execute_ conn "pragma journal_size_limit = 6144000;"
    execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB);"
  connVar <- liftIO $ newTMVarIO conn
  let
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

withPersistenceIncremental :: FilePath -> (PersistenceIncremental a IO -> IO b) -> IO ()
withPersistenceIncremental fp action = do
  incPersistence@PersistenceIncremental{closeDb} <- createPersistenceIncremental fp
  void $ action incPersistence
  closeDb
