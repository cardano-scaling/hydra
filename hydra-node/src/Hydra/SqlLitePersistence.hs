-- | Handles to save/load files across the hydra-node using sql-lite.
-- We use a simple JSON encoding and two modes of operation to store things: Full and Incremental.
module Hydra.SqlLitePersistence where

import Hydra.Prelude

import Conduit (ConduitT, ResourceT, yield)
import Data.Aeson qualified as Aeson
import Database.SQLite.Simple (
  Only (..),
  execute,
  execute_,
  query_,
  withConnection,
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
  , dropDb :: m ()
  }

createPersistence ::
  MonadIO m =>
  FilePath ->
  m (Persistence a m)
createPersistence fp = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory fp
  liftIO $ withConnection fp $ \conn -> do
    execute_ conn "pragma journal_mode = WAL;"
    execute_ conn "pragma synchronous = normal;"
    execute_ conn "pragma journal_size_limit = 6144000;"
    execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB)"
  pure $
    Persistence
      { save = \a -> liftIO $ withConnection fp $ \conn' ->
          execute conn' "INSERT INTO items (msg) VALUES (?)" (Only $ Aeson.encode a)
      , load = liftIO $ withConnection fp $ \conn' -> do
          r <- query_ conn' "SELECT msg FROM items ORDER BY id DESC LIMIT 1"
          case r of
            [] -> pure Nothing
            (Only result : _) -> pure $ Aeson.decode result
      , dropDb = liftIO $ removeFile fp
      }

-- | Handle to save incrementally and load files to/from db using JSON encoding.
data PersistenceIncremental a m = PersistenceIncremental
  { append :: ToJSON a => a -> m ()
  , appendMany :: ToJSON a => [a] -> m ()
  , loadAll :: FromJSON a => m [a]
  , source :: FromJSON a => ConduitT () a (ResourceT m) ()
  -- ^ Stream all elements.
  , dropDb :: m ()
  }

createPersistenceIncremental ::
  forall a m.
  MonadIO m =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory fp
  liftIO $ withConnection fp $ \conn -> do
    execute_ conn "pragma journal_mode = WAL;"
    execute_ conn "pragma synchronous = normal;"
    execute_ conn "pragma journal_size_limit = 6144000;"
    execute_ conn "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY AUTOINCREMENT, msg BLOB)"
  pure $
    PersistenceIncremental
      { append = \a -> liftIO $ withConnection fp $ \conn' ->
          execute conn' "INSERT INTO items (msg) VALUES (?)" (Only $ Aeson.encode a)
      , appendMany = \items -> liftIO $ withConnection fp $ \conn' -> do
          execute_ conn' "BEGIN"
          forM_ items $ \item ->
            execute conn' "INSERT INTO items (msg) VALUES (?)" (Only $ Aeson.encode item)
          execute_ conn' "COMMIT"
      , loadAll = liftIO $ withConnection fp $ \conn' -> do
          r <- query_ conn' "SELECT msg FROM items ORDER BY id ASC"
          pure $ mapMaybe (Aeson.decode . (\(Only b) -> b)) r
      , source =
          liftIO
            ( withConnection fp $ \conn' ->
                do
                  r <- query_ conn' "SELECT msg FROM items ORDER BY id ASC"
                  pure $ mapMaybe (Aeson.decode . (\(Only b) -> b)) r
            )
            >>= mapM_ yield
      , dropDb = liftIO $ removeFile fp
      }
