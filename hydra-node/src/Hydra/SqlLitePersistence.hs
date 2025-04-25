-- | Handles to save/load files across the hydra-node using sql-lite.
-- We use a simple JSON encoding and two modes of operation to store things: Full and Incremental.
module Hydra.SqlLitePersistence where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Database.SQLite.Simple (
  Only (..),
  Query (..),
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
  let dbName = Query (T.pack $ "\"" <> fp <> "\"")

  liftIO $ withConnection fp $ \conn -> do
    execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> dbName <> " (id INTEGER PRIMARY KEY, msg BLOB)"

  pure $
    Persistence
      { save = \a -> liftIO $ withConnection fp $ \conn' ->
          execute conn' ("INSERT INTO " <> dbName <> " (msg) VALUES (?)") (Only $ Aeson.encode a)
      , load = liftIO $ withConnection fp $ \conn' -> do
          r <- query_ conn' ("SELECT msg FROM " <> dbName <> " ORDER BY id DESC LIMIT 1")
          case r of
            [] -> pure Nothing
            (Only result : _) -> pure $ Aeson.decode result
      , dropDb = liftIO $ removeFile fp
      }

-- | Handle to save incrementally and load files to/from db using JSON encoding.
data PersistenceIncremental a m = PersistenceIncremental
  { append :: ToJSON a => a -> m ()
  , loadAll :: FromJSON a => m [a]
  , dropDb :: m ()
  }

createPersistenceIncremental ::
  forall a m.
  MonadIO m =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory fp
  let dbName = Query (T.pack $ "\"" <> fp <> "\"")

  liftIO $ withConnection fp $ \conn -> do
    execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> dbName <> " (id INTEGER PRIMARY KEY, msg BLOB)"

  pure $
    PersistenceIncremental
      { append = \a -> liftIO $ withConnection fp $ \conn' ->
          execute conn' ("INSERT INTO " <> dbName <> " (msg) VALUES (?)") (Only $ Aeson.encode a)
      , loadAll = liftIO $ withConnection fp $ \conn' -> do
          r <- query_ conn' ("SELECT msg FROM " <> dbName <> " ORDER BY id ASC")
          pure $ mapMaybe (Aeson.decode . (\(Only b) -> b)) r
      , dropDb = liftIO $ removeFile fp
      }
