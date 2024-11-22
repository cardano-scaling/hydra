{-# LANGUAGE OverloadedStrings #-}

-- | Handles to save/load files across the hydra-node. We use a simple JSON
-- encoding and two modes of operation to store things: Full and Incremental.
module Hydra.SqlLitePersistence where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Database.SQLite.Simple (FromRow, Only (..), Query (..), execute, execute_, field, fold_, fromRow, query_, withConnection)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory)

data PersistenceException
  = PersistenceException String
  | IncorrectAccessException String
  deriving stock (Eq, Show)

instance Exception PersistenceException

-- | Handle to save and load files to/from db using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  , dropDb :: m ()
  }

-- | A carrier type that wraps the JSON string. Needed just to specify the sql instances we need.
newtype Record = Record BSL.ByteString deriving newtype (Eq, Show)

instance FromRow Record where
  fromRow = Record <$> field

createPersistence ::
  MonadIO m =>
  FilePath ->
  m (Persistence a m)
createPersistence fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  let dbName = Query (T.pack $ "\"" <> fp <> "\"")
  liftIO $ withConnection fp $ \conn -> do
    execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> dbName <> " (id INTEGER PRIMARY KEY, msg SQLBlob)"
  pure $
    Persistence
      { save = \a -> liftIO $ withConnection fp $ \conn' ->
          execute conn' ("INSERT INTO " <> dbName <> " (msg) VALUES (?)") (Only $ Aeson.encode a)
      , load = liftIO $ withConnection fp $ \conn' -> do
          r <- query_ conn' ("SELECT msg FROM " <> dbName <> " ORDER BY id DESC LIMIT 1")
          case r of
            [] -> pure Nothing
            (Record result : _) -> pure $ Aeson.decode result
      , dropDb = liftIO $ removeFile fp
      }

-- | Handle to save incrementally and load files to/from disk using JSON encoding.
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
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  let dbName = Query (T.pack $ "\"" <> fp <> "\"")
  liftIO $ withConnection fp $ \conn -> do
    execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> dbName <> " (id INTEGER PRIMARY KEY, msg SQLBlob)"
  pure $
    PersistenceIncremental
      { append =
          -- TODO: try to batch insert here or use some other trick to make it faster
          \a -> liftIO $ withConnection fp $ \conn' ->
            execute conn' ("INSERT INTO " <> dbName <> " (msg) VALUES (?)") (Only $ Aeson.encode a)
      , loadAll = liftIO $ withConnection fp $ \conn' -> do
          let collectValues acc (Record i) = pure $ i : acc
          bsVals <- fold_ conn' ("SELECT msg FROM " <> dbName <> " ORDER BY id DESC") [] collectValues
          pure $ mapMaybe Aeson.decode bsVals
      , dropDb = liftIO $ removeFile fp
      }
