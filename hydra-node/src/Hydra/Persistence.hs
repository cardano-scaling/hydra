{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Persistence where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (withBinaryFileDurableAtomic, writeBinaryFileDurableAtomic)

-- ** Save and load files

-- | Handle to save and load files to/from disk using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  }

-- | Handle to append and load files to/from disk using JSON encoding.
data PersistenceClient a m = PersistenceClient
  { loadAll :: FromJSON a => m [a]
  , append :: ToJSON a => a -> m ()
  }

newtype PersistenceException
  = PersistenceException String
  deriving (Eq, Show)

instance Exception PersistenceException

-- | Initialize persistence handle for given type 'a' at given file path.
createPersistence :: (MonadIO m, MonadThrow m) => Proxy a -> FilePath -> m (Persistence a m)
createPersistence _ fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  pure $
    Persistence
      { save = \a -> do
          writeBinaryFileDurableAtomic fp . toStrict $ Aeson.encode a
      , load =
          liftIO (doesFileExist fp) >>= \case
            False -> pure Nothing
            True -> do
              bs <- readFileBS fp
              -- XXX: This is weird and smelly
              if BS.null bs
                then pure Nothing
                else case Aeson.eitherDecodeStrict' bs of
                  Left e -> throwIO $ PersistenceException e
                  Right a -> pure (Just a)
      }

-- | Initialize persistence handle for given type 'a' at given file path.
createPersistenceClient :: (MonadIO m, MonadThrow m) => Proxy a -> FilePath -> m (PersistenceClient a m)
createPersistenceClient _ fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  pure $
    PersistenceClient
      { loadAll =
          liftIO (doesFileExist fp) >>= \case
            False -> pure []
            True -> do
              bs <- readFileBS fp
              -- XXX: This is weird and smelly
              if BS.null bs
                then pure []
                else case Aeson.eitherDecodeStrict' bs of
                  Left e -> throwIO $ PersistenceException e
                  Right a -> pure a
      , append = \a -> do
          -- atomicity in file writing implies a file copy everytime we append something to it
          let bytes = toStrict $ Aeson.encode a
          liftIO $ withBinaryFileDurableAtomic fp AppendMode (`BS.hPut` bytes)
      }
