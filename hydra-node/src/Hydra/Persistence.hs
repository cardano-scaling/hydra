module Hydra.Persistence where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (withBinaryFileDurable, writeBinaryFileDurableAtomic)

-- ** Save and load files

newtype PersistenceException
  = PersistenceException String
  deriving (Eq, Show)

instance Exception PersistenceException

-- | Handle to save and load files to/from disk using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  }

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

-- | Handle to save incrementally and load files to/from disk using JSON encoding.
data PersistenceIncremental a m = PersistenceIncremental
  { loadAll :: FromJSON a => m [a]
  , append :: ToJSON a => a -> m ()
  }

-- | Initialize persistence handle for given type 'a' at given file path.
createPersistenceIncremental :: (MonadIO m, MonadThrow m) => Proxy a -> FilePath -> m (PersistenceIncremental a m)
createPersistenceIncremental _ fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  pure $
    PersistenceIncremental
      { loadAll =
          liftIO (doesFileExist fp) >>= \case
            False -> pure []
            True -> do
              bs <- readFileBS fp
              -- XXX: This is weird and smelly
              if BS.null bs
                then pure []
                else do
                  case forM (C8.lines bs) $ Aeson.eitherDecodeStrict' of
                    Left e -> throwIO $ PersistenceException e
                    Right decoded -> pure decoded
      , append = \a -> do
          let bytes = toStrict $ Aeson.encode a <> "\n"
          liftIO $ withBinaryFileDurable fp AppendMode (`BS.hPut` bytes)
      }
