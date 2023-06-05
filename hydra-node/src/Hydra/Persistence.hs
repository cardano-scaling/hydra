-- | Handles to save/load files across the hydra-node. We use a simple JSON
-- encoding and two modes of operation to store things: Full and Incremental.
module Hydra.Persistence where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Reflection (Reifies)
import Hydra.API.ServerOutput (ServerOutputConfig)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (withBinaryFile, writeBinaryFileDurableAtomic)

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
createPersistence ::
  (MonadIO m, MonadThrow m) =>
  FilePath ->
  m (Persistence a m)
createPersistence fp = do
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
              if BS.null bs
                then pure Nothing
                else case Aeson.eitherDecodeStrict' bs of
                  Left e -> throwIO $ PersistenceException e
                  Right a -> pure (Just a)
      }

-- | Handle to save incrementally and load files to/from disk using JSON encoding.
data PersistenceIncremental r a m = PersistenceIncremental
  { append :: (ToJSON a, Reifies r ServerOutputConfig) => a -> m ()
  , loadAll :: FromJSON a => m [a]
  }

-- | Initialize persistence handle for given type 'a' at given file path.
createPersistenceIncremental ::
  forall r m a.
  (MonadIO m, MonadThrow m, Reifies r ServerOutputConfig) =>
  FilePath ->
  m (PersistenceIncremental r a m)
createPersistenceIncremental fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  pure $
    PersistenceIncremental
      { append = \a -> do
          let bytes = toStrict $ Aeson.encode a <> "\n"
          liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes)
      , loadAll =
          liftIO (doesFileExist fp) >>= \case
            False -> pure []
            True -> do
              bs <- readFileBS fp
              -- NOTE: We require the whole file to be loadable. It might
              -- happen that the data written by 'append' is only there
              -- partially and then this will fail (which we accept now).
              case forM (C8.lines bs) Aeson.eitherDecodeStrict' of
                Left e -> throwIO $ PersistenceException e
                Right decoded -> pure decoded
      }
