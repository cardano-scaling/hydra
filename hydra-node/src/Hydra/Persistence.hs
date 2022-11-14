module Hydra.Persistence where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

-- ** Save and load files

-- | Handle to save and load files to/from disk using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
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
                  Right a -> pure $ Just a
      }
