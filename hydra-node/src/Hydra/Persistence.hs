-- | Handles to save/load files across the hydra-node. We use a simple JSON
-- encoding and two modes of operation to store things: Full and Incremental.
module Hydra.Persistence where

import Hydra.Prelude

import Conduit (
  ConduitT,
  MonadUnliftIO,
  ResourceT,
  linesUnboundedAsciiC,
  mapMC,
  runResourceT,
  sourceFileBS,
  sourceToList,
  (.|),
 )
import Control.Monad.Trans.Resource (allocate)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO (MVar, newMVar, putMVar, takeMVar, withMVar)
import UnliftIO.IO.File (withBinaryFile, writeBinaryFileDurableAtomic)

newtype PersistenceException
  = PersistenceException String
  deriving stock (Eq, Show)

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
data PersistenceIncremental a m = PersistenceIncremental
  { append :: ToJSON a => a -> m ()
  , source :: FromJSON a => ConduitT () a (ResourceT m) ()
  -- ^ Stream all elements from the file.
  , registerThread :: m ()
  -- ^ Register an authorized thread to append to the file.
  }

-- | Load all elements from persistence into a list.
-- XXX: Deprecate this to avoid large memory usage.
loadAll :: (FromJSON a, MonadUnliftIO m) => PersistenceIncremental a m -> m [a]
loadAll PersistenceIncremental{source} =
  runResourceT $ sourceToList source

-- | Initialize persistence handle for given type 'a' at given file path.
--
-- This instance of `PersistenceIncremental` is "thread-safe" in the sense that
-- it prevents appending while a source is still running (while ResourceT is
-- still not fully unwrapped.
createPersistenceIncremental ::
  forall a m.
  ( MonadUnliftIO m
  , MonadThrow m
  , MonadThread m
  , FromJSON a
  ) =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  mutex <- newMVar ()
  pure $
    PersistenceIncremental
      { append = \a ->
          withMVar mutex $ \_ ->
            liftIO $ do
              let bytes = toStrict $ Aeson.encode a <> "\n"
              withBinaryFile fp AppendMode (`BS.hPut` bytes)
      , source = source mutex
      }
 where
  source :: forall i. MVar () -> ConduitT i a (ResourceT m) ()
  source mutex = do
    liftIO (doesFileExist fp) >>= \case
      False -> pure ()
      True -> do
        -- NOTE: Here we take the mutex which will be automatically released
        -- upon running the 'ResourceT' for example when calling runConduitRes.
        void $ allocate (takeMVar mutex) (putMVar mutex)
        -- NOTE: Read, decode and yield values line by line.
        sourceFileBS fp
          .| linesUnboundedAsciiC
          .| mapMC
            ( \bs ->
                case Aeson.eitherDecodeStrict' bs of
                  Left e ->
                    lift . throwIO $
                      PersistenceException $
                        "Error when decoding from file " <> fp <> ": " <> show e <> "\n" <> show bs
                  Right decoded -> pure decoded
            )
