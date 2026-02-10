-- | Handles to save/load files across the hydra-node. We use a simple JSON
-- encoding and two modes of operation to store things: Full and Incremental.
module Hydra.Persistence where

import Hydra.Prelude

import Hydra.Logging (Tracer, traceWith)
import Hydra.PersistenceLog (PersistenceLog (..))
import "aeson" Data.Aeson qualified as Aeson
import "bytestring" Data.ByteString qualified as BS
import "conduit" Conduit (
  ConduitT,
  MonadUnliftIO,
  ResourceT,
  concatC,
  linesUnboundedAsciiC,
  mapMC,
  runResourceT,
  sourceFileBS,
  sourceToList,
  (.|),
 )
import "directory" System.Directory (createDirectoryIfMissing, doesFileExist)
import "filepath" System.FilePath (takeDirectory)
import "resourcet" Control.Monad.Trans.Resource (allocate)
import "unliftio" UnliftIO (MVar, newMVar, putMVar, takeMVar, withMVar)
import "unliftio" UnliftIO.IO.File (withBinaryFile, writeBinaryFileDurableAtomic)

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
  , FromJSON a
  ) =>
  Tracer IO PersistenceLog ->
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental tracer fp = do
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
  -- Maybe read the next item from persistence; or, if we failed to
  -- decode it, we will emit a warning.
  maybeDecode :: ByteString -> ResourceT m (Maybe a)
  maybeDecode bs = case Aeson.eitherDecodeStrict' bs of
    Left e -> do
      liftIO $ traceWith tracer $ FailedToDecodeJson{reason = show e, filepath = fp, contents = show bs}
      pure Nothing
    Right decoded -> pure (Just decoded)
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
          .| mapMC maybeDecode
          .| concatC
