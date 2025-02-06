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
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Class.MonadFork (ThreadId, myThreadId)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (withBinaryFile, writeBinaryFileDurableAtomic)

data PersistenceException
  = PersistenceException String
  | IncorrectAccessException String
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
-- it prevents loading from a different thread once one starts `append`ing
-- through the handle. If another thread attempts to `source` (or `loadAll`)
-- after this point, an `IncorrectAccessException` will be raised.
createPersistenceIncremental ::
  forall a m.
  ( MonadUnliftIO m
  , MonadThrow m
  , FromJSON a
  ) =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  authorizedThread <- liftIO $ newTVarIO Nothing
  pure $
    PersistenceIncremental
      { append = \a -> liftIO $ do
          tid <- myThreadId
          atomically $ writeTVar authorizedThread $ Just tid
          let bytes = toStrict $ Aeson.encode a <> "\n"
          withBinaryFile fp AppendMode (`BS.hPut` bytes)
      , source = source authorizedThread
      }
 where
  source :: forall i. TVar IO (Maybe (ThreadId IO)) -> ConduitT i a (ResourceT m) ()
  source authorizedThread = do
    liftIO $ do
      tid <- myThreadId
      authTid <- readTVarIO authorizedThread
      when (isJust authTid && authTid /= Just tid) $
        throwIO (IncorrectAccessException $ "Trying to load persisted data in " <> fp <> " from different thread")

    liftIO (doesFileExist fp) >>= \case
      False -> pure ()
      True -> do
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
