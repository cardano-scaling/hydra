-- | Handles to save/load files across the hydra-node. We use a simple JSON
-- encoding and two modes of operation to store things: Full and Incremental.
module Hydra.Persistence where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (newTVarIO, throwSTM, writeTVar)
import Control.Lens.Combinators (iforM)
import Control.Monad.Class.MonadFork (myThreadId)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (withBinaryFile, writeBinaryFile)

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
          -- FIXME: this is not atomic / durable
          writeBinaryFile fp . toStrict $ Aeson.encode a
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
  , loadAll :: FromJSON a => m [a]
  }

-- | Initialize persistence handle for given type 'a' at given file path.
--
-- This instance of `PersistenceIncremental` is "thread-safe" in the sense that
-- it prevents loading from a different thread once one starts `append`ing
-- through the handle. If another thread attempts to `loadAll` after this point,
-- an `IncorrectAccessException` will be raised.
createPersistenceIncremental ::
  forall a m.
  (MonadIO m, MonadThrow m, MonadSTM m, MonadThread m, MonadThrow (STM m)) =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  authorizedThread <- newTVarIO Nothing
  pure $
    PersistenceIncremental
      { append = \a -> do
          tid <- myThreadId
          atomically $ writeTVar authorizedThread $ Just tid
          let bytes = toStrict $ Aeson.encode a <> "\n"
          liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes)
      , loadAll = do
          tid <- myThreadId
          atomically $ do
            authTid <- readTVar authorizedThread
            when (isJust authTid && authTid /= Just tid) $
              throwSTM (IncorrectAccessException $ "Trying to load persisted data in " <> fp <> " from different thread")

          liftIO (doesFileExist fp) >>= \case
            False -> pure []
            True -> do
              bs <- readFileBS fp
              -- NOTE: We require the whole file to be loadable. It might
              -- happen that the data written by 'append' is only there
              -- partially and then this will fail (which we accept now).
              iforM (C8.lines bs) $ \i o ->
                case Aeson.eitherDecodeStrict' o of
                  Left e -> throwIO $ PersistenceException ("Error at line: " <> show (i + 1) <> " in file " <> fp <> " - " <> e)
                  Right decoded -> pure decoded
      }
