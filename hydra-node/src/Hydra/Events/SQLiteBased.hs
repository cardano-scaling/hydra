-- | A SQLite-backed event source and sink.
--
-- This is the recommended persistence backend for new deployments.
-- See 'migrateFromFileBased' for upgrading from the legacy file-based store.
module Hydra.Events.SQLiteBased where

import Hydra.Prelude

import Conduit (ConduitT, ResourceT, yieldMany)
import Control.Concurrent.Class.MonadSTM (writeTVar)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Database.SQLite.Simple (Connection, Only (..), execute, execute_, open, query_, withTransaction)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import Hydra.Events.Rotation (EventStore (..))
import System.Directory (doesFileExist, renameFile)

-- | Create an 'EventStore' backed by a SQLite database at the given file path.
-- The database and schema are created on first use if they do not exist.
-- Also returns the underlying 'Connection' for use with 'migrateFromFileBased'.
mkSQLiteEventStore ::
  forall e.
  (ToJSON e, FromJSON e, HasEventId e) =>
  FilePath ->
  IO (Connection, EventStore e IO)
mkSQLiteEventStore dbFile = do
  conn <- open dbFile
  initSchema conn
  eventIdV <- newLabelledTVarIO "sqlite-event-store-event-id" Nothing
  -- Initialise last-seen event id from existing rows.
  rows <- query_ conn "SELECT event_id FROM events ORDER BY event_id DESC LIMIT 1" :: IO [Only Word64]
  case rows of
    [Only lastId] -> atomically $ writeTVar eventIdV (Just lastId)
    _ -> pure ()

  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId evt =
      writeTVar eventIdV (Just $ getEventId evt)

    loadRows :: IO [(Word64, Text)]
    loadRows = query_ conn "SELECT event_id, event_data FROM events ORDER BY event_id ASC"

    decodeRow :: (Word64, Text) -> Maybe e
    decodeRow (_, evData) =
      Aeson.decodeStrict' (encodeUtf8 evData)

    sourceEvents :: ConduitT () e (ResourceT IO) ()
    sourceEvents = do
      allRows <- liftIO loadRows
      yieldMany $ mapMaybe decodeRow allRows

    store evt = do
      let evData = decodeUtf8 . toStrict $ Aeson.encode evt
      execute conn "INSERT OR IGNORE INTO events (event_id, event_data) VALUES (?, ?)" (getEventId evt, evData :: Text)
      atomically $ setLastSeenEventId evt

    putEvent evt =
      atomically getLastSeenEventId >>= \case
        Nothing -> store evt
        Just lastSeenEventId
          | getEventId evt > lastSeenEventId -> store evt
          | otherwise -> pure ()

    rotate _ checkpointEvent =
      withTransaction conn $ do
        execute_ conn "DELETE FROM events"
        store checkpointEvent

  pure
    ( conn
    , EventStore
        { eventSource = EventSource{sourceEvents}
        , eventSink = EventSink{putEvent}
        , rotate
        }
    )

-- | Migrate events from a legacy newline-delimited JSON file into a SQLite
-- event store.
--
-- Safe to call when the legacy file does not exist (no-op). Safe to re-run:
-- existing events are never duplicated because of @INSERT OR IGNORE@.
--
-- On success the legacy file is renamed to @<path>.migrated@ so that
-- subsequent node restarts skip the migration step automatically.
--
-- All inserts are batched in a single transaction for performance.
migrateFromFileBased ::
  forall e.
  (FromJSON e, HasEventId e) =>
  FilePath ->
  Connection ->
  EventStore e IO ->
  IO ()
migrateFromFileBased legacyFile conn EventStore{eventSink = EventSink{putEvent}} = do
  exists <- doesFileExist legacyFile
  when exists $ do
    contents <- BS.readFile legacyFile
    let events :: [e]
        events = mapMaybe Aeson.decodeStrict' (BS.split 10 contents)
    withTransaction conn $ forM_ events putEvent
    renameFile legacyFile (legacyFile <> ".migrated")

-- Internal

initSchema :: Connection -> IO ()
initSchema conn = do
  execute_ conn "PRAGMA journal_mode=WAL"
  -- With WAL, NORMAL skips per-write fsyncs and only syncs during
  -- checkpoints — safe because the chain is the source of truth.
  execute_ conn "PRAGMA synchronous=NORMAL"
  execute_ conn "PRAGMA cache_size=-65536" -- 64 MB page cache
  execute_ conn "PRAGMA temp_store=MEMORY"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS events \
    \(event_id INTEGER NOT NULL PRIMARY KEY, event_data TEXT NOT NULL)"
