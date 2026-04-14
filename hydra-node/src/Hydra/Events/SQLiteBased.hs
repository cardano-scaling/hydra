-- | A SQLite-backed event source and sink.
--
-- This is the recommended persistence backend for new deployments.
-- See 'migrateFromFileBased' for upgrading from the legacy file-based store.
module Hydra.Events.SQLiteBased where

import Hydra.Prelude

import Conduit (ConduitT, ResourceT, await, bracketP, runConduitRes, sourceFile, yield, (.|))
import Control.Concurrent.Class.MonadSTM (writeTVar)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Combinators (linesUnboundedAscii)
import Data.Conduit.Combinators qualified as C
import Database.SQLite.Simple (Connection, Only (..), Statement, closeStatement, execute, execute_, nextRow, open, openStatement, query_, withTransaction)
import Hydra.Events (EventSink (..), EventSource (..), HasEventId (..))
import Hydra.Events.Rotation (EventStore (..))
import Hydra.Logging (Tracer, traceWith)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)

-- | Exception thrown when a persisted event cannot be decoded.
data EventDecodingException = EventDecodingException
  { eventId :: Word64
  , decodeError :: String
  }
  deriving stock (Show)

instance Exception EventDecodingException

data SQLiteLog
  = MigratingFromFileBased {legacyFile :: FilePath}
  | MigrationSkipped {legacyFile :: FilePath}
  | MigrationComplete {legacyFile :: FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create an 'EventStore' backed by a SQLite database at the given file path.
-- The database and schema are created on first use if they do not exist.
-- Also returns the underlying 'Connection' for use with 'migrateFromFileBased'.
mkSQLiteEventStore ::
  forall e.
  (ToJSON e, FromJSON e, HasEventId e) =>
  FilePath ->
  IO (Connection, EventStore e IO)
mkSQLiteEventStore dbFile = do
  createDirectoryIfMissing True (takeDirectory dbFile)
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

    decodeRow :: (Word64, Text) -> IO e
    decodeRow (eid, evData) =
      case Aeson.eitherDecodeStrict' (encodeUtf8 evData) of
        Right evt -> pure evt
        -- NOTE: This will prevent the node from starting, which is intentional —
        -- starting with missing events would silently corrupt the head state.
        Left err -> throwIO EventDecodingException{eventId = eid, decodeError = err}

    sourceEvents :: ConduitT () e (ResourceT IO) ()
    sourceEvents =
      bracketP openStmt closeStatement yieldRows
     where
      openStmt :: IO Statement
      openStmt = openStatement conn "SELECT event_id, event_data FROM events ORDER BY event_id ASC"

      yieldRows :: Statement -> ConduitT () e (ResourceT IO) ()
      yieldRows stmt = do
        mRow <- liftIO (nextRow stmt)
        case mRow of
          Nothing -> pure ()
          Just row -> do
            evt <- liftIO (decodeRow row)
            yield evt
            yieldRows stmt

    store evt = do
      let evData = decodeUtf8 . toStrict $ Aeson.encode evt
      execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (getEventId evt, evData :: Text)
      atomically $ setLastSeenEventId evt

    putEvent evt =
      atomically getLastSeenEventId >>= \case
        Nothing -> store evt
        Just lastSeenEventId
          | getEventId evt > lastSeenEventId -> store evt
          | otherwise -> pure ()

    rotate _ checkpointEvent = do
      let evData = decodeUtf8 . toStrict $ Aeson.encode checkpointEvent
      withTransaction conn $ do
        execute_ conn "DELETE FROM events"
        execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (getEventId checkpointEvent, evData :: Text)
      atomically $ setLastSeenEventId checkpointEvent

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
-- Safe to call when the legacy file does not exist (no-op). Not safe to re-run:
-- duplicate event ids will cause a primary key constraint violation.
--
-- On success the legacy file is renamed to @<path>.migrated@ so that
-- subsequent node restarts skip the migration step automatically.
--
-- All inserts are batched in a single transaction for performance.
migrateFromFileBased ::
  forall e.
  (FromJSON e, HasEventId e) =>
  Tracer IO SQLiteLog ->
  FilePath ->
  Connection ->
  EventStore e IO ->
  IO ()
migrateFromFileBased tracer legacyFile conn EventStore{eventSink = EventSink{putEvent}} = do
  exists <- doesFileExist legacyFile
  if not exists
    then traceWith tracer MigrationSkipped{legacyFile}
    else do
      traceWith tracer MigratingFromFileBased{legacyFile}
      withTransaction conn $
        runConduitRes $
          sourceFile legacyFile
            .| linesUnboundedAscii
            .| C.filter (not . BS.null)
            .| numberLines
            .| C.mapM_ (lift . putEvent)
      renameFile legacyFile (legacyFile <> ".migrated")
      traceWith tracer MigrationComplete{legacyFile}
 where
  numberLines :: ConduitT ByteString e (ResourceT IO) ()
  numberLines = go 1
   where
    go !lineNo = do
      mLine <- await
      case mLine of
        Nothing -> pure ()
        Just line -> do
          evt <- liftIO (decodeLine lineNo line)
          yield evt
          go (lineNo + 1)

  decodeLine :: Int -> ByteString -> IO e
  decodeLine lineNo line =
    case Aeson.eitherDecodeStrict' line of
      Right evt -> pure evt
      -- NOTE: This will prevent the node from starting, which is intentional —
      -- starting with missing events would silently corrupt the head state.
      Left err -> throwIO EventDecodingException{eventId = fromIntegral lineNo, decodeError = err}

-- Internal

initSchema :: Connection -> IO ()
initSchema conn = do
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA busy_timeout=5000"
  -- With WAL, NORMAL skips per-write fsyncs and only syncs during
  -- checkpoints — safe because the chain is the source of truth.
  execute_ conn "PRAGMA synchronous=NORMAL"
  execute_ conn "PRAGMA cache_size=-65536" -- 64 MB page cache
  execute_ conn "PRAGMA temp_store=MEMORY"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS events \
    \(event_id INTEGER NOT NULL PRIMARY KEY, event_data TEXT NOT NULL)"
