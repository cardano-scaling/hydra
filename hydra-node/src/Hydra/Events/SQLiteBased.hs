-- | A SQLite-backed event source and sink.
--
-- This is the recommended persistence backend for new deployments.
-- See 'withSQLiteEventStore' which handles migration from the legacy
-- file-based store automatically.
--
-- == Architecture
--
-- Events are stored in a single @events@ table with an integer primary key
-- (@event_id@) and a BLOB column (@event_data@) containing JSON-encoded event
-- data. The database uses WAL journal mode with @synchronous=NORMAL@ to avoid
-- per-write fsyncs while still syncing at WAL checkpoints.
--
-- == Async write-behind
--
-- To keep persistence off the hot path, writes use an async write-behind
-- strategy. 'putEvent' and 'putEvents' encode events eagerly to strict
-- 'ByteString' and enqueue them into a bounded 'TBQueue'. A background writer
-- thread drains the queue and batch-inserts rows using 'executeMany' inside a
-- single transaction, amortising WAL frame writes across multiple events.
--
-- The last-seen event id 'TVar' is updated atomically at enqueue time (not
-- write time), so de-duplication and source-of-truth tracking remain correct
-- even though the physical write is deferred.
--
-- All SQLite writes go through the single writer thread, preventing concurrent
-- access races. Operations that need data flushed (rotation, reads) use a flush
-- marker: a 'TMVar' is enqueued and the caller blocks until the writer thread
-- has processed all preceding items and signalled it. 'sourceEvents'
-- auto-flushes before reading, so callers always see all enqueued events.
--
-- == Tradeoffs
--
-- * __Writer thread crash surfacing__: The background writer is 'link'ed to
--   the calling thread. If it dies (e.g. SQLite I/O error), the exception
--   propagates immediately rather than leaving the node silently stalled.
--   Use 'withSQLiteEventStore' which handles cleanup (flush + cancel) on exit.
--
-- * __Data loss on hard crash__: Events in the queue that have not yet been
--   flushed to SQLite are lost on SIGKILL, OOM, or power loss. This is
--   acceptable because the L1 chain is the source of truth — the node replays
--   missed events from chain on restart.
--
-- * __Rotation ordering__: 'rotate' flushes the write queue synchronously
--   before performing DELETE + INSERT. This is safe because
--   rotation is only called from the single-threaded event processing loop
--   ('processStateChanges'), so no concurrent enqueues can occur between the
--   flush and the rotation write.
module Hydra.Events.SQLiteBased where

import Hydra.Prelude

import Conduit (ConduitT, ResourceT, bracketP, runConduitRes, sourceFile, yield, (.|))
import Control.Concurrent.Class.MonadSTM (flushTBQueue, newEmptyTMVarIO, newTBQueueIO, putTMVar, readTBQueue, takeTMVar, writeTBQueue, writeTVar)
import Control.Monad.Class.MonadAsync (async, cancel, link)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Combinators (linesUnboundedAscii)
import Data.Conduit.Combinators qualified as C
import Database.SQLite.Simple (Connection, Only (..), Statement, close, closeStatement, execute, executeMany, execute_, nextRow, open, openStatement, query_, withTransaction)
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

-- | Items in the write-behind queue: either an event row to insert or a flush
-- marker that the writer thread signals after processing all preceding items.
type WriteItem = Either (TMVar IO ()) (Word64, ByteString)

-- | Bracket-style wrapper around 'mkSQLiteEventStore'. Creates the database,
-- schema, and writer thread, runs the callback, then flushes queued writes and
-- cancels the writer thread on exit. The writer thread is 'link'ed so that
-- crashes surface immediately in the calling thread.
--
-- If a legacy state file exists at @legacyStateFile@, events are migrated into
-- SQLite automatically before the callback runs.
--
-- Flushing of the async write queue and reinitialisation of the last-seen
-- event id are handled internally: 'sourceEvents' auto-flushes before
-- reading, 'rotate' flushes before deleting, migration reinitialises the
-- event id TVar, and this bracket flushes on exit.
withSQLiteEventStore ::
  forall e a.
  (FromJSON e, ToJSON e, HasEventId e) =>
  Tracer IO SQLiteLog ->
  FilePath ->
  FilePath ->
  (EventStore e IO -> IO a) ->
  IO a
withSQLiteEventStore tracer dbFile legacyStateFile callback = do
  (conn, store, flush, reinitLastSeen, cancelWriter) <- mkSQLiteEventStore dbFile
  migrateFromFileBased (Proxy @e) tracer legacyStateFile conn reinitLastSeen
  callback store
    `finally` (flush >> cancelWriter >> close conn)

-- | Create an 'EventStore' backed by a SQLite database at the given file path.
-- The database and schema are created on first use if they do not exist.
-- Returns @(conn, store, flush, reinitLastSeen, cancelWriter)@. Internal —
-- prefer 'withSQLiteEventStore' which handles cleanup, migration, and flushing
-- automatically.
mkSQLiteEventStore ::
  forall e.
  (ToJSON e, FromJSON e, HasEventId e) =>
  FilePath ->
  IO (Connection, EventStore e IO, IO (), IO (), IO ())
mkSQLiteEventStore dbFile = do
  createDirectoryIfMissing True (takeDirectory dbFile)
  conn <- open dbFile
  initSchema conn
  eventIdV <- newLabelledTVarIO "sqlite-event-store-event-id" Nothing
  -- Initialise last-seen event id from existing rows.
  rows <- selectLastEventId conn
  case rows of
    [Only lastId] -> atomically $ writeTVar eventIdV (Just lastId)
    _ -> pure ()

  writeQueue <- newTBQueueIO 1000
  writerThread <- async $ writerLoop conn writeQueue
  link writerThread
  let
    getLastSeenEventId = readTVar eventIdV

    setLastSeenEventId evt =
      writeTVar eventIdV (Just $ getEventId evt)

    decodeRow :: (Word64, ByteString) -> IO e
    decodeRow (eid, evData) =
      case Aeson.eitherDecodeStrict' evData of
        Right evt -> pure evt
        -- NOTE: This will prevent the node from starting, which is intentional —
        -- starting with missing events would silently corrupt the head state.
        Left err -> throwIO EventDecodingException{eventId = eid, decodeError = err}

    sourceEvents :: ConduitT () e (ResourceT IO) ()
    sourceEvents = do
      -- Flush queued writes so reads see all enqueued events.
      liftIO $ flushWriteQueue writeQueue
      bracketP openStmt closeStatement yieldRows
     where
      openStmt :: IO Statement
      openStmt = getEventsASC conn

      yieldRows :: Statement -> ConduitT () e (ResourceT IO) ()
      yieldRows stmt = do
        mRow <- liftIO (nextRow stmt)
        case mRow of
          Nothing -> pure ()
          Just row -> do
            evt <- liftIO (decodeRow row)
            yield evt
            yieldRows stmt

    enqueueEvent evt = do
      let !encoded = toStrict $ Aeson.encode evt
      atomically $ do
        writeTBQueue writeQueue (Right (getEventId evt, encoded))
        setLastSeenEventId evt

    putEvent evt =
      atomically getLastSeenEventId >>= \case
        Nothing -> enqueueEvent evt
        Just lastSeenEventId
          | getEventId evt > lastSeenEventId -> enqueueEvent evt
          | otherwise -> pure ()

    putEvents evts = do
      lastSeen <- atomically getLastSeenEventId
      let newEvts = case lastSeen of
            Nothing -> evts
            Just lastId -> filter (\e -> getEventId e > lastId) evts
      unless (null newEvts) $ do
        let !encodedEvts = map (\evt -> Right (getEventId evt, toStrict $ Aeson.encode evt)) newEvts
        atomically $ do
          forM_ encodedEvts $ writeTBQueue writeQueue
          case nonEmpty newEvts of
            Just ne -> setLastSeenEventId (last ne)
            Nothing -> pure ()

    rotate _ checkpointEvent = do
      flushWriteQueue writeQueue
      let evData = toStrict $ Aeson.encode checkpointEvent
      withTransaction conn $ do
        deleteAllEvents conn
        insertEvent conn (getEventId checkpointEvent, evData)
      atomically $ setLastSeenEventId checkpointEvent

  let reinitLastSeen = do
        latestRows <- selectLastEventId conn
        case latestRows of
          [Only lastId] -> atomically $ writeTVar eventIdV (Just lastId)
          _ -> pure ()

  pure
    ( conn
    , EventStore
        { eventSource = EventSource{sourceEvents}
        , eventSink = EventSink{putEvent, putEvents}
        , rotate
        }
    , flushWriteQueue writeQueue
    , reinitLastSeen
    , cancel writerThread
    )

-- | Background writer that drains the queue and batch-inserts into SQLite.
-- Each iteration blocks for at least one item, then flushes everything
-- available. Event rows are batch-inserted in a single transaction, then any
-- flush markers in the batch are signalled.
writerLoop :: Connection -> TBQueue IO WriteItem -> IO ()
writerLoop conn queue = forever $ do
  first' <- atomically $ readTBQueue queue
  rest <- atomically $ flushTBQueue queue
  let allItems = first' : rest
      (flushSignals, eventRows) = partitionEithers allItems
  unless (null eventRows) $
    withTransaction conn $
      insertEvents conn eventRows
  forM_ flushSignals $ \mv -> atomically $ putTMVar mv ()

-- | Block until all items currently in the write queue have been flushed to
-- SQLite. Sends a flush marker through the queue and waits for the writer thread
-- to signal completion.
flushWriteQueue :: TBQueue IO WriteItem -> IO ()
flushWriteQueue queue = do
  mv <- newEmptyTMVarIO
  atomically $ writeTBQueue queue (Left mv)
  atomically $ takeTMVar mv

-- | Migrate events from a legacy newline-delimited JSON file into SQLite.
-- Writes directly to the database, bypassing the async write queue (migration
-- runs at startup before the node processes inputs). After inserting, calls
-- @reinitLastSeen@ to sync the in-memory event id TVar with the database.
--
-- Safe to call when the legacy file does not exist (no-op). Not safe to re-run:
-- duplicate event ids will cause a primary key constraint violation.
--
-- On success the legacy file is renamed to @<path>.migrated@ so that
-- subsequent node restarts skip the migration step automatically.
migrateFromFileBased ::
  forall e.
  (FromJSON e, HasEventId e) =>
  Proxy e ->
  Tracer IO SQLiteLog ->
  FilePath ->
  Connection ->
  IO () ->
  IO ()
migrateFromFileBased _proxy tracer legacyFile conn reinitLastSeen = do
  exists <- doesFileExist legacyFile
  if not exists
    then traceWith tracer MigrationSkipped{legacyFile}
    else do
      traceWith tracer MigratingFromFileBased{legacyFile}
      rawLines <-
        runConduitRes $
          sourceFile legacyFile
            .| linesUnboundedAscii
            .| C.filter (not . BS.null)
            .| C.sinkList
      -- Decode each line to extract the event id, then store the original raw
      -- bytes. Invalid JSON is caught here so corrupt files fail at migration.
      rowParams <- forM (zip [1 ..] rawLines) $ \(lineNo :: Int, line) ->
        case Aeson.eitherDecodeStrict' @e line of
          Right evt -> pure (getEventId evt, line)
          Left err -> throwIO EventDecodingException{eventId = fromIntegral lineNo, decodeError = err}
      unless (null rowParams) $
        withTransaction conn $
          insertEvents conn rowParams
      -- Re-read the last event id from the database so the in-memory
      -- de-duplication TVar is consistent with the migrated rows.
      reinitLastSeen
      renameFile legacyFile (legacyFile <> ".migrated")
      traceWith tracer MigrationComplete{legacyFile}

-- Internal

-- | Current schema version. Bump this and add a migration step to
-- 'applyMigrations' whenever the schema changes.
nextVersion :: Int
nextVersion = 1

-- | Initialise connection pragmas, then create or migrate the schema to
-- 'nextVersion' using SQLite's built-in @user_version@ pragma.
initSchema :: Connection -> IO ()
initSchema conn = do
  configurePragmas conn
  v <- getSchemaVersion conn
  applyMigrations conn v

configurePragmas :: Connection -> IO ()
configurePragmas conn =
  mapM_
    (execute_ conn)
    [ "PRAGMA journal_mode=WAL"
    , "PRAGMA busy_timeout=5000"
    , -- With WAL, NORMAL skips per-write fsyncs and only syncs during
      -- checkpoints — safe because the chain is the source of truth.
      "PRAGMA synchronous=NORMAL"
    , "PRAGMA cache_size=-65536" -- 64 MB page cache
    , "PRAGMA temp_store=MEMORY"
    ]

-- | Read the schema version from @PRAGMA user_version@ (0 for a fresh DB).
getSchemaVersion :: Connection -> IO Int
getSchemaVersion conn = do
  [[v]] <- query_ conn "PRAGMA user_version"
  pure v

setSchemaVersion :: Connection -> Int -> IO ()
setSchemaVersion conn v =
  -- PRAGMA doesn't support parameter binding, so we use show directly.
  -- The value is an Int we control, not user input.
  execute_ conn $ fromString $ "PRAGMA user_version = " <> show v

-- | Apply all pending migrations from version @v@ up to 'nextVersion'.
-- Each step runs in its own transaction so that a crash mid-migration leaves
-- the database at a well-defined version.
applyMigrations :: Connection -> Int -> IO ()
applyMigrations conn v
  | v > nextVersion =
      error $ "Database schema version " <> show v <> " is newer than supported " <> show nextVersion <> ", cannot downgrade"
  | v == nextVersion = pure ()
  | otherwise = do
      migrateStep conn v
      setSchemaVersion conn (v + 1)
      applyMigrations conn (v + 1)

-- | Individual migration steps. Pattern-match on the /source/ version.
migrateStep :: Connection -> Int -> IO ()
migrateStep conn = \case
  0 -> createEventsTable conn
  unknown ->
    error $ "Unknown schema version " <> show unknown <> ", cannot migrate"

-- SQL queries

createEventsTable :: Connection -> IO ()
createEventsTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS events \
    \(event_id INTEGER NOT NULL PRIMARY KEY, event_data BLOB NOT NULL)"

selectLastEventId :: Connection -> IO [Only Word64]
selectLastEventId conn =
  query_ conn "SELECT event_id FROM events ORDER BY event_id DESC LIMIT 1"

getEventsASC :: Connection -> IO Statement
getEventsASC conn =
  openStatement conn "SELECT event_id, event_data FROM events ORDER BY event_id ASC"

insertEvent :: Connection -> (Word64, ByteString) -> IO ()
insertEvent conn =
  execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)"

insertEvents :: Connection -> [(Word64, ByteString)] -> IO ()
insertEvents conn =
  executeMany conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)"

deleteAllEvents :: Connection -> IO ()
deleteAllEvents conn =
  execute_ conn "DELETE FROM events"
