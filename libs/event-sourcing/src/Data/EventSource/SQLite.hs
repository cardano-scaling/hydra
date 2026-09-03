-- | A SQLite-backed event source and sink.
--
-- == Architecture
--
-- Events are stored in a single @events@ table with an integer primary key
-- (@event_id@) and a BLOB column (@event_data@) containing CBOR-encoded event
-- data (via 'ToCBOR' / 'FromCBOR'). The database uses WAL journal mode with
-- @synchronous=NORMAL@ to avoid per-write fsyncs while still syncing at WAL
-- checkpoints.
--
-- == Schema migrations
--
-- The schema version is tracked in @PRAGMA user_version@ and migrated on open
-- (see 'applyMigrations'). Version 1 stored event data as JSON; opening a
-- version 1 database re-encodes every row to CBOR in one transaction and runs
-- @VACUUM@ afterwards to reclaim the freed space. A row that fails to decode
-- aborts the migration (and thereby startup) with 'EventDecodingException',
-- rolling back to an intact version 1 database. The legacy file-based store
-- (JSON lines) is migrated by decoding each line as JSON and inserting CBOR.
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
--   propagates immediately rather than leaving the caller silently stalled.
--   Use 'withSQLiteEventStore' which handles cleanup (flush + cancel) on exit.
--
-- * __Data loss on hard crash__: Events in the queue that have not yet been
--   flushed to SQLite are lost on SIGKILL, OOM, or power loss. This is
--   acceptable when an external source of truth can replay missed events.
--
-- * __Rotation ordering__: 'rotate' flushes the write queue synchronously,
--   archives the current database to @old-state/<name>-<logId>.db@ via
--   @VACUUM INTO@, then performs DELETE + INSERT. This is safe because rotation
--   is expected to be called only from a single-threaded processing loop, so no
--   concurrent enqueues can occur between the flush and the rotation write. The
--   archive is taken before the DELETE, so a backup failure aborts rotation and
--   leaves the events intact.
--
-- * __Separate read connection__: 'sourceEvents' streams over a dedicated
--   connection. It can run concurrently on other threads, and @VACUUM INTO@
--   fails with "SQL statements in progress" if a streaming statement is open on
--   the same connection as the rotation. WAL mode makes readers on a separate
--   connection safe.
module Data.EventSource.SQLite where

import Cardano.Binary (FromCBOR, ToCBOR, decodeFull', serialize')
import Conduit (ConduitT, ResourceT, bracketP, runConduitRes, sourceFile, yield, (.|))
import Control.Concurrent.Class.Labelled (newLabelledTVarIO)
import Control.Concurrent.Class.MonadSTM (
  TBQueue,
  TMVar,
  atomically,
  flushTBQueue,
  newEmptyTMVarIO,
  newTBQueueIO,
  putTMVar,
  readTBQueue,
  readTVar,
  takeTMVar,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (Exception, finally, throwIO)
import Control.Monad (forM, forM_, forever, unless, when)
import Control.Monad.Class.MonadAsync (async, cancel, link)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Conduit.Combinators (linesUnboundedAscii)
import Data.Conduit.Combinators qualified as C
import Data.Either (partitionEithers)
import Data.EventSource (EventSink (..), EventSource (..), HasEventId (..))
import Data.EventSource.Rotation (EventStore (..))
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection, Only (..), Statement, close, closeStatement, execute, executeMany, execute_, nextRow, open, openStatement, query, query_, withTransaction)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

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
  deriving anyclass (ToJSON)

-- | Items in the write-behind queue: either an event to insert or a flush
-- marker that the writer thread signals after processing all preceding items.
--
-- Events are queued unencoded and CBOR-encoded on the writer thread: the
-- encoding of an event carrying a large payload otherwise sits on the caller's
-- thread. The bounded queue briefly pins event values instead of compact
-- bytes, but the writer drains whole-queue batches so the window is short.
type WriteItem e = Either (TMVar IO ()) (Word64, e)

-- | Bracket-style wrapper around 'mkSQLiteEventStore'. Creates the database,
-- schema, and writer thread, runs the callback, then flushes queued writes and
-- cancels the writer thread on exit. The writer thread is 'link'ed so that
-- crashes surface immediately in the calling thread.
--
-- If a legacy state file exists at @legacyStateFile@, events are migrated into
-- SQLite automatically before the callback runs.
withSQLiteEventStore ::
  forall e a.
  (ToCBOR e, FromCBOR e, FromJSON e, HasEventId e) =>
  Tracer IO SQLiteLog ->
  FilePath ->
  FilePath ->
  (EventStore e IO -> IO a) ->
  IO a
withSQLiteEventStore tracer dbFile legacyStateFile callback = do
  (conn, store, flush, reinitLastSeen, cleanup) <- mkSQLiteEventStore dbFile
  migrateFromFileBased (Proxy @e) tracer legacyStateFile conn reinitLastSeen
  callback store
    `finally` (flush >> cleanup >> close conn)

-- | Create an 'EventStore' backed by a SQLite database at the given file path.
-- The database and schema are created on first use if they do not exist.
-- Returns @(conn, store, flush, reinitLastSeen, cleanup)@. Internal —
-- prefer 'withSQLiteEventStore' which handles cleanup, migration, and flushing
-- automatically.
mkSQLiteEventStore ::
  forall e.
  (ToCBOR e, FromCBOR e, FromJSON e, HasEventId e) =>
  FilePath ->
  IO (Connection, EventStore e IO, IO (), IO (), IO ())
mkSQLiteEventStore dbFile = do
  createDirectoryIfMissing True (takeDirectory dbFile)
  conn <- open dbFile
  -- Rows of a version 1 database are JSON-encoded and get re-encoded to CBOR
  -- by the schema migration; a row that fails to decode aborts startup.
  let reencodeRow :: Word64 -> ByteString -> IO ByteString
      reencodeRow eid bytes =
        case Aeson.eitherDecodeStrict' @e bytes of
          Right evt -> pure $ serialize' evt
          Left err -> throwIO EventDecodingException{eventId = eid, decodeError = err}
  initSchema conn reencodeRow
  -- Dedicated connection for 'sourceEvents' streams, so concurrent history
  -- replay cannot hold statements open on the connection rotation runs
  -- VACUUM INTO on (see module header).
  readConn <- open dbFile
  configurePragmas readConn
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
      case decodeFull' evData of
        Right evt -> pure evt
        -- NOTE: This will prevent startup, which is intentional — starting
        -- with missing events would silently corrupt the aggregated state.
        Left err -> throwIO EventDecodingException{eventId = eid, decodeError = show err}

    sourceEvents :: ConduitT () e (ResourceT IO) ()
    sourceEvents = do
      -- Flush queued writes so reads see all enqueued events.
      liftIO $ flushWriteQueue writeQueue
      bracketP openStmt closeStatement yieldRows
     where
      openStmt :: IO Statement
      openStmt = getEventsASC readConn

      yieldRows :: Statement -> ConduitT () e (ResourceT IO) ()
      yieldRows stmt = do
        mRow <- liftIO (nextRow stmt)
        case mRow of
          Nothing -> pure ()
          Just row -> do
            evt <- liftIO (decodeRow row)
            yield evt
            yieldRows stmt

    enqueueEvent evt =
      atomically $ do
        writeTBQueue writeQueue (Right (getEventId evt, evt))
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
        atomically $ do
          forM_ newEvts $ \evt -> writeTBQueue writeQueue (Right (getEventId evt, evt))
          case nonEmpty newEvts of
            Just ne -> setLastSeenEventId (NE.last ne)
            Nothing -> pure ()

    rotate logId checkpointEvent = do
      flushWriteQueue writeQueue
      -- Archive the current database before removing events, so the
      -- pre-rotation log is retained (mirrors the old file-based backup).
      backupDatabase conn dbFile logId
      let evData = serialize' checkpointEvent
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
    , cancel writerThread >> close readConn
    )

-- | Background writer that drains the queue and batch-inserts into SQLite.
-- Each iteration blocks for at least one item, then flushes everything
-- available. Events are CBOR-encoded here, off the caller's thread, then
-- batch-inserted in a single transaction, and any flush markers in the batch
-- are signalled. Encode errors surface as writer thread crashes, which are
-- 'link'ed to the caller.
writerLoop :: ToCBOR e => Connection -> TBQueue IO (WriteItem e) -> IO ()
writerLoop conn queue = forever $ do
  first' <- atomically $ readTBQueue queue
  rest <- atomically $ flushTBQueue queue
  let allItems = first' : rest
      (flushSignals, events) = partitionEithers allItems
      eventRows = map (second serialize') events
  unless (null eventRows) $
    withTransaction conn $
      insertEvents conn eventRows
  forM_ flushSignals $ \mv -> atomically $ putTMVar mv ()

-- | Block until all items currently in the write queue have been flushed to
-- SQLite. Sends a flush marker through the queue and waits for the writer thread
-- to signal completion.
flushWriteQueue :: TBQueue IO (WriteItem e) -> IO ()
flushWriteQueue queue = do
  mv <- newEmptyTMVarIO
  atomically $ writeTBQueue queue (Left mv)
  atomically $ takeTMVar mv

-- | Migrate events from a legacy newline-delimited JSON file into SQLite.
-- Writes directly to the database, bypassing the async write queue (migration
-- runs at startup before inputs are processed). After inserting, calls
-- @reinitLastSeen@ to sync the in-memory event id TVar with the database.
--
-- Safe to call when the legacy file does not exist (no-op). Not safe to re-run:
-- duplicate event ids will cause a primary key constraint violation.
--
-- On success the legacy file is renamed to @<path>.migrated@ so that
-- subsequent restarts skip the migration step automatically.
migrateFromFileBased ::
  forall e.
  (FromJSON e, ToCBOR e, HasEventId e) =>
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
      -- Decode each JSON line (legacy files are always JSON) and store the
      -- event re-encoded as CBOR. Invalid JSON is caught here so corrupt
      -- files fail at migration.
      rowParams <- forM (zip [1 ..] rawLines) $ \(lineNo :: Int, line) ->
        case Aeson.eitherDecodeStrict' @e line of
          Right evt -> pure (getEventId evt, serialize' evt)
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
-- 'migrateStep' whenever the schema changes.
nextVersion :: Int
nextVersion = 2

-- | Re-encode a single event row given its event id and stored bytes, used by
-- the version 1 (JSON) to version 2 (CBOR) migration. Must throw when the row
-- cannot be decoded.
type ReencodeRow = Word64 -> ByteString -> IO ByteString

-- | Initialise connection pragmas, then create or migrate the schema to
-- 'nextVersion' using SQLite's built-in @user_version@ pragma.
initSchema :: Connection -> ReencodeRow -> IO ()
initSchema conn reencodeRow = do
  configurePragmas conn
  v <- getSchemaVersion conn
  applyMigrations conn reencodeRow v
  -- Reclaim the space freed by the JSON -> CBOR re-encode. VACUUM cannot run
  -- inside a transaction and is a space optimization only: a crash between
  -- the migration commit and here costs disk space, not correctness.
  when (v == 1) $ execute_ conn "VACUUM"

configurePragmas :: Connection -> IO ()
configurePragmas conn =
  mapM_
    (execute_ conn)
    [ "PRAGMA journal_mode=WAL"
    , "PRAGMA busy_timeout=5000"
    , -- With WAL, NORMAL skips per-write fsyncs and only syncs during
      -- checkpoints — safe when the source of truth is external.
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
-- Each step runs together with its version bump in one transaction
-- (@PRAGMA user_version@ is transactional), so a crash or decoding failure
-- mid-migration rolls back to a well-defined version.
applyMigrations :: Connection -> ReencodeRow -> Int -> IO ()
applyMigrations conn reencodeRow v
  | v > nextVersion =
      error $ "Database schema version " <> show v <> " is newer than supported " <> show nextVersion <> ", cannot downgrade"
  | v == nextVersion = pure ()
  | otherwise = do
      withTransaction conn $ do
        migrateStep conn reencodeRow v
        setSchemaVersion conn (v + 1)
      applyMigrations conn reencodeRow (v + 1)

-- | Individual migration steps. Pattern-match on the /source/ version.
migrateStep :: Connection -> ReencodeRow -> Int -> IO ()
migrateStep conn reencodeRow = \case
  0 -> createEventsTable conn
  1 -> reencodeAllEvents conn reencodeRow
  unknown ->
    error $ "Unknown schema version " <> show unknown <> ", cannot migrate"

-- | Re-encode all event rows using the given 'ReencodeRow' function (the
-- version 1 JSON to version 2 CBOR migration). Rows are processed in batches
-- of ascending event id so memory stays bounded for large databases. Runs
-- inside the caller's transaction.
reencodeAllEvents :: Connection -> ReencodeRow -> IO ()
reencodeAllEvents conn reencodeRow = go 0
 where
  batchSize = 1000 :: Int

  go :: Word64 -> IO ()
  go startId = do
    rows :: [(Word64, ByteString)] <-
      query conn "SELECT event_id, event_data FROM events WHERE event_id >= ? ORDER BY event_id LIMIT ?" (startId, batchSize)
    case nonEmpty rows of
      Nothing -> pure ()
      Just neRows -> do
        updates <- forM rows $ \(eid, evData) -> do
          encoded <- reencodeRow eid evData
          pure (encoded, eid)
        executeMany conn "UPDATE events SET event_data = ? WHERE event_id = ?" updates
        go (fst (NE.last neRows) + 1)

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

-- | Archive the current database before rotation removes the events, into an
-- @old-state@ subdirectory next to the database, with the log id inserted
-- before the extension (e.g. @old-state/state-42.db@). Uses @VACUUM INTO@ so
-- the snapshot reflects all committed (WAL) data in a single self-contained
-- file, regardless of WAL checkpoint state. The destination is removed first if
-- present (e.g. a re-rotation at the same log id), since @VACUUM INTO@ requires
-- it not to exist.
backupDatabase :: Connection -> FilePath -> Word64 -> IO ()
backupDatabase conn dbFile logId = do
  let backupDir = takeDirectory dbFile </> "old-state"
      backupPath = backupDir </> (takeBaseName dbFile <> "-" <> show logId <> takeExtension dbFile)
  createDirectoryIfMissing True backupDir
  doesFileExist backupPath >>= \exists -> when exists (removeFile backupPath)
  execute conn "VACUUM INTO ?" (Only backupPath)
