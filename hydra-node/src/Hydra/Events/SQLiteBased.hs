-- | A SQLite-backed event source and sink.
--
-- This is the recommended persistence backend for new deployments.
-- See 'migrateFromFileBased' for upgrading from the legacy file-based store.
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
-- access races. Operations that need data flushed (rotation, reads in tests)
-- use a flush marker: a 'TMVar' is enqueued and the caller blocks until the
-- writer thread has processed all preceding items and signalled it.
--
-- == Tradeoffs
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
import Control.Monad.Class.MonadAsync (async)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Combinators (linesUnboundedAscii)
import Data.Conduit.Combinators qualified as C
import Database.SQLite.Simple (Connection, Only (..), Statement, closeStatement, execute, executeMany, execute_, nextRow, open, openStatement, query_, withTransaction)
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

-- | Create an 'EventStore' backed by a SQLite database at the given file path.
-- The database and schema are created on first use if they do not exist.
-- Returns the underlying 'Connection' (for 'migrateFromFileBased'), a flush
-- action that blocks until all queued writes have been persisted to SQLite,
-- and a @reinitLastSeen@ action that re-reads the last event id from the
-- database into the in-memory TVar (used after migration).
mkSQLiteEventStore ::
  forall e.
  (ToJSON e, FromJSON e, HasEventId e) =>
  FilePath ->
  IO (Connection, EventStore e IO, IO (), IO ())
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

  writeQueue <- newTBQueueIO 1000
  -- NOTE: We intentionally do not 'link' the writer thread. In tests the
  -- TBQueue becomes unreachable when the test scope exits, causing io-classes
  -- to throw a BlockedIndefinitely async exception that cannot be reliably
  -- caught due to MonadCatch/io-classes semantics. Without 'link' the writer
  -- thread exits silently on cleanup. In production the writer never dies; if
  -- it did, 'flushWriteQueue' would block indefinitely, stalling the node.
  _writerThread <- async $ writerLoop conn writeQueue
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
        execute_ conn "DELETE FROM events"
        execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (getEventId checkpointEvent, evData :: ByteString)
      atomically $ setLastSeenEventId checkpointEvent

  let reinitLastSeen = do
        latestRows <- query_ conn "SELECT event_id FROM events ORDER BY event_id DESC LIMIT 1" :: IO [Only Word64]
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
      executeMany conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" eventRows
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
          executeMany conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" rowParams
      -- Re-read the last event id from the database so the in-memory
      -- de-duplication TVar is consistent with the migrated rows.
      reinitLastSeen
      renameFile legacyFile (legacyFile <> ".migrated")
      traceWith tracer MigrationComplete{legacyFile}

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
    \(event_id INTEGER NOT NULL PRIMARY KEY, event_data BLOB NOT NULL)"
