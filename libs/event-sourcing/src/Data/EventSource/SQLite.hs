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
-- == Persistence compatibility
--
-- @PRAGMA user_version@ identifies both the schema and event format. Version 3
-- uses the current HeadLogic checkpoint layouts. Older databases and legacy
-- state files are rejected before replay, without migration. A row that fails
-- to decode in a supported database aborts replay with 'EventDecodingException'.
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
import Conduit (ConduitT, ResourceT, bracketP, yield)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.Labelled (newLabelledTVarIO)
import Control.Concurrent.Class.MonadSTM (
  TBQueue,
  TMVar,
  atomically,
  flushTBQueue,
  lengthTBQueue,
  newEmptyTMVarIO,
  newTBQueueIO,
  putTMVar,
  readTBQueue,
  readTVar,
  takeTMVar,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (Exception (displayException), finally, onException, throwIO)
import Control.Monad (forM_, forever, unless, when)
import Control.Monad.Class.MonadAsync (async, cancel, link)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.EventSource (EventSink (..), EventSource (..), HasEventId (..))
import Data.EventSource.Rotation (EventStore (..))
import Data.String (fromString)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection, Only (..), Statement, close, closeStatement, execute, executeMany, execute_, nextRow, open, openStatement, query_, withTransaction)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

-- | Exception thrown when a persisted event cannot be decoded.
data EventDecodingException = EventDecodingException
  { eventId :: Word64
  , decodeError :: String
  }
  deriving stock (Show)

instance Exception EventDecodingException

data PersistenceException
  = IncompatibleDatabase FilePath Int
  | LegacyStateFile FilePath
  deriving stock (Eq, Show)

instance Exception PersistenceException where
  displayException = \case
    IncompatibleDatabase path version ->
      "Incompatible persistence format in "
        <> show path
        <> ": database version "
        <> show version
        <> ", expected "
        <> show nextVersion
        <> ". No automatic migration is available."
    LegacyStateFile path ->
      "Incompatible legacy state file " <> show path <> ". No automatic migration is available."

data SQLiteLog
  = SQLiteStats {queueDepth :: Int}
  | SQLiteWriteStarted
  | SQLiteWriteCompleted {writeSeconds :: Double}
  | SQLiteWriteFailed
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

writeQueueCapacity :: Int
writeQueueCapacity = 1000

-- | Bracket-style wrapper around 'mkSQLiteEventStore'. Creates the database,
-- schema, and writer thread, runs the callback, then flushes queued writes and
-- cancels the writer thread on exit. The writer thread is 'link'ed so that
-- crashes surface immediately in the calling thread.
--
-- A legacy state file is rejected before opening the database.
withSQLiteEventStore ::
  forall e a.
  (ToCBOR e, FromCBOR e, HasEventId e) =>
  Tracer IO SQLiteLog ->
  FilePath ->
  FilePath ->
  (EventStore e IO -> IO a) ->
  IO a
withSQLiteEventStore tracer dbFile legacyStateFile callback = do
  legacyExists <- doesFileExist legacyStateFile
  when legacyExists $ throwIO $ LegacyStateFile legacyStateFile
  (conn, store, flush, cleanup) <- mkSQLiteEventStore tracer dbFile
  callback store
    `finally` (flush >> cleanup >> close conn)

-- | Create an 'EventStore' backed by a SQLite database at the given file path.
-- The database and schema are created on first use if they do not exist.
-- Returns @(conn, store, flush, cleanup)@. Internal —
-- prefer 'withSQLiteEventStore' which handles cleanup and flushing
-- automatically.
mkSQLiteEventStore ::
  forall e.
  (ToCBOR e, FromCBOR e, HasEventId e) =>
  Tracer IO SQLiteLog ->
  FilePath ->
  IO (Connection, EventStore e IO, IO (), IO ())
mkSQLiteEventStore tracer dbFile = do
  createDirectoryIfMissing True (takeDirectory dbFile)
  conn <- open dbFile
  initSchema dbFile conn `onException` close conn
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

  writeQueue <- newTBQueueIO $ fromIntegral writeQueueCapacity
  writerThread <- async $ writerLoop tracer conn writeQueue
  statsThread <- async $ forever $ do
    queueDepth <- fromIntegral <$> atomically (lengthTBQueue writeQueue)
    traceWith tracer SQLiteStats{queueDepth}
    threadDelay 1000000
  link writerThread
  link statsThread
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

    enqueueEvent evt = do
      lastSeen <- getLastSeenEventId
      when (maybe True (getEventId evt >) lastSeen) $ do
        writeTBQueue writeQueue (Right (getEventId evt, evt))
        setLastSeenEventId evt

    putEvent = atomically . enqueueEvent

    -- Bound each enqueue transaction so the writer can drain between chunks.
    putEvents [] = pure ()
    putEvents evts = do
      let (batch, rest) = splitAt writeQueueCapacity evts
      atomically $ forM_ batch enqueueEvent
      putEvents rest

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

  pure
    ( conn
    , EventStore
        { eventSource = EventSource{sourceEvents}
        , eventSink = EventSink{putEvent, putEvents}
        , rotate
        }
    , flushWriteQueue writeQueue
    , cancel statsThread >> cancel writerThread >> close readConn
    )

-- | Background writer that drains the queue and batch-inserts into SQLite.
-- Each iteration blocks for at least one item, then flushes everything
-- available. Events are CBOR-encoded here, off the caller's thread, then
-- batch-inserted in a single transaction, and any flush markers in the batch
-- are signalled. Encode errors surface as writer thread crashes, which are
-- 'link'ed to the caller.
writerLoop :: ToCBOR e => Tracer IO SQLiteLog -> Connection -> TBQueue IO (WriteItem e) -> IO ()
writerLoop tracer conn queue = forever $ do
  first' <- atomically $ readTBQueue queue
  rest <- atomically $ flushTBQueue queue
  let allItems = first' : rest
      (flushSignals, events) = partitionEithers allItems
      eventRows = map (second serialize') events
  unless (null eventRows) $ do
    started <- getMonotonicTimeNSec
    traceWith tracer SQLiteWriteStarted
    withTransaction conn (insertEvents conn eventRows)
      `onException` traceWith tracer SQLiteWriteFailed
    finished <- getMonotonicTimeNSec
    traceWith tracer $ SQLiteWriteCompleted (secondsBetween finished started)
  forM_ flushSignals $ \mv -> atomically $ putTMVar mv ()

-- | Block until all items currently in the write queue have been flushed to
-- SQLite. Sends a flush marker through the queue and waits for the writer thread
-- to signal completion.
flushWriteQueue :: TBQueue IO (WriteItem e) -> IO ()
flushWriteQueue queue = do
  mv <- newEmptyTMVarIO
  atomically $ writeTBQueue queue (Left mv)
  atomically $ takeTMVar mv

-- Internal

-- | Current schema and event format. Older formats have no migration.
nextVersion :: Int
nextVersion = 3

-- | Reject incompatible stores before changing pragmas or reading events.
-- Only an empty, unversioned database can be initialised as a new store.
initSchema :: FilePath -> Connection -> IO ()
initSchema dbFile conn = do
  version <- getSchemaVersion conn
  case version of
    0 -> do
      tables :: [Only String] <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'table'"
      unless (null tables) $ throwIO $ IncompatibleDatabase dbFile version
      configurePragmas conn
      withTransaction conn $ do
        createEventsTable conn
        setSchemaVersion conn nextVersion
    _ | version == nextVersion -> configurePragmas conn
    _ -> throwIO $ IncompatibleDatabase dbFile version

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

secondsBetween :: Word64 -> Word64 -> Double
secondsBetween end start = fromIntegral (end - start) / 1.0e9
