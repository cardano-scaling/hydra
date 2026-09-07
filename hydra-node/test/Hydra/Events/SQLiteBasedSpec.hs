-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Data.EventSource.SQLite'.
module Hydra.Events.SQLiteBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull')
import Control.Tracer.JSON (Envelope (..), nullTracer)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.EventSource (EventSink (..), EventSource (..), getEvents, putEvent)
import Data.EventSource.Rotation (EventStore (..))
import Data.EventSource.SQLite (EventDecodingException, SQLiteLog (..), getSchemaVersion, nextVersion, withSQLiteEventStore)
import Data.List (zipWith3)
import Data.List qualified as List
import Database.SQLite.Simple (Only (..), close, execute, execute_, open, query)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import System.Directory (doesFileExist, getFileSize)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Ledger.Simple ()
import Test.QuickCheck (forAllShrink, generate, ioProperty, sublistOf, suchThat, vectorOf, (===))
import Test.QuickCheck.Gen (listOf)
import Test.Util (captureTracer)

spec :: Spec
spec = do
  describe "mkSQLiteEventStore" $ do
    prop "can stream events" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $
          withEventSourceAndSink $ \EventSource{sourceEvents} EventSink{putEvent} -> do
            forM_ events putEvent
            -- XXX: Should assert while streaming
            streamedEvents <- getEvents (EventSource sourceEvents)
            pure $
              streamedEvents === events

    prop "can handle continuous events" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $ do
          withEventSourceAndSink $ \src EventSink{putEvent} -> do
            forM_ events putEvent
            loadedEvents <- getEvents src
            pure $
              loadedEvents === events

    prop "can handle non-continuous events" $
      forAllShrink (sublistOf =<< genContinuousEvents) shrink $ \events ->
        ioProperty $ do
          withEventSourceAndSink $ \src EventSink{putEvent} -> do
            forM_ events putEvent
            loadedEvents <- getEvents src
            pure $
              loadedEvents === events

    prop "can handle duplicate events" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $
          withEventSourceAndSink $ \src EventSink{putEvent} -> do
            forM_ events putEvent
            loadedEvents <- getEvents src
            -- Put the loaded events again (as the node would do)
            forM_ loadedEvents putEvent
            allEvents <- getEvents src
            pure $
              allEvents === loadedEvents

    it "throws EventDecodingException on invalid data in database" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \store -> do
          -- Insert a row with undecodable data directly via a separate connection
          bracket (open dbFile) close $ \conn ->
            execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (1 :: Word64, "not valid cbor" :: ByteString)
          getEvents (eventSource store)
            `shouldThrow` \(_ :: EventDecodingException) -> True

    it "throws EventDecodingException on invalid lines during migration" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let legacyFile = tmpDir <> "/state"
        let dbFile = tmpDir <> "/hydra.db"
        writeFileBS legacyFile "{invalid json\n"
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile legacyFile (\_ -> pure ())
          `shouldThrow` \(_ :: EventDecodingException) -> True

    it "fresh database ends up at nextVersion" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \_ -> do
          v <- bracket (open dbFile) close getSchemaVersion
          v `shouldBe` nextVersion

    it "opening the database twice does not fail" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \_ -> pure ()
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \_ -> do
          v <- bracket (open dbFile) close getSchemaVersion
          v `shouldBe` nextVersion

    it "rejects a database with a newer schema version" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        -- Create a DB with a version beyond what we know
        bracket (open dbFile) close $ \conn ->
          execute_ conn $ fromString $ "PRAGMA user_version = " <> show (nextVersion + 1)
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile (\_ -> pure ())
          `shouldThrow` anyErrorCall

    it "migrates a v1 JSON database to v2 CBOR" $
      migratesV1JsonToV2CBOR 50

    it "aborts migration and keeps v1 intact on a corrupt row" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        goodEvent :: StateEvent SimpleTx <- generate arbitrary
        bracket (open dbFile) close $ \conn -> do
          execute_ conn "CREATE TABLE events (event_id INTEGER NOT NULL PRIMARY KEY, event_data BLOB NOT NULL)"
          execute_ conn "PRAGMA user_version = 1"
          execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (1 :: Word64, toStrict (Aeson.encode goodEvent))
          execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (2 :: Word64, "not valid json" :: ByteString)
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile (\_ -> pure ())
          `shouldThrow` \(_ :: EventDecodingException) -> True
        -- The failed migration must roll back: still version 1, rows untouched.
        v <- bracket (open dbFile) close getSchemaVersion
        v `shouldBe` 1

    prop "can migrate from file-based store" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $ do
          withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
            let legacyFile = tmpDir <> "/state"
            let dbFile = tmpDir <> "/hydra.db"
            -- Write events to the legacy file
            forM_ events $ \e ->
              BS.appendFile legacyFile (toStrict (Aeson.encode e) <> "\n")
            -- Migrate into SQLite
            (tracer, getTraces) <- captureTracer "sqlite"
            withSQLiteEventStore tracer dbFile legacyFile $ \store -> do
              -- Verify all events are present
              loadedEvents <- getEvents (eventSource store)
              -- Verify migration was logged
              traces <- getTraces
              let msgs = fmap message traces
              unless (null events) $
                msgs `shouldSatisfy` elem MigrationComplete{legacyFile}
              pure $
                loadedEvents === events

    it "keeps a backup of the database before rotating" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        events <- generate $ genContinuousEvents `suchThat` (not . null)
        let lastId = eventId (List.last events)
            -- content is irrelevant here; we only assert on the archived events
            checkpointEvent = List.last events
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \EventStore{eventSink = EventSink{putEvent}, eventSource, rotate} -> do
          forM_ events putEvent
          rotate lastId checkpointEvent
          -- the active store holds only the checkpoint after rotation
          active <- getEvents eventSource
          active `shouldBe` [checkpointEvent]
        -- the pre-rotation events are retained in the numbered backup
        let backupPath = tmpDir <> "/old-state/hydra-" <> show lastId <> ".db"
        doesFileExist backupPath `shouldReturn` True
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer backupPath stateFile $ \EventStore{eventSource} -> do
          backedUp <- getEvents eventSource
          backedUp `shouldBe` events

-- | Create a v1 database with @n@ JSON-encoded 'StateEvent Tx' rows (as
-- written by hydra-node versions before the CBOR switch), open it (triggering
-- the migration) and assert nothing is lost: the events load back equal, and
-- each migrated row's raw CBOR decodes to exactly the event that was inserted
-- as JSON. Uses the production event type ('Tx', the one real hydra.db files
-- store) rather than 'SimpleTx'.
migratesV1JsonToV2CBOR :: Int -> IO ()
migratesV1JsonToV2CBOR n =
  withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
    let dbFile = tmpDir <> "/hydra.db"
        stateFile = tmpDir <> "/state"
    events <- generate $ mkContinuousEvents @Tx <$> vectorOf n arbitrary <*> vectorOf n arbitrary
    bracket (open dbFile) close $ \conn -> do
      execute_ conn "CREATE TABLE events (event_id INTEGER NOT NULL PRIMARY KEY, event_data BLOB NOT NULL)"
      execute_ conn "PRAGMA user_version = 1"
      forM_ events $ \e ->
        execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (eventId e, toStrict (Aeson.encode e))
    sizeBefore <- getFileSize dbFile
    withSQLiteEventStore @(StateEvent Tx) nullTracer dbFile stateFile $ \store -> do
      loadedEvents <- getEvents (eventSource store)
      loadedEvents `shouldBe` events
    v <- bracket (open dbFile) close getSchemaVersion
    v `shouldBe` nextVersion
    -- Random events from the original JSON database must be found in the
    -- migrated database under the same event_id, with the row blob
    -- decoding (as CBOR) to exactly the event that was inserted.
    picked <- generate $ sublistOf events `suchThat` (not . null)
    bracket (open dbFile) close $ \conn ->
      forM_ picked $ \e -> do
        rows :: [Only ByteString] <-
          query conn "SELECT event_data FROM events WHERE event_id = ?" (Only (eventId e))
        case rows of
          [Only bytes] ->
            case decodeFull' bytes of
              Left err ->
                expectationFailure $
                  "failed to decode migrated row " <> show (eventId e) <> " as CBOR: " <> show err
              Right (decoded :: StateEvent Tx) -> decoded `shouldBe` e
          _ ->
            expectationFailure $
              "expected exactly one row for event_id " <> show (eventId e) <> ", got " <> show (length rows)
    -- On real 'Tx' events transaction/UTxO bytes dominate and JSON stores
    -- them hex-encoded, so re-encoding to CBOR + VACUUM must strictly shrink
    -- the database — this locks in the compression win of the migration.
    sizeAfter <- getFileSize dbFile
    sizeAfter `shouldSatisfy` (< sizeBefore)

genContinuousEvents :: Gen [StateEvent SimpleTx]
genContinuousEvents =
  mkContinuousEvents <$> listOf arbitrary <*> listOf arbitrary

mkContinuousEvents :: [StateChanged tx] -> [UTCTime] -> [StateEvent tx]
mkContinuousEvents = zipWith3 StateEvent [0 ..]

withEventSourceAndSink :: (EventSource (StateEvent SimpleTx) IO -> EventSink (StateEvent SimpleTx) IO -> IO b) -> IO b
withEventSourceAndSink action =
  withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
    let dbFile = tmpDir <> "/hydra.db"
        stateFile = tmpDir <> "/state"
    withSQLiteEventStore nullTracer dbFile stateFile $ \EventStore{eventSource, eventSink} ->
      action eventSource eventSink
