-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Hydra.Events.SQLiteBased'.
module Hydra.Events.SQLiteBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (zipWith3)
import Database.SQLite.Simple (execute, execute_, open)
import Hydra.Events (EventSink (..), EventSource (..), getEvents, putEvent)
import Hydra.Events.Rotation (EventStore (..))
import Hydra.Events.SQLiteBased (EventDecodingException, SQLiteLog (..), getSchemaVersion, nextVersion, withSQLiteEventStore)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Envelope (..), nullTracer)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Ledger.Simple ()
import Test.QuickCheck (forAllShrink, ioProperty, sublistOf, (===))
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
          -- Insert a row with invalid JSON directly via a separate connection
          conn <- open dbFile
          execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (1 :: Word64, "not valid json" :: ByteString)
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
          conn <- open dbFile
          v <- getSchemaVersion conn
          v `shouldBe` nextVersion

    it "opening the database twice does not fail" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \_ -> pure ()
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \_ -> do
          conn <- open dbFile
          v <- getSchemaVersion conn
          v `shouldBe` nextVersion

    it "rejects a database with a newer schema version" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        -- Create a DB with a version beyond what we know
        conn <- open dbFile
        execute_ conn $ fromString $ "PRAGMA user_version = " <> show (nextVersion + 1)
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile (\_ -> pure ())
          `shouldThrow` anyErrorCall

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

genContinuousEvents :: Gen [StateEvent SimpleTx]
genContinuousEvents =
  zipWith3 StateEvent [0 ..] <$> listOf arbitrary <*> listOf arbitrary

withEventSourceAndSink :: (EventSource (StateEvent SimpleTx) IO -> EventSink (StateEvent SimpleTx) IO -> IO b) -> IO b
withEventSourceAndSink action =
  withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
    let dbFile = tmpDir <> "/hydra.db"
        stateFile = tmpDir <> "/state"
    withSQLiteEventStore nullTracer dbFile stateFile $ \EventStore{eventSource, eventSink} ->
      action eventSource eventSink
