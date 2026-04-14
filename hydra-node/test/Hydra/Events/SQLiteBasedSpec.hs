-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Hydra.Events.SQLiteBased'.
module Hydra.Events.SQLiteBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (zipWith3)
import Database.SQLite.Simple (execute)
import Hydra.Events (EventSink (..), EventSource (..), getEvents, putEvent)
import Hydra.Events.Rotation (EventStore (..))
import Hydra.Events.SQLiteBased (EventDecodingException, SQLiteLog (..), migrateFromFileBased, mkSQLiteEventStore)
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
        (conn, store) <- mkSQLiteEventStore @(StateEvent SimpleTx) dbFile
        -- Insert a row with invalid JSON directly into the database
        execute conn "INSERT INTO events (event_id, event_data) VALUES (?, ?)" (1 :: Word64, "not valid json" :: Text)
        getEvents (eventSource store)
          `shouldThrow` \(_ :: EventDecodingException) -> True

    it "throws EventDecodingException on invalid lines during migration" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let legacyFile = tmpDir <> "/state"
        let dbFile = tmpDir <> "/hydra.db"
        writeFileBS legacyFile "{invalid json\n"
        (conn, store) <- mkSQLiteEventStore @(StateEvent SimpleTx) dbFile
        migrateFromFileBased nullTracer legacyFile conn store
          `shouldThrow` \(_ :: EventDecodingException) -> True

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
            (conn, store) <- mkSQLiteEventStore dbFile
            migrateFromFileBased tracer legacyFile conn store
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
    (_, EventStore{eventSource, eventSink}) <- mkSQLiteEventStore dbFile
    action eventSource eventSink
