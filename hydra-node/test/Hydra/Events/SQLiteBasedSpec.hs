-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Hydra.Events.SQLiteBased'.
module Hydra.Events.SQLiteBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (zipWith3)
import Hydra.Events (EventSink (..), EventSource (..), getEvents, putEvent)
import Hydra.Events.Rotation (EventStore (..))
import Hydra.Events.SQLiteBased (migrateFromFileBased, mkSQLiteEventStore)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Simple (SimpleTx)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Ledger.Simple ()
import Test.QuickCheck (forAllShrink, ioProperty, sublistOf, (===))
import Test.QuickCheck.Gen (listOf)

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
            (conn, store) <- mkSQLiteEventStore dbFile
            migrateFromFileBased legacyFile conn store
            -- Verify all events are present
            loadedEvents <- getEvents (eventSource store)
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
