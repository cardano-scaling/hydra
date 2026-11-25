-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Data.EventSource.SQLite'.
module Hydra.Events.SQLiteBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (putTMVar, takeTMVar, tryPutTMVar)
import Control.Tracer (Tracer (..), emit)
import Control.Tracer.JSON (nullTracer)
import Data.EventSource (EventSink (..), EventSource (..), getEvents, putEvent)
import Data.EventSource.Rotation (EventStore (..))
import Data.EventSource.SQLite (EventDecodingException, SQLiteLog (..), getSchemaVersion, nextVersion, withSQLiteEventStore)
import Data.List (zipWith3)
import Data.List qualified as List
import Database.SQLite.Simple (close, execute, open)
import Hydra.HeadLogic.Outcome (StateChanged (NetworkConnected))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Simple (SimpleTx)
import System.Directory (doesFileExist)
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Ledger.Simple ()
import Test.QuickCheck (forAllShrink, generate, ioProperty, sublistOf, suchThat, (===))
import Test.QuickCheck.Gen (listOf)

spec :: Spec
spec = do
  describe "bounded batch writes" $
    forM_ [999, 1000, 1001, 2501 :: Int] $ \count ->
      it ("persists and deduplicates a batch of " <> show count <> " events") $
        failAfter 10 $
          withEventSourceAndSink $ \source sink -> do
            now <- getCurrentTime
            let events = [StateEvent i NetworkConnected now | i <- [0 .. fromIntegral count - 1]]
            putEvents sink events
            putEvents sink events
            getEvents source `shouldReturn` events

  it "reports SQLite completion only after the batch commits" $
    failAfter 5 $
      withTempDir "sqlite-completion" $ \dir -> do
        started <- newLabelledEmptyTMVarIO "write-started"
        allowCommit <- newLabelledEmptyTMVarIO "allow-commit"
        observations <- newIORef []
        now <- getCurrentTime
        let tracer = Tracer . emit $ \message -> do
              atomicModifyIORef' observations $ \events -> (message : events, ())
              case message of
                SQLiteWriteStarted -> atomically (putTMVar started ()) >> atomically (takeTMVar allowCommit)
                _ -> pure ()
            event = StateEvent 0 NetworkConnected now :: StateEvent SimpleTx
        withSQLiteEventStore tracer (dir <> "/hydra.db") (dir <> "/state") $ \EventStore{eventSink, eventSource} -> do
          putEvent eventSink event
          bracket_
            (atomically $ takeTMVar started)
            (void . atomically $ tryPutTMVar allowCommit ())
            ( do
                samples <- readIORef observations
                [writeSeconds | SQLiteWriteCompleted{writeSeconds} <- samples] `shouldBe` []
            )
          getEvents eventSource `shouldReturn` [event]
          samples <- readIORef observations
          [writeSeconds >= 0 | SQLiteWriteCompleted{writeSeconds} <- samples] `shouldBe` [True]

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

    it "fresh database ends up at nextVersion" $ do
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        withSQLiteEventStore @(StateEvent SimpleTx) nullTracer dbFile stateFile $ \_ -> do
          v <- bracket (open dbFile) close getSchemaVersion
          v `shouldBe` nextVersion

    it "reopens the current format and replays its persisted events" $
      withTempDir "hydra-sqlite-persistence" $ \tmpDir -> do
        let dbFile = tmpDir <> "/hydra.db"
            stateFile = tmpDir <> "/state"
        event :: StateEvent SimpleTx <- generate arbitrary
        withSQLiteEventStore nullTracer dbFile stateFile $ \EventStore{eventSink} ->
          putEvent eventSink event
        withSQLiteEventStore nullTracer dbFile stateFile $ \EventStore{eventSource} -> do
          getEvents eventSource `shouldReturn` [event]
          bracket (open dbFile) close getSchemaVersion `shouldReturn` nextVersion

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
