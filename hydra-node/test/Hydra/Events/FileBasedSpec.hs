-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Hydra.Events.FileBased'.
module Hydra.Events.FileBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..), getEvents, putEvent)
import Hydra.Events.FileBased (eventPairFromPersistenceIncremental)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Test.QuickCheck (forAllShrink, ioProperty, sublistOf, (===))
import Test.QuickCheck.Gen (listOf)

spec :: Spec
spec = do
  describe "eventPairFromPersistenceIncremental" $ do
    prop "can handle continuous events" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $ do
          withEventSourceAndSink $ \EventSource{getEvents} EventSink{putEvent} -> do
            forM_ events putEvent
            loadedEvents <- getEvents
            pure $
              loadedEvents === events

    prop "can handle non-continuous events" $
      forAllShrink (sublistOf =<< genContinuousEvents) shrink $ \events ->
        ioProperty $ do
          withEventSourceAndSink $ \EventSource{getEvents} EventSink{putEvent} -> do
            forM_ events putEvent
            loadedEvents <- getEvents
            pure $
              loadedEvents === events

    prop "can handle duplicate events" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $
          withEventSourceAndSink $ \EventSource{getEvents} EventSink{putEvent} -> do
            -- Put some events
            forM_ events putEvent
            loadedEvents <- getEvents
            -- Put the loaded events again (as the node would do)
            forM_ loadedEvents putEvent
            allEvents <- getEvents
            pure $
              allEvents === loadedEvents

    prop "can bootstrap from plain StateChanged events" $
      forAllShrink genContinuousEvents shrink $ \events -> do
        ioProperty $ do
          withTempDir "hydra-persistence" $ \tmpDir -> do
            -- Store state changes directly (legacy)
            let stateChanges = map stateChanged events
            PersistenceIncremental{append} <- createPersistenceIncremental (tmpDir <> "/data")
            forM_ stateChanges append
            -- Load and store events through the event source interface
            (EventSource{getEvents}, EventSink{putEvent}) <-
              eventPairFromPersistenceIncremental
                =<< createPersistenceIncremental (tmpDir <> "/data")
            loadedEvents <- getEvents
            -- Store all loaded events like the node would do
            forM_ loadedEvents putEvent
            pure $
              map stateChanged loadedEvents === stateChanges

genContinuousEvents :: Gen [StateEvent SimpleTx]
genContinuousEvents =
  zipWith StateEvent [0 ..] <$> listOf arbitrary

withEventSourceAndSink :: (EventSource (StateEvent SimpleTx) IO -> EventSink (StateEvent SimpleTx) IO -> IO b) -> IO b
withEventSourceAndSink action =
  withTempDir "hydra-persistence" $ \tmpDir -> do
    (eventSource, eventSink) <-
      eventPairFromPersistenceIncremental
        =<< createPersistenceIncremental (tmpDir <> "/data")
    action eventSource eventSink
