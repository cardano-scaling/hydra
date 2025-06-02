-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Hydra.Events.FileBased'.
module Hydra.Events.FileBasedSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

-- IsChainState tx instance to serialize 'StateEvent Tx'
import Hydra.Chain.Direct.State ()

import Conduit (runConduitRes, sinkList, (.|))
import Data.List (zipWith3)
import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..), getEvents, putEvent)
import Hydra.Events.FileBased (eventPairFromPersistenceIncremental)
import Hydra.HeadLogic (StateChanged)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Test.Aeson.GenericSpecs (
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
  roundtripAndGoldenSpecsWithSettings,
  sampleSize,
 )
import Test.QuickCheck (forAllShrink, ioProperty, sublistOf, (===))
import Test.QuickCheck.Gen (listOf)

spec :: Spec
spec = do
  describe "persisted event format" $ do
    -- NOTE: Whenever one of these fails, make sure to record a **BREAKING** change of the persisted 'state'.
    roundtripAndGoldenSpecsWithSettings (defaultSettings{sampleSize = 1}) (Proxy @(MinimumSized (StateEvent Tx)))
    roundtripAndGoldenADTSpecsWithSettings (defaultSettings{sampleSize = 1}) (Proxy @(MinimumSized (StateChanged Tx)))

  describe "eventPairFromPersistenceIncremental" $ do
    prop "can stream events" $
      forAllShrink genContinuousEvents shrink $ \events ->
        ioProperty $
          withEventSourceAndSink $ \EventSource{sourceEvents} EventSink{putEvent} -> do
            -- Put some events
            forM_ events putEvent
            -- XXX: Should assert while streaming
            streamedEvents <- runConduitRes $ sourceEvents .| sinkList
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
            -- Put some events
            forM_ events putEvent
            loadedEvents <- getEvents src
            -- Put the loaded events again (as the node would do)
            forM_ loadedEvents putEvent
            allEvents <- getEvents src
            pure $
              allEvents === loadedEvents

    prop "can bootstrap from plain StateChanged events" $
      forAllShrink genContinuousEvents shrink $ \events -> do
        ioProperty $ do
          withTempDir "hydra-persistence" $ \tmpDir -> do
            PersistenceIncremental{append} <- createPersistenceIncremental (tmpDir <> "/data")
            forM_ events append
            -- Load and store events through the event source interface
            (src, EventSink{putEvent}) <-
              eventPairFromPersistenceIncremental
                =<< createPersistenceIncremental (tmpDir <> "/data")
            loadedEvents <- getEvents src
            -- Store all loaded events like the node would do
            forM_ loadedEvents putEvent
            pure $
              loadedEvents === events

genContinuousEvents :: Gen [StateEvent SimpleTx]
genContinuousEvents =
  zipWith3 StateEvent [0 ..] <$> listOf arbitrary <*> listOf arbitrary

withEventSourceAndSink :: (EventSource (StateEvent SimpleTx) IO -> EventSink (StateEvent SimpleTx) IO -> IO b) -> IO b
withEventSourceAndSink action =
  withTempDir "hydra-persistence" $ \tmpDir -> do
    persistence <- createPersistenceIncremental (tmpDir <> "/data")
    (eventSource, eventSink) <- eventPairFromPersistenceIncremental persistence
    action eventSource eventSink
