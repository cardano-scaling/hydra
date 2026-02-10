-- | Tests for the 'EventSource' and 'EventSink' implementation in 'Hydra.Events.FileBased'.
module Hydra.Events.FileBasedSpec where

import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude

-- IsChainState tx instance to serialize 'StateEvent Tx'
import "hydra-node" Hydra.Chain.Direct.State ()

import "QuickCheck" Test.QuickCheck (forAllShrink, ioProperty, sublistOf, (===))
import "QuickCheck" Test.QuickCheck.Gen (listOf)
import "base" Data.List (zipWith3)
import "conduit" Conduit (runConduitRes, sinkList, (.|))
import "hspec-golden-aeson" Test.Aeson.GenericSpecs (
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
  roundtripAndGoldenSpecsWithSettings,
  sampleSize,
 )
import "hydra-node" Hydra.Events (EventSink (..), EventSource (..), getEvents, putEvent)
import "hydra-node" Hydra.Events.FileBased (mkFileBasedEventStore)
import "hydra-node" Hydra.Events.Rotation (EventStore (..))
import "hydra-node" Hydra.HeadLogic (StateChanged)
import "hydra-node" Hydra.HeadLogic.StateEvent (StateEvent (..))
import "hydra-node" Hydra.Ledger.Simple (SimpleTx)
import "hydra-node" Hydra.Logging (Verbosity (Verbose), withTracer)
import "hydra-node" Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import "hydra-node" Test.Hydra.Chain.Direct.State ()
import "hydra-node" Test.Hydra.HeadLogic.StateEvent ()
import "hydra-node" Test.Hydra.Ledger.Simple ()
import "hydra-tx" Hydra.Ledger.Cardano (Tx)

spec :: Spec
spec = do
  describe "persisted event format" $ do
    -- NOTE: Whenever one of these fails, make sure to record a **BREAKING** change of the persisted 'state'.
    roundtripAndGoldenSpecsWithSettings (defaultSettings{sampleSize = 1}) (Proxy @(MinimumSized (StateEvent Tx)))
    roundtripAndGoldenADTSpecsWithSettings (defaultSettings{sampleSize = 1}) (Proxy @(MinimumSized (StateChanged Tx)))

  describe "mkFileBasedEventStore" $ do
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
            withTracer (Verbose "hydra-persistence") $ \tracer -> do
              let stateDir = tmpDir <> "/data"
              PersistenceIncremental{append} <- createPersistenceIncremental tracer stateDir
              forM_ events append
              -- Load and store events through the event source interface
              EventStore{eventSource = src, eventSink = EventSink{putEvent}} <-
                mkFileBasedEventStore stateDir =<< createPersistenceIncremental tracer stateDir
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
    withTracer (Verbose "hydra-persistence") $ \tracer -> do
      let stateDir = tmpDir <> "/data"
      EventStore{eventSource, eventSink} <- mkFileBasedEventStore stateDir =<< createPersistenceIncremental tracer stateDir
      action eventSource eventSink
