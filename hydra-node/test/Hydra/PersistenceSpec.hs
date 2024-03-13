{-# LANGUAGE LambdaCase #-}

module Hydra.PersistenceSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..), getEvents, putEvent)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Persistence (Persistence (..), PersistenceException (..), PersistenceIncremental (..), createPersistence, createPersistenceIncremental, eventPairFromPersistenceIncremental)
import Test.QuickCheck (checkCoverage, cover, elements, forAllShrink, ioProperty, oneof, sublistOf, suchThat, (===))
import Test.QuickCheck.Gen (listOf)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

spec :: Spec
spec = do
  describe "Persistence" $ do
    it "can handle empty files" $ do
      withTempDir "hydra-persistence" $ \tmpDir -> do
        let fp = tmpDir <> "/data"
        writeFileBS fp ""
        Persistence{load} <- createPersistence fp
        load `shouldReturn` (Nothing :: Maybe Aeson.Value)

    it "is consistent after save/load roundtrip" $
      checkCoverage $
        monadicIO $ do
          item <- pick genPersistenceItem
          actualResult <- run $
            withTempDir "hydra-persistence" $ \tmpDir -> do
              Persistence{save, load} <- createPersistence $ tmpDir <> "/data"
              save item
              load
          pure $ actualResult === Just item

  describe "PersistenceIncremental" $ do
    it "can handle empty files" $ do
      withTempDir "hydra-persistence" $ \tmpDir -> do
        let fp = tmpDir <> "/data"
        writeFileBS fp ""
        PersistenceIncremental{loadAll} <- createPersistenceIncremental fp
        loadAll `shouldReturn` ([] :: [Aeson.Value])

    it "is consistent after multiple append calls in presence of new-lines" $
      checkCoverage $
        monadicIO $ do
          items <- pick $ listOf genPersistenceItem
          monitor (cover 1 (null items) "no items stored")
          monitor (cover 10 (containsNewLine items) "some item contains a new line")

          actualResult <- run $
            withTempDir "hydra-persistence" $ \tmpDir -> do
              PersistenceIncremental{loadAll, append} <- createPersistenceIncremental $ tmpDir <> "/data"
              forM_ items append
              loadAll
          pure $ actualResult === items

    it "it cannot load from a different thread once having started appending" $
      monadicIO $ do
        items <- pick $ listOf genPersistenceItem
        moreItems <- pick $ listOf genPersistenceItem `suchThat` ((> 2) . length)
        pure $
          withTempDir "hydra-persistence" $ \tmpDir -> do
            PersistenceIncremental{loadAll, append} <- createPersistenceIncremental $ tmpDir <> "/data"
            forM_ items append
            loadAll `shouldReturn` items
            race_
              (forever $ threadDelay 0.01 >> loadAll)
              (forM_ moreItems $ \item -> append item >> threadDelay 0.01)
              `shouldThrow` \case
                IncorrectAccessException{} -> True
                _ -> False

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

genPersistenceItem :: Gen Aeson.Value
genPersistenceItem =
  oneof
    [ pure Null
    , String <$> genSomeText
    ]

genSomeText :: Gen Text
genSomeText = do
  let t = ['A' .. 'z'] <> ['\n', '\t', '\r']
  Text.pack <$> listOf (elements t)

containsNewLine :: [Aeson.Value] -> Bool
containsNewLine = \case
  [] -> False
  (i : is) -> case i of
    String t | "\n" `Text.isInfixOf` t -> True
    _ -> containsNewLine is
