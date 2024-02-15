{-# LANGUAGE LambdaCase #-}

module Hydra.PersistenceSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Hydra.Persistence (Persistence (..), PersistenceException (..), PersistenceIncremental (..), createPersistence, createPersistenceIncremental, eventPairFromPersistenceIncremental, getEvents', putEvent', putEventsToSinks)
import Test.QuickCheck (checkCoverage, cover, elements, oneof, suchThat, (===))
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
    it "re-delivers events on EventSource load, to all EventSinks" $
      checkCoverage $
        monadicIO $ do
          items <- pick $ listOf genPersistenceItem
          run $
            withTempDir "hydra-persistence" $ \tmpDir -> do
              -- FIXME(Elaine): swap for createEventPairIncremental only once nothing is using eventPairFromPersistenceIncremental
              -- initialize an event source
              persistEventSource <- createPersistenceIncremental $ tmpDir <> "/dataEventSource"
              let (eventSource, eventSink) = eventPairFromPersistenceIncremental persistEventSource
              putEventsToSinks (eventSink :| []) items

              -- initialize some event sinks
              persistSink1 <- createPersistenceIncremental $ tmpDir <> "/data1"
              persistSink2 <- createPersistenceIncremental $ tmpDir <> "/data2"
              let (sink1Source, sink1Sink) = eventPairFromPersistenceIncremental persistSink1
                  (sink2Source, sink2Sink) = eventPairFromPersistenceIncremental persistSink2
                  eventSinks = eventSink :| [sink1Sink, sink2Sink]

              -- load the event source, as if we had started a node
              -- TODO(Elaine): this on its own isn't enough to ensure persistence is working end to end, make sure to test that
              -- but it is an okay reference point
              -- maybe in node?
              -- test for loadStateEventSource
              getEvents' eventSource >>= putEventsToSinks eventSinks

              -- after loading our node, all sinks should recieved the same events
              getEvents' sink1Source `shouldReturn` items
              getEvents' sink2Source `shouldReturn` items
              -- including the event source itself, which will now have duplicated events, at least by current definition
              getEvents' eventSource `shouldReturn` (items <> items)
          pure ()

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
