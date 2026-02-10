{-# LANGUAGE LambdaCase #-}

module Hydra.PersistenceSpec where

import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (checkCoverage, cover, elements, oneof, suchThat, (===))
import "QuickCheck" Test.QuickCheck.Gen (listOf)
import "QuickCheck" Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)
import "aeson" Data.Aeson (Value (..))
import "aeson" Data.Aeson qualified as Aeson
import "text" Data.Text qualified as Text

import Hydra.Logging (Envelope (..), Verbosity (Verbose), withTracer)
import Hydra.Persistence (Persistence (..), PersistenceIncremental (..), createPersistence, createPersistenceIncremental, loadAll)
import Hydra.PersistenceLog
import Test.Util (captureTracer)

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
    it "can ignore invalid lines and emits warning" $ do
      withTempDir "hydra-persistence" $ \tmpDir -> do
        (tracer, getTraces) <- captureTracer "persistence-incremental"
        let fp = tmpDir <> "/data"
        writeFileBS fp "\"abc\"\n{\"xyz\": "
        p <- createPersistenceIncremental tracer fp
        loadAll p `shouldReturn` ([Aeson.String "abc"] :: [Aeson.Value])
        traces <- getTraces
        let rightMsg [Envelope{message = FailedToDecodeJson{}}] = True
            rightMsg _ = False
        traces `shouldSatisfy` rightMsg

    it "can handle empty files" $ do
      withTracer (Verbose "persistence-incremental") $ \tracer -> do
        withTempDir "hydra-persistence" $ \tmpDir -> do
          let fp = tmpDir <> "/data"
          writeFileBS fp ""
          p <- createPersistenceIncremental tracer fp
          loadAll p `shouldReturn` ([] :: [Aeson.Value])

    it "is consistent after multiple append calls in presence of new-lines" $
      checkCoverage $
        monadicIO $ do
          items <- pick $ listOf genPersistenceItem
          monitor (cover 1 (null items) "no items stored")
          monitor (cover 10 (containsNewLine items) "some item contains a new line")

          actualResult <- run $
            withTracer (Verbose "persistence-incremental") $ \tracer -> do
              withTempDir "hydra-persistence" $ \tmpDir -> do
                p <- createPersistenceIncremental tracer $ tmpDir <> "/data"
                forM_ items $ append p
                loadAll p
          pure $ actualResult === items

    it "it cannot load from a different thread once having started appending" $
      monadicIO $ do
        items <- pick $ listOf genPersistenceItem
        moreItems <- pick $ listOf genPersistenceItem `suchThat` ((> 2) . length)
        pure $
          withTracer (Verbose "persistence-incremental") $ \tracer -> do
            withTempDir "hydra-persistence" $ \tmpDir -> do
              p <- createPersistenceIncremental tracer $ tmpDir <> "/data"
              forM_ items $ append p
              loadAll p `shouldReturn` items
              raceLabelled_
                ("forever-load-all", forever $ threadDelay 0.01 >> loadAll p)
                ("append-more-items", forM_ moreItems $ \item -> append p item >> threadDelay 0.01)

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
