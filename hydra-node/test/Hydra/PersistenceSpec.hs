{-# LANGUAGE LambdaCase #-}

module Hydra.PersistenceSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Hydra.Persistence (Persistence (..), PersistenceIncremental (..), createPersistence, createPersistenceIncremental, loadAll)
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
        p <- createPersistenceIncremental fp
        loadAll p `shouldReturn` ([] :: [Aeson.Value])

    it "is consistent after multiple append calls in presence of new-lines" $
      checkCoverage $
        monadicIO $ do
          items <- pick $ listOf genPersistenceItem
          monitor (cover 1 (null items) "no items stored")
          monitor (cover 10 (containsNewLine items) "some item contains a new line")

          actualResult <- run $
            withTempDir "hydra-persistence" $ \tmpDir -> do
              p <- createPersistenceIncremental $ tmpDir <> "/data"
              forM_ items $ append p
              loadAll p
          pure $ actualResult === items

    it "it cannot load from a different thread once having started appending" $
      monadicIO $ do
        items <- pick $ listOf genPersistenceItem
        moreItems <- pick $ listOf genPersistenceItem `suchThat` ((> 2) . length)
        pure $
          withTempDir "hydra-persistence" $ \tmpDir -> do
            p <- createPersistenceIncremental $ tmpDir <> "/data"
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
