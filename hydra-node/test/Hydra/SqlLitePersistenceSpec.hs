{-# LANGUAGE OverloadedStrings #-}

module Hydra.SqlLitePersistenceSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.SqlLitePersistence (
  Persistence (..),
  PersistenceIncremental (..),
  createPersistence,
  createPersistenceIncremental,
 )
import System.Directory (doesFileExist)

newtype TestData = TestData {value :: String}
  deriving newtype (Show, Eq)
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

spec :: Spec
spec = do
  describe "Persistence" $ do
    let testDbPath = "test.db"
    it "should return Nothing when no value is present" $ do
      Persistence{load} <- createPersistence testDbPath
      loadedValue :: Maybe TestData <- load
      loadedValue `shouldBe` Nothing
    it "should save and load data" $ do
      Persistence{load, save} <- createPersistence testDbPath
      let testValue = TestData "Hello, Hydra!"
      liftIO $ save testValue
      loadedValue <- load
      loadedValue `shouldBe` Just testValue
    it "should drop the database" $ do
      Persistence{dropDb} <- createPersistence testDbPath
      dropDb
      dbExists <- liftIO $ doesFileExist testDbPath
      dbExists `shouldBe` False
  describe "PersistenceIncremental" $ do
    let testDbPath = "test-inc.db"
    it "should return an empty list when no values are stored" $ do
      PersistenceIncremental{loadAll, countAll} <- createPersistenceIncremental testDbPath
      loadedValues :: [TestData] <- loadAll
      loadedValues `shouldBe` []
      count <- countAll
      count `shouldBe` 0
    it "should append and load multiple values" $ do
      PersistenceIncremental{appendMany, loadAll, countAll} <- createPersistenceIncremental testDbPath
      let testValues = [TestData "A", TestData "B", TestData "C"]
      liftIO $ appendMany testValues
      loadedValues <- loadAll
      loadedValues `shouldBe` testValues
      count <- countAll
      count `shouldBe` 3
    it "should drop the incremental database" $ do
      PersistenceIncremental{dropDb} <- createPersistenceIncremental testDbPath
      dropDb
      dbExists <- liftIO $ doesFileExist testDbPath
      dbExists `shouldBe` False
