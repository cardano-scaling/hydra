module Hydra.SqlLitePersistenceSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.SqlLitePersistence (
  Checkpointer (..),
  Persistence (..),
  PersistenceIncremental (..),
  createPersistence,
  createPersistenceIncremental,
  createRotatedEventLog,
 )
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeDirectory, (</>))

newtype TestData = TestData {value :: Text}
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
    it "should rotate event log" $ do
      let checkpointer =
            Checkpointer
              { countRate = 2
              , fileCondition = const $ pure True
              , checkpoint = pure . TestData . foldMap value
              }
      PersistenceIncremental{append, loadAll, closeDb} <- createRotatedEventLog testDbPath checkpointer
      let totalElements :: Int = 10
      let testValues = TestData . show <$> [1 .. totalElements]
      forM_ testValues append
      loadedValues <- loadAll
      loadedValues `shouldBe` [TestData "123456789", TestData "10"]
      closeDb
      eventLogs <- searchEventLogs testDbPath
      pruneDb testDbPath
      length eventLogs `shouldBe` 4

searchEventLogs :: FilePath -> IO [FilePath]
searchEventLogs fp = do
  files <- listDirectory dir
  let matchingFiles = filter (baseName `isPrefixOf`) files
  pure matchingFiles
 where
  dir = takeDirectory fp
  baseName = takeBaseName fp

pruneDb :: FilePath -> IO ()
pruneDb fp = do
  files <- listDirectory dir
  let matchingFiles = filter (baseName `isPrefixOf`) files
  mapM_ (\f -> removeFile (dir </> f)) matchingFiles
 where
  dir = takeDirectory fp
  baseName = takeBaseName fp
