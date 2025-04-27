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
      Persistence{load, closeDb} <- createPersistence testDbPath
      loadedValue :: Maybe TestData <- load
      closeDb
      loadedValue `shouldBe` Nothing
    it "should save and load data" $ do
      Persistence{load, save, closeDb} <- createPersistence testDbPath
      let testValue = TestData "Hello, Hydra!"
      liftIO $ save testValue
      loadedValue <- load
      closeDb
      loadedValue `shouldBe` Just testValue
    it "should drop the database" $ do
      Persistence{dropDb} <- createPersistence testDbPath
      dropDb
      dbExists <- liftIO $ doesFileExist testDbPath
      dbExists `shouldBe` False
  describe "PersistenceIncremental" $ do
    let testDbPath = "test-inc.db"
    it "should return an empty list when no values are stored" $ do
      PersistenceIncremental{loadAll, countAll, closeDb} <- createPersistenceIncremental testDbPath
      loadedValues :: [TestData] <- loadAll
      count <- countAll
      closeDb
      loadedValues `shouldBe` []
      count `shouldBe` 0
    it "should append and load multiple values" $ do
      PersistenceIncremental{appendMany, loadAll, countAll, closeDb} <- createPersistenceIncremental testDbPath
      let testValues = [TestData "A", TestData "B", TestData "C"]
      liftIO $ appendMany testValues
      loadedValues <- loadAll
      count <- countAll
      closeDb
      loadedValues `shouldBe` testValues
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
      PersistenceIncremental{append, appendMany, loadAll, closeDb} <- createRotatedEventLog testDbPath checkpointer
      mapM_ append $ TestData . show <$> [1 .. 5 :: Int]
      loadedValues1 <- loadAll
      appendMany $ TestData . show <$> [6 .. 9 :: Int]
      append $ TestData "10"
      loadedValues2 <- loadAll
      -- XXX: closes wal and shm files to not be found during `searchEventLogs`
      closeDb
      eventLogs <- searchEventLogs testDbPath
      pruneDb testDbPath
      loadedValues1 `shouldBe` [TestData "123", TestData "4", TestData "5"]
      loadedValues2 `shouldBe` [TestData "123456789", TestData "10"]
      sort eventLogs `shouldBe` sort ["test-inc.db", "test-inc-1.db", "test-inc-2.db"]

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
