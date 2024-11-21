module Hydra.SqlLitePersistenceSpec where

import Hydra.Prelude hiding (drop, label)
import Test.Hydra.Prelude

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Hydra.SqlLitePersistence (Persistence (..), PersistenceIncremental (..), createPersistence, createPersistenceIncremental)
import Test.QuickCheck (checkCoverage, cover, elements, oneof, (===))
import Test.QuickCheck.Gen (listOf)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

dbName :: String
dbName = "testdb"

setupPersistence :: (Persistence a IO -> IO ()) -> IO ()
setupPersistence action = do
  persistence@Persistence{dropDb} <- createPersistence dbName
  action persistence
  dropDb

setupPersistenceIncremental :: (PersistenceIncremental a IO -> IO ()) -> IO ()
setupPersistenceIncremental action = do
  persistence@PersistenceIncremental{dropDb} <- createPersistenceIncremental dbName
  action persistence
  dropDb

spec :: Spec
spec = do
  describe "SqlLitePersistence" $ do
    around setupPersistence $ do
      it "can handle empty reads" $ \Persistence{load} ->
        load `shouldReturn` (Nothing :: Maybe Aeson.Value)

      it "is consistent after save/load roundtrip" $ \Persistence{save, load} ->
        checkCoverage $
          monadicIO $ do
            item <- pick genPersistenceItem
            actualResult <- run $ do
              save item
              load
            pure $ actualResult === Just item

  describe "SqlLitePersistenceIncremental" $ do
    around setupPersistenceIncremental $ do
      it "can handle empty reads" $ \PersistenceIncremental{loadAll} ->
        loadAll `shouldReturn` ([] :: [Aeson.Value])

      it "is consistent after multiple append calls" $ \PersistenceIncremental{loadAll, append} ->
        checkCoverage $
          monadicIO $ do
            items <- pick $ listOf genPersistenceItem
            monitor (cover 1 (null items) "no items stored")
            actualResult <- run $ do
              forM_ items append
              loadAll
            pure $ all (`elem` actualResult) items

genPersistenceItem :: Gen Aeson.Value
genPersistenceItem =
  oneof
    [ pure Null
    , String <$> genSomeText
    ]

genSomeText :: Gen Text
genSomeText = do
  let t = ['A' .. 'z']
  Text.pack <$> listOf (elements t)
