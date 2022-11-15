module Hydra.PersistenceSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.Aeson as Aeson
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Test.QuickCheck (generate, oneof)
import Test.QuickCheck.Gen (listOf)

spec :: Spec
spec =
  describe "PersistenceIncremental" $
    it "is consistent after multiple append calls" $
      withTempDir "hydra-persistence" $ \tmpDir -> do
        items :: [Aeson.Value] <- generate $ listOf genPersistenceItem
        PersistenceIncremental{loadAll, append} <- createPersistenceIncremental Proxy $ tmpDir <> "/data"
        forM_ items append
        loadAll `shouldReturn` items

genPersistenceItem :: Gen Aeson.Value
genPersistenceItem = oneof []
