module Hydra.PersistenceSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.Aeson as Aeson
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Test.QuickCheck (generate)
import Test.QuickCheck.Gen (listOf)

spec :: Spec
spec =
  describe "PersistenceIncremental" $
    it "is consistent after multiple append calls" $
      withTempDir "hydra-persistence" $ \tmpDir -> do
        items :: [Aeson.Value] <- generate $ listOf genPersistenceItem
        PersistenceIncremental{} <- createPersistenceIncremental Proxy $ tmpDir <> "/data"
        pure ()

genPersistenceItem :: Gen Aeson.Value
genPersistenceItem = undefined
