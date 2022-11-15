{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hydra.PersistenceSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Test.QuickCheck (elements, generate, oneof)
import Test.QuickCheck.Gen (listOf)

spec :: Spec
spec =
  describe "PersistenceIncremental" $
    it "is consistent after multiple append calls" $
      withTempDir "hydra-persistence" $ \tmpDir -> do
        items :: [Aeson.Value] <- generate $ listOf genPersistenceItem
        print items
        PersistenceIncremental{loadAll, append} <- createPersistenceIncremental Proxy $ tmpDir <> "/data"
        forM_ items append
        loadAll `shouldReturn` items

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
