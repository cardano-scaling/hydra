{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hydra.PersistenceSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Test.QuickCheck (collect, counterexample, elements, oneof, (===))
import Test.QuickCheck.Gen (listOf)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

spec :: Spec
spec =
  describe "PersistenceIncremental" $
    it "is consistent after multiple append calls" $
      monadicIO $ do
        items :: [Aeson.Value] <- pick $ listOf genPersistenceItem
        monitor (collect items)
        monitor (counterexample $ "items: " <> (show items))
        actualResult <- run $
          withTempDir "hydra-persistence" $ \tmpDir -> do
            PersistenceIncremental{loadAll, append} <- createPersistenceIncremental Proxy $ tmpDir <> "/data"
            forM_ items append
            loadAll
        pure $ actualResult === items

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
