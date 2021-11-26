{-# LANGUAGE TypeApplications #-}

module Test.GeneratorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (encode)
import Data.Text (unpack)
import Hydra.Generator (Dataset (..), genConstantUtxoDataset, genDataset)
import Hydra.Ledger (applyTransactions, balance)
import Hydra.Ledger.Cardano (CardanoTx, Utxo, cardanoLedger, genUtxo)
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.QuickCheck (Positive (Positive), Property, counterexample, forAll)

spec :: Spec
spec = parallel $ do
  roundtripSpecs (Proxy @Dataset)
  prop "compute values from UTXO set" prop_computeValueFromUtxo
  prop "generates a Dataset that keeps UTXO constant" prop_keepsUtxoConstant
  prop "generates a valid Dataset" prop_generateValidDataset

prop_computeValueFromUtxo :: Property
prop_computeValueFromUtxo =
  forAll genUtxo $ \utxo ->
    balance @CardanoTx utxo /= mempty

prop_keepsUtxoConstant :: Property
prop_keepsUtxoConstant =
  forAll arbitrary $ \(Positive n) ->
    forAll (genConstantUtxoDataset n) $ \Dataset{initialUtxo, transactionsSequence} ->
      let finalUtxo = foldl' apply initialUtxo transactionsSequence
       in length finalUtxo == length initialUtxo
            & counterexample ("\ntransactions: " <> jsonString transactionsSequence)
            & counterexample ("\nutxo: " <> jsonString initialUtxo)

prop_generateValidDataset :: Property
prop_generateValidDataset =
  forAll arbitrary $ \(Positive n) ->
    forAll (genDataset n) $ \Dataset{initialUtxo, transactionsSequence} ->
      let finalUtxo = foldl' apply initialUtxo transactionsSequence
       in not $ null finalUtxo

apply :: Utxo -> CardanoTx -> Utxo
apply utxo tx =
  case applyTransactions cardanoLedger utxo [tx] of
    Left err -> error $ "invalid generated data set" <> show err
    Right finalUtxo -> finalUtxo

jsonString :: ToJSON a => a -> String
jsonString = unpack . decodeUtf8 . encode
