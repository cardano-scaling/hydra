{-# LANGUAGE TypeApplications #-}

module Test.GeneratorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (encode)
import Data.Text (unpack)
import Hydra.Cardano.Api (UTxO, utxoFromTx)
import Hydra.Generator (Dataset (..), defaultProtocolParameters, genConstantUTxODataset)
import Hydra.Ledger (applyTransactions, balance)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genUTxO)
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.QuickCheck (Positive (Positive), Property, counterexample, forAll)

spec :: Spec
spec = parallel $ do
  roundtripSpecs (Proxy @Dataset)
  prop "compute values from UTXO set" prop_computeValueFromUTxO
  prop "generates a Dataset that keeps UTXO constant" prop_keepsUTxOConstant

prop_computeValueFromUTxO :: Property
prop_computeValueFromUTxO =
  forAll genUTxO $ \utxo ->
    balance @Tx utxo /= mempty

prop_keepsUTxOConstant :: Property
prop_keepsUTxOConstant =
  forAll arbitrary $ \(Positive n) ->
    forAll (genConstantUTxODataset defaultProtocolParameters n) $ \Dataset{fundingTransaction, transactionsSequence} ->
      let initialUTxO = utxoFromTx fundingTransaction
          finalUTxO = foldl' apply initialUTxO transactionsSequence
       in length finalUTxO == length initialUTxO
            & counterexample ("\ntransactions: " <> jsonString transactionsSequence)
            & counterexample ("\nutxo: " <> jsonString initialUTxO)

apply :: UTxO -> Tx -> UTxO
apply utxo tx =
  case applyTransactions cardanoLedger utxo [tx] of
    Left err -> error $ "invalid generated data set" <> show err
    Right finalUTxO -> finalUTxO

jsonString :: ToJSON a => a -> String
jsonString = unpack . decodeUtf8 . encode
