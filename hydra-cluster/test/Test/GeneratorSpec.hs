{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.GeneratorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (Actor (Faucet), keysFor)
import Data.Text (unpack)
import Hydra.Cardano.Api (UTxO, prettyPrintJSON, utxoFromTx)
import Hydra.Generator (
  ClientDataset (..),
  Dataset (..),
  defaultProtocolParameters,
  genDatasetConstantUTxO,
 )
import Hydra.Ledger (applyTransactions, balance)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genUTxO)
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.QuickCheck (Positive (Positive), Property, counterexample, forAll, idempotentIOProperty)

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
  forAll arbitrary $ \(Positive n) -> do
    idempotentIOProperty $ do
      faucetSk <- snd <$> keysFor Faucet
      -- XXX: non-exhaustive pattern match
      pure $
        forAll (genDatasetConstantUTxO faucetSk defaultProtocolParameters 1 n) $
          \Dataset{fundingTransaction, clientDatasets = [ClientDataset{txSequence}]} ->
            let initialUTxO = utxoFromTx fundingTransaction
                finalUTxO = foldl' apply initialUTxO txSequence
             in length finalUTxO == length initialUTxO
                  & counterexample ("transactions: " <> prettyJSONString txSequence)
                  & counterexample ("utxo: " <> prettyJSONString initialUTxO)
                  & counterexample ("funding tx: " <> prettyJSONString fundingTransaction)

apply :: UTxO -> Tx -> UTxO
apply utxo tx =
  case applyTransactions cardanoLedger utxo [tx] of
    Left err -> error $ "invalid generated data set" <> show err
    Right finalUTxO -> finalUTxO

prettyJSONString :: ToJSON a => a -> String
prettyJSONString = unpack . decodeUtf8 . prettyPrintJSON
