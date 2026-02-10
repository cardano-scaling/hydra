{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.GeneratorSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (
  Positive (Positive),
  Property,
  counterexample,
  forAll,
  idempotentIOProperty,
 )
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hspec-golden-aeson" Test.Aeson.GenericSpecs (roundtripSpecs)
import "hydra-cardano-api" Hydra.Cardano.Api (LedgerEra, UTxO, prettyPrintJSON, utxoFromTx)

import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (
  ClientDataset (..),
  Dataset (..),
  generateConstantUTxODataset,
 )
import "hydra-node" Hydra.Ledger (applyTransactions)
import "hydra-node" Test.Hydra.Node.Fixture (defaultGlobals, defaultPParams)
import "hydra-tx" Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import "hydra-tx" Hydra.Ledger.Cardano (Globals, LedgerEnv, Tx, cardanoLedger, newLedgerEnv)
import "text" Data.Text (unpack)

spec :: Spec
spec = parallel $ do
  roundtripSpecs (Proxy @Dataset)
  prop "generates a Dataset that keeps UTXO constant" prop_keepsUTxOConstant

prop_keepsUTxOConstant :: Property
prop_keepsUTxOConstant =
  forAll arbitrary $ \(Positive n) -> do
    idempotentIOProperty $ do
      faucetSk <- snd <$> keysFor Faucet

      let ledgerEnv = newLedgerEnv defaultPParams
      -- XXX: non-exhaustive pattern match
      pure $
        forAll (generateConstantUTxODataset faucetSk 1 n) $
          \Dataset{fundingTransaction, clientDatasets = [ClientDataset{txSequence}]} ->
            let initialUTxO = utxoFromTx fundingTransaction
                finalUTxO = foldl' (apply defaultGlobals ledgerEnv) initialUTxO txSequence
             in length (UTxO.txOutputs finalUTxO) == length (UTxO.txOutputs initialUTxO)
                  & counterexample ("transactions: " <> prettyJSONString txSequence)
                  & counterexample ("utxo: " <> prettyJSONString initialUTxO)
                  & counterexample ("funding tx: " <> prettyJSONString fundingTransaction)

apply :: Globals -> LedgerEnv LedgerEra -> UTxO -> Tx -> UTxO
apply globals ledgerEnv utxo tx =
  case applyTransactions (cardanoLedger globals ledgerEnv) (ChainSlot 0) utxo [tx] of
    Left err -> error $ "invalid generated data set" <> show err
    Right finalUTxO -> finalUTxO

prettyJSONString :: ToJSON a => a -> String
prettyJSONString = unpack . decodeUtf8 . prettyPrintJSON
