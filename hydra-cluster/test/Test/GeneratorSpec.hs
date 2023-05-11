{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.GeneratorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (QueryPoint (QueryTip), queryGenesisParameters)
import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Data.Text (unpack)
import Hydra.Cardano.Api (LedgerEra, UTxO, prettyPrintJSON, utxoFromTx)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (
  ClientDataset (..),
  Dataset (..),
  defaultProtocolParameters,
  genDatasetConstantUTxO,
 )
import Hydra.Ledger (ChainSlot (ChainSlot), applyTransactions)
import Hydra.Ledger.Cardano (Tx, cardanoLedger)
import Hydra.Ledger.Cardano.Configuration (
  Globals,
  LedgerEnv,
  newGlobals,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
 )
import Hydra.Logging (showLogsOnFailure)
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.EndToEndSpec (withClusterTempDir)
import Test.QuickCheck (
  Positive (Positive),
  Property,
  counterexample,
  forAll,
  idempotentIOProperty,
  property,
  quickCheck,
 )

spec :: Spec
spec = parallel $ do
  roundtripSpecs (Proxy @Dataset)
  around showLogsOnFailure $
    it "generates a Dataset that keeps UTXO constant" $ \tracer -> do
      failAfter 60 $
        withClusterTempDir "queryGenesisParameters" $ \tmpDir -> do
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{nodeSocket, networkId} -> do
            globals <- newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip
            quickCheck $ property $ prop_keepsUTxOConstant globals

prop_keepsUTxOConstant :: Globals -> Property
prop_keepsUTxOConstant globals =
  forAll arbitrary $ \(Positive n) -> do
    idempotentIOProperty $ do
      faucetSk <- snd <$> keysFor Faucet
      ledgerEnv <-
        newLedgerEnv
          <$> readJsonFileThrow protocolParametersFromJson "config/protocol-parameters.json"
      -- XXX: non-exhaustive pattern match
      pure $
        forAll (genDatasetConstantUTxO faucetSk defaultProtocolParameters 1 n) $
          \Dataset{fundingTransaction, clientDatasets = [ClientDataset{txSequence}]} ->
            let initialUTxO = utxoFromTx fundingTransaction
                finalUTxO = foldl' (apply globals ledgerEnv) initialUTxO txSequence
             in length finalUTxO == length initialUTxO
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
