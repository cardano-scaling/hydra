module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Cardano (cardanoLedger, genSequenceOfValidTransactions, genUtxo)
import System.Environment (withArgs)
import System.FilePath ((</>))
import Test.QuickCheck (generate, scale)

main :: IO ()
main =
  getArgs >>= \case
    [benchDir] -> do
      txs <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "txs.json")
      utxo <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "utxo.json")
      putStrLn $ "Using UTxO and Transactions from: " <> benchDir
      run benchDir utxo txs
    _ -> do
      tmpDir <- createSystemTempDirectory "bench"

      initialUtxo <- generate genUtxo
      txs <- generate $ scale (* 100) $ genSequenceOfValidTransactions initialUtxo
      -- Sanity check the generated txs
      case applyTransactions cardanoLedger initialUtxo txs of
        Left err -> die $ "Generated invalid transactions: " <> show err
        Right _ -> do
          saveTransactions tmpDir txs
          saveUtxos tmpDir initialUtxo
          run tmpDir initialUtxo txs
 where
  -- TODO(SN): Ideally we would like to say "to re-run use ... " on errors
  run fp utxo txs =
    withArgs [] . hspec $ bench fp utxo txs

  saveTransactions tmpDir txs = do
    let txsFile = tmpDir </> "txs.json"
    putStrLn $ "Writing transactions to: " <> txsFile
    encodeFile txsFile txs

  saveUtxos tmpDir utxos = do
    let utxosFile = tmpDir </> "utxo.json"
    putStrLn $ "Writing UTxO set to: " <> utxosFile
    encodeFile utxosFile utxos
