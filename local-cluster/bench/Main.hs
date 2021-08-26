module Main where

import Hydra.Prelude

import Bench.EndToEnd (bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Cardano (cardanoLedger, genSequenceOfValidTransactions, genUtxo)
import System.FilePath (takeDirectory, (</>))
import Test.Hydra.Prelude (createSystemTempDirectory)
import Test.QuickCheck (generate, scale)

main :: IO ()
main =
  getArgs >>= \case
    [utxosFile, txsFile] ->
      eitherDecodeFileStrict' txsFile
        >>= \ts ->
          eitherDecodeFileStrict' utxosFile >>= \us ->
            case (ts, us) of
              (Right txs, Right utxos) -> do
                putStrLn $ "Using transactions from: " <> txsFile
                putStrLn $ "Using UTxOs from: " <> utxosFile
                bench (takeDirectory txsFile) utxos txs
              err -> die (show err)
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
          bench tmpDir initialUtxo txs
 where
  saveTransactions tmpDir txs = do
    let txsFile = tmpDir </> "txs.json"
    putStrLn $ "Writing transactions to: " <> txsFile
    encodeFile txsFile txs

  saveUtxos tmpDir utxos = do
    let utxosFile = tmpDir </> "utxos.json"
    putStrLn $ "Writing UTxO set to: " <> utxosFile
    encodeFile utxosFile utxos
