module Main where

import Hydra.Prelude

import Bench.EndToEnd (bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Ledger.Simple (genSequenceOfValidTransactions, utxoRefs)
import Test.Hydra.Prelude (createSystemTempDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.QuickCheck (generate, scale)

main :: IO ()
main =
  getArgs >>= \case
    [txsFile] ->
      eitherDecodeFileStrict' txsFile >>= \case
        Left err -> die err
        Right txs -> do
          putStrLn $ "Using transactions from: " <> txsFile
          bench (takeDirectory txsFile) txs
    _ -> do
      tmpDir <- createSystemTempDirectory "bench"

      let initialUtxo = utxoRefs [1, 2, 3]
      txs <- generate $ scale (* 100) $ genSequenceOfValidTransactions initialUtxo

      let txsFile = tmpDir </> "txs.json"
      putStrLn $ "Writing transactions to: " <> txsFile
      encodeFile txsFile txs

      bench tmpDir txs
