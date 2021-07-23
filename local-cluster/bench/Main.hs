module Main where

import Hydra.Prelude

import Bench.EndToEnd (bench)
import Data.Aeson (eitherDecodeFileStrict')
import Hydra.Ledger.Simple (genSequenceOfValidTransactions, utxoRefs)
import Test.QuickCheck (generate, scale)

main :: IO ()
main =
  getArgs >>= \case
    [txsFile] ->
      eitherDecodeFileStrict' txsFile >>= \case
        Left err -> die err
        Right txs -> bench txs
    _ -> do
      let initialUtxo = utxoRefs [1, 2, 3]
      txs <- generate $ scale (* 100) $ genSequenceOfValidTransactions initialUtxo
      bench txs
