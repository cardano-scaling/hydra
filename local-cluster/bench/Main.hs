module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Cardano (cardanoLedger, genSequenceOfValidTransactions, genUtxo)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, strOption, value)
import System.Environment (withArgs)
import System.FilePath ((</>))
import Test.QuickCheck (generate, scale)

data Options = Options
  { outputDirectory :: Maybe FilePath
  , scalingFactor :: Int
  }

benchOptionsParser :: Parser Options
benchOptionsParser =
  Options
    <$> optional
      ( strOption
          ( long "output-directory"
              <> help "Directory containing generated transactions and UTxO set to use for replaying a previous benchmark."
          )
      )
      <*> option
        auto
        ( long "scaling-factor"
            <> value 100
            <> metavar "INT"
            <> help "The scaling factor to apply to transactions generator (default: 100)"
        )

benchOptions :: ParserInfo Options
benchOptions =
  info
    (benchOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Starts a cluster of Hydra nodes interconnected through a network and \
          \ talking to mock-chain, generates an initial UTxO set and a bunch \
          \ of valid transactions, and send those transactions to the cluster as \
          \ fast as possible."
        <> header "bench - load tester for Hydra node cluster"
    )

main :: IO ()
main =
  execParser benchOptions >>= \case
    Options{outputDirectory = Just benchDir} -> do
      txs <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "txs.json")
      utxo <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "utxo.json")
      putStrLn $ "Using UTxO and Transactions from: " <> benchDir
      run benchDir utxo txs
    Options{scalingFactor} -> do
      tmpDir <- createSystemTempDirectory "bench"

      initialUtxo <- generate genUtxo
      txs <- generate $ scale (* scalingFactor) $ genSequenceOfValidTransactions initialUtxo
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
