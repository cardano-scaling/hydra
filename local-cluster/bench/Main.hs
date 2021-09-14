module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Generator (generateConstantUtxoDataset, generateDataset)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  flag,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  strOption,
  value,
 )
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath ((</>))
import Test.QuickCheck (generate, getSize, scale)

data Options = Options
  { outputDirectory :: Maybe FilePath
  , scalingFactor :: Int
  , concurrency :: Int
  , timeoutSeconds :: DiffTime
  , clusterSize :: Word64
  , constantUtxo :: GeneratorType
  }

data GeneratorType = LargeTxs | ConstantUtxo

benchOptionsParser :: Parser Options
benchOptionsParser =
  Options
    <$> optional
      ( strOption
          ( long "output-directory"
              <> help
                "Directory containing generated transactions and UTxO set. \
                \ * If the directory exists, it's assumed to be used for replaying \
                \   a previous benchmark and is expected to contain 'txs.json' and \
                \   'utxo.json' files, \
                \ * If the directory does not exist, it will be created and \
                \   populated with new transactions and UTxO set."
          )
      )
      <*> option
        auto
        ( long "scaling-factor"
            <> value 100
            <> metavar "INT"
            <> help "The scaling factor to apply to transactions generator (default: 100)"
        )
      <*> option
        auto
        ( long "concurrency"
            <> value 1
            <> metavar "INT"
            <> help
              "The concurrency level in the generated dataset. This number is used to \
              \ define how many independent UTXO set and transaction sequences will be \
              \ generated and concurrently submitted to the nodes (default: 1)"
        )
      <*> option
        auto
        ( long "timeout"
            <> value 600.0
            <> metavar "SECONDS"
            <> help
              "The timeout for the run, in seconds (default: '600s')"
        )
      <*> option
        auto
        ( long "cluster-size"
            <> value 3
            <> metavar "INT"
            <> help
              "The number of Hydra nodes to start and connect (default: 3)"
        )
      <*> flag
        LargeTxs
        ConstantUtxo
        ( long "constant-utxo"
            <> help
              "If set, generate transactions s.t. the size of UTXO set stays small and constant"
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
          \ fast as possible.\n \
          \ Arguments can control various parameters of the run, like number of ndoes, \
          \ number of clients or type of transactions generater..."
        <> header "bench - load tester for Hydra node cluster"
    )

main :: IO ()
main =
  execParser benchOptions >>= \case
    Options{outputDirectory = Just benchDir, scalingFactor, concurrency, timeoutSeconds, clusterSize, constantUtxo} -> do
      existsDir <- doesDirectoryExist benchDir
      if existsDir
        then replay timeoutSeconds clusterSize benchDir
        else createDirectory benchDir >> play scalingFactor concurrency timeoutSeconds clusterSize constantUtxo benchDir
    Options{scalingFactor, concurrency, timeoutSeconds, clusterSize, constantUtxo} ->
      createSystemTempDirectory "bench" >>= play scalingFactor concurrency timeoutSeconds clusterSize constantUtxo
 where
  play scalingFactor concurrency timeoutSeconds clusterSize constantUtxo benchDir = do
    let generator = case constantUtxo of
          LargeTxs -> generateDataset
          ConstantUtxo -> generateConstantUtxoDataset
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    dataset <- replicateM concurrency (generator numberOfTxs)
    saveDataset benchDir dataset
    run timeoutSeconds benchDir dataset clusterSize

  replay timeoutSeconds clusterSize benchDir = do
    datasets <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "dataset.json")
    putStrLn $ "Using UTxO and Transactions from: " <> benchDir
    run timeoutSeconds benchDir datasets clusterSize

  -- TODO(SN): Ideally we would like to say "to re-run use ... " on errors
  run timeoutSeconds benchDir datasets clusterSize =
    withArgs [] . hspec $ bench timeoutSeconds benchDir datasets clusterSize

  saveDataset tmpDir dataset = do
    let txsFile = tmpDir </> "dataset.json"
    putStrLn $ "Writing dataset to: " <> txsFile
    encodeFile txsFile dataset
