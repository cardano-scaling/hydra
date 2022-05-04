{-# LANGUAGE OverloadedStrings #-}

import Hydra.Prelude hiding (catch)

import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import Data.ByteString (hPut)
import Hydra.Ledger.Cardano.Evaluate (pparams)
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption,
 )
import Plutus.Orphans ()
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import TxCost (
  computeAbortCost,
  computeCloseCost,
  computeCollectComCost,
  computeCommitCost,
  computeFanOutCost,
  computeHashingCost,
  computeInitCost,
  computeMerkleTreeCost,
  maxCpu,
  maxMem,
 )

newtype Options = Options {outputDirectory :: Maybe FilePath}

txCostOptionsParser :: Parser Options
txCostOptionsParser =
  Options
    <$> optional
      ( strOption
          ( long "output-directory"
              <> short 'o'
              <> metavar "DIR"
              <> help
                "Directory where benchmark files should be output to. \
                \ If none is given, output is sent to stdout"
          )
      )

logFilterOptions :: ParserInfo Options
logFilterOptions =
  info
    (txCostOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Runs benchmarks assessing the execution cost of various on-chain \
          \ constructs: Some specific Plutus code, all OCV transactions,... \
          \ The output is valid markdown that can be used as is to be processed \
          \ and published."
        <> header "tx-cost - Hydra OCV Code Benchmarks"
    )

main :: IO ()
main =
  execParser logFilterOptions >>= \case
    Options{outputDirectory = Nothing} -> writeTransactionCostMarkdown stdout
    Options{outputDirectory = Just outputDir} -> do
      unlessM (doesDirectoryExist outputDir) $ createDirectoryIfMissing True outputDir
      withFile (outputDir </> "transaction-cost.md") WriteMode writeTransactionCostMarkdown

writeTransactionCostMarkdown :: Handle -> IO ()
writeTransactionCostMarkdown hdl = do
  initC <- costOfInit
  commitC <- costOfCommit
  collectComC <- costOfCollectCom
  closeC <- costOfClose
  abortC <- costOfAbort
  fanout <- costOfFanOut
  mt <- costOfMerkleTree
  let h = costOfHashing
  hPut hdl $
    encodeUtf8 $
      unlines $
        pageHeader
          <> intersperse
            ""
            [ initC
            , commitC
            , collectComC
            , closeC
            , abortC
            , fanout
            , mt
            , h
            ]

pageHeader :: [Text]
pageHeader =
  [ "--- "
  , "sidebar_label: 'Transactions Costs' "
  , "sidebar_position: 3 "
  , "--- "
  , ""
  , "# Transactions Costs "
  , ""
  , "Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs."
  , ""
  , "| Metadata   |"
  , "| ---    | ----    |"
  , "| _Generated at_ | " <> show now <> " |"
  , "| _Max. memory units_ | " <> show maxMem <> " |"
  , "| _Max. CPU units_ | " <> show maxCpu <> " |"
  , "| _Max. tx size (kB)_ | " <> show (Ledger._maxTxSize pparams) <> " |"
  , ""
  ]

{-# NOINLINE now #-}
now :: UTCTime
now = unsafePerformIO getCurrentTime

costOfInit :: IO Text
costOfInit = markdownInitCost <$> computeInitCost
 where
  markdownInitCost stats =
    unlines $
      [ "## Cost of Init Transaction"
      , ""
      , "| # Parties | Tx. size |"
      , "| :-------- | -------: |"
      ]
        <> fmap
          ( \(numParties, txSize) ->
              "| " <> show numParties
                <> "| "
                <> show txSize
                <> " | "
          )
          stats

costOfCommit :: IO Text
costOfCommit = markdownCommitCost <$> computeCommitCost
 where
  markdownCommitCost stats =
    unlines $
      [ "## Cost of Commit Transaction"
      , ""
      , "| # UTxO Committed | Assets size | Tx. size | % max Mem |   % max CPU |"
      , "| :--------------- | ----------: | -------: | --------: | ----------: |"
      ]
        <> map
          ( \(ulen, valueSz, txSize, mem, cpu) ->
              "| " <> show ulen
                <> "| "
                <> show valueSz
                <> "| "
                <> show txSize
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
          )
          stats

costOfCollectCom :: IO Text
costOfCollectCom = markdownCollectComCost <$> computeCollectComCost
 where
  markdownCollectComCost stats =
    unlines $
      [ "## Cost of CollectCom Transaction"
      , ""
      , "| # Parties | Tx. size | % max Mem |   % max CPU |"
      , "| :-------- | -------: | --------: | ----------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu) ->
              "| " <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
          )
          stats

costOfClose :: IO Text
costOfClose = markdownClose <$> computeCloseCost
 where
  markdownClose stats =
    unlines $
      [ "## Cost of Close Transaction"
      , ""
      , "| # Parties | Tx. size | % max Mem |   % max CPU |"
      , "| :-------- | -------: | --------: | ----------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu) ->
              "| " <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
          )
          stats

costOfAbort :: IO Text
costOfAbort = markdownAbortCost <$> computeAbortCost
 where
  markdownAbortCost stats =
    unlines $
      [ "## Cost of Abort Transaction"
      , ""
      , "| # Parties | Tx. size | % max Mem |   % max CPU |"
      , "| :-------- | -------: | --------: | ----------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu) ->
              "| " <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
          )
          stats

costOfFanOut :: IO Text
costOfFanOut = markdownFanOutCost <$> computeFanOutCost
 where
  markdownFanOutCost stats =
    unlines $
      [ "## Cost of FanOut Transaction (spend Head + burn HeadTokens)"
      , ""
      , "| UTXO  | Tx. size | % max Mem |   % max CPU |"
      , "| :---- | -------: | --------: | ----------: |"
      ]
        <> fmap
          ( \(numElems, txSize, mem, cpu) ->
              "| " <> show numElems
                <> "| "
                <> show txSize
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
          )
          stats

costOfMerkleTree :: IO Text
costOfMerkleTree = markdownMerkleTreeCost <$> computeMerkleTreeCost
 where
  markdownMerkleTreeCost stats =
    unlines $
      [ "## Cost of on-chain Merkle-Tree"
      , ""
      , "| Size | % member max mem | % member max cpu | % builder max mem | % builder max cpu |"
      , "| :--- | ---------------: | ---------------: | ----------------: | ----------------: |"
      ]
        <> fmap
          ( \(numElems, memberMem, memberCpu, builderMem, builderCpu) ->
              "| "
                <> show numElems
                <> " | "
                <> show (100 * fromIntegral (fromIntegral memberMem `div` numElems) / maxMem)
                <> " | "
                <> show (100 * fromIntegral (fromIntegral memberCpu `div` numElems) / maxCpu)
                <> " | "
                <> show (100 * fromIntegral builderMem / maxMem)
                <> " | "
                <> show (100 * fromIntegral builderCpu / maxCpu)
                <> " |"
          )
          stats

costOfHashing :: Text
costOfHashing = markdownHashingCost computeHashingCost
 where
  markdownHashingCost stats =
    unlines $
      [ "##  Cost of on-chain Hashing"
      , ""
      ]
        <> concatMap
          ( \(n, s, costs) ->
              [ "###  n = " <> show n <> ", s = " <> show s
              , ""
              , "| Algorithm | Cpu  | Mem  | Δcpu | Δmem |"
              , "| :-------- | ---: | ---: | ---: | ---: |"
              ]
                <> fmap
                  ( \case
                      Right (algorithm, baseCpu, baseMem, cpu, mem) ->
                        "| " <> show algorithm
                          <> " | "
                          <> show baseCpu
                          <> " | "
                          <> show baseMem
                          <> " | "
                          <> show (toInteger cpu - toInteger baseCpu)
                          <> " | "
                          <> show (toInteger mem - toInteger baseMem)
                          <> " |"
                      Left _ -> "| - | - |"
                  )
                  costs
          )
          stats
