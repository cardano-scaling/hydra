import Hydra.Prelude hiding (catch)

import Data.ByteString (hPut)
import Data.Fixed (Centi)
import Hydra.Cardano.Api (Coin (..), serialiseToRawBytesHexText)
import Hydra.Contract (ScriptInfo (..), scriptInfo)
import Hydra.Ledger.Cardano.Evaluate (maxCpu, maxMem, maxTxSize)
import Hydra.Plutus.Orphans ()
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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import TxCost (
  computeAbortCost,
  computeCloseCost,
  computeCollectComCost,
  computeCommitCost,
  computeContestCost,
  computeFanOutCost,
  computeInitCost,
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
          \ The output is valid Markdown that can be used as is to be processed \
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
  contestC <- costOfContest
  abortC <- costOfAbort
  fanoutC <- costOfFanOut
  hPut hdl $
    encodeUtf8 $
      unlines $
        pageHeader
          <> scriptSizes
          <> intersperse
            ""
            [ initC
            , commitC
            , collectComC
            , closeC
            , contestC
            , abortC
            , fanoutC
            ]

-- NOTE: GitHub actions CI depends on the number of header lines, see
-- .github/workflows/ci-nix.yaml
pageHeader :: [Text]
pageHeader =
  [ "--- "
  , "sidebar_label: 'Transactions Costs' "
  , "sidebar_position: 3 "
  , "--- "
  , ""
  , "# Transaction costs "
  , ""
  , "Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs."
  , ""
  , "| Metadata | |"
  , "| :--- | :--- |"
  , "| _Generated at_ | " <> show now <> " |"
  , "| _Max. memory units_ | " <> show maxMem <> " |"
  , "| _Max. CPU units_ | " <> show maxCpu <> " |"
  , "| _Max. tx size (kB)_ | " <> show maxTxSize <> " |"
  , ""
  ]

{-# NOINLINE now #-}
now :: UTCTime
now = unsafePerformIO getCurrentTime

scriptSizes :: [Text]
scriptSizes =
  [ "## Script summary"
  , ""
  , "| Name   | Hash | Size (Bytes) "
  , "| :----- | :--- | -----------: "
  , "| " <> "νInitial" <> " | " <> serialiseToRawBytesHexText initialScriptHash <> " | " <> show initialScriptSize <> " | "
  , "| " <> "νCommit" <> " | " <> serialiseToRawBytesHexText commitScriptHash <> " | " <> show commitScriptSize <> " | "
  , "| " <> "νHead" <> " | " <> serialiseToRawBytesHexText headScriptHash <> " | " <> show headScriptSize <> " | "
  , "| " <> "μHead" <> " | " <> serialiseToRawBytesHexText mintingScriptHash <> "* | " <> show mintingScriptSize <> " | "
  , ""
  , "* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head."
  ]
 where
  ScriptInfo
    { mintingScriptHash
    , mintingScriptSize
    , initialScriptHash
    , initialScriptSize
    , commitScriptHash
    , commitScriptSize
    , headScriptHash
    , headScriptSize
    } = scriptInfo

costOfInit :: IO Text
costOfInit = markdownInitCost <$> computeInitCost
 where
  markdownInitCost stats =
    unlines $
      [ "## `Init` transaction costs"
      , ""
      , "| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :------ | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

costOfCommit :: IO Text
costOfCommit = markdownCommitCost <$> computeCommitCost
 where
  markdownCommitCost stats =
    unlines $
      [ "## `Commit` transaction costs"
      , " This uses ada-only outputs for better comparability."
      , ""
      , "| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :--- | ------: | --------: | --------: | --------: |"
      ]
        <> map
          ( \(ulen, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show ulen
                <> "| "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

costOfCollectCom :: IO Text
costOfCollectCom = markdownCollectComCost <$> computeCollectComCost
 where
  markdownCollectComCost stats =
    unlines $
      [ "## `CollectCom` transaction costs"
      , ""
      , "| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :------ | :----------- |------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numParties, utxoSize, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numParties
                <> " | "
                <> show utxoSize
                <> " | "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

costOfClose :: IO Text
costOfClose = markdownClose <$> computeCloseCost
 where
  markdownClose stats =
    unlines $
      [ "## `Close` transaction costs"
      , ""
      , "| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :------ | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

costOfContest :: IO Text
costOfContest = markdownContest <$> computeContestCost
 where
  markdownContest stats =
    unlines $
      [ "## `Contest` transaction costs"
      , ""
      , "| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :------ | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

costOfAbort :: IO Text
costOfAbort = markdownAbortCost <$> computeAbortCost
 where
  markdownAbortCost stats =
    unlines $
      [ "## `Abort` transaction costs"
      , "There is some variation due to the random mixture of initial and already committed outputs."
      , ""
      , "| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :------ | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

costOfFanOut :: IO Text
costOfFanOut = markdownFanOutCost <$> computeFanOutCost
 where
  markdownFanOutCost stats =
    unlines $
      [ "## `FanOut` transaction costs"
      , "Involves spending head output and burning head tokens. Uses ada-only UTxO for better comparability."
      , ""
      , "| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(parties, numElems, utxoSize, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show parties
                <> " | "
                <> show numElems
                <> " | "
                <> show utxoSize
                <> " | "
                <> show txSize
                <> " | "
                <> show (mem `percentOf` maxMem)
                <> " | "
                <> show (cpu `percentOf` maxCpu)
                <> " | "
                <> show (realToFrac minFee / 1_000_000 :: Centi)
                <> " |"
          )
          stats

percentOf :: (Real a, Real b) => a -> b -> Centi
part `percentOf` total =
  100 * realToFrac part / realToFrac total
