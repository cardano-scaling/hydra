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
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
 )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck.Gen (Gen (MkGen), chooseAny, generate)
import Test.QuickCheck.Random (mkQCGen)
import TxCost (
  computeAbortCost,
  computeCloseCost,
  computeCollectComCost,
  computeCommitCost,
  computeContestCost,
  computeDecrementCost,
  computeFanOutCost,
  computeInitCost,
 )

data Options = Options {outputDirectory :: Maybe FilePath, seed :: Maybe Int}

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
    <*> optional
      ( option
          auto
          ( long "seed"
              <> short 's'
              <> metavar "INT"
              <> help "A seed value"
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
    Options{outputDirectory = Nothing, seed = seed} -> writeTransactionCostMarkdown seed stdout
    Options{outputDirectory = Just outputDir, seed = seed} -> do
      unlessM (doesDirectoryExist outputDir) $ createDirectoryIfMissing True outputDir
      withFile (outputDir </> "transaction-cost.md") WriteMode (writeTransactionCostMarkdown seed)

writeTransactionCostMarkdown :: Maybe Int -> Handle -> IO ()
writeTransactionCostMarkdown mseed hdl = do
  seed <- case mseed of
    Nothing -> generate chooseAny
    Just s -> pure s
  let initC = costOfInit seed
  let commitC = costOfCommit seed
  let collectComC = costOfCollectCom seed
  let decrementC = costOfDecrement seed
  let closeC = costOfClose seed
  let contestC = costOfContest seed
  let abortC = costOfAbort seed
  let fanoutC = costOfFanOut seed
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
            , decrementC
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
  , "sidebar_label: 'Transaction costs' "
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
  , "| " <> "νDeposit" <> " | " <> serialiseToRawBytesHexText depositScriptHash <> " | " <> show depositScriptSize <> " | "
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
    , depositScriptHash
    , depositScriptSize
    } = scriptInfo

genFromSeed :: Gen a -> Int -> a
genFromSeed (MkGen g) seed = g (mkQCGen seed) 30

costOfInit :: Int -> Text
costOfInit = markdownInitCost . genFromSeed computeInitCost
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

costOfCommit :: Int -> Text
costOfCommit = markdownCommitCost . genFromSeed computeCommitCost
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

costOfCollectCom :: Int -> Text
costOfCollectCom = markdownCollectComCost . genFromSeed computeCollectComCost
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

costOfDecrement :: Int -> Text
costOfDecrement = markdownDecrementCost . genFromSeed computeDecrementCost
 where
  markdownDecrementCost stats =
    unlines $
      [ "## Cost of Decrement Transaction"
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

costOfClose :: Int -> Text
costOfClose = markdownClose . genFromSeed computeCloseCost
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

costOfContest :: Int -> Text
costOfContest = markdownContest . genFromSeed computeContestCost
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

costOfAbort :: Int -> Text
costOfAbort = markdownAbortCost . genFromSeed computeAbortCost
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

costOfFanOut :: Int -> Text
costOfFanOut = markdownFanOutCost . genFromSeed computeFanOutCost
 where
  markdownFanOutCost stats =
    unlines $
      [ "## `FanOut` transaction costs"
      , "Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability."
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
