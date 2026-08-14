import Hydra.Prelude hiding (catch)

import Data.Aeson (encode, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (hPut)
import Data.Fixed (Centi)
import Hydra.Cardano.Api (Coin (..), serialiseToRawBytesHexText)
import Hydra.Contract (HydraScriptCatalogue (..), hydraScriptCatalogue)
import Hydra.Plutus.Orphans ()
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
  short,
  strOption,
 )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hydra.Ledger.Cardano.Fixtures (maxCpu, maxMem, maxTxSize)
import Test.QuickCheck.Gen (Gen (MkGen), chooseAny, generate)
import Test.QuickCheck.Random (mkQCGen)
import TxCost (
  CpuUnit,
  MemUnit,
  NumParties,
  NumUTxO,
  TxSize,
  computeCloseCost,
  computeContestCost,
  computeDecrementCost,
  computeFanOutCost,
  computeFinalPartialFanOutCost,
  computeIncrementCost,
  computeInitCost,
  computePartialFanOutMixedCost,
  computePartialFanOutNominalCost,
 )

data Format = Markdown | Json
  deriving stock (Eq, Show)

data Options = Options {outputDirectory :: Maybe FilePath, seed :: Maybe Int, format :: Format}

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
    <*> flag
      Markdown
      Json
      ( long "json"
          <> help
            "Emit the same measurements as machine-readable JSON instead of \
            \ Markdown. Used by tx-cost-diff to compare two revisions."
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
    Options{outputDirectory = Nothing, seed, format} -> write format seed stdout
    Options{outputDirectory = Just outputDir, seed, format} -> do
      unlessM (doesDirectoryExist outputDir) $ createDirectoryIfMissing True outputDir
      withFile (outputDir </> fileName format) WriteMode (write format seed)
 where
  write = \case
    Markdown -> writeTransactionCostMarkdown
    Json -> writeTransactionCostJson

  fileName = \case
    Markdown -> "transaction-cost.md"
    Json -> "transaction-cost.json"

writeTransactionCostMarkdown :: Maybe Int -> Handle -> IO ()
writeTransactionCostMarkdown mseed hdl = do
  seed <- case mseed of
    Nothing -> generate chooseAny
    Just s -> pure s
  let initC = costOfInit seed
  let incrementC = costOfIncrement seed
  let decrementC = costOfDecrement seed
  let closeC = costOfClose seed
  let contestC = costOfContest seed
  let fanoutC = costOfFanOut seed
  let partialFanoutNominalC = costOfPartialFanOutNominal seed
  let partialFanoutMixedC = costOfPartialFanOutMixed seed
  let finalPartialFanoutC = costOfFinalPartialFanOut seed
  hPut hdl $
    encodeUtf8 $
      unlines $
        pageHeader
          <> scriptSizes
          <> intersperse
            ""
            [ initC
            , incrementC
            , decrementC
            , closeC
            , contestC
            , fanoutC
            , partialFanoutNominalC
            , partialFanoutMixedC
            , finalPartialFanoutC
            ]

-- | Emit the same measurements as 'writeTransactionCostMarkdown', but as JSON
-- keyed by table and row so they can be compared numerically.
--
-- Each table carries the names of its index columns (which identify a row) and
-- of its value columns; 'tx-cost-diff' matches rows across two revisions by
-- index and subtracts the values. Emitting this directly is what lets the diff
-- avoid rendering to HTML and scraping the tables back out positionally.
writeTransactionCostJson :: Maybe Int -> Handle -> IO ()
writeTransactionCostJson mseed hdl = do
  seed <- case mseed of
    Nothing -> generate chooseAny
    Just s -> pure s
  hPut hdl . toStrict . encode $
    Aeson.object
      [ "maxMemoryUnits" .= maxMem
      , "maxCpuUnits" .= maxCpu
      , "maxTxSizeBytes" .= maxTxSize
      , "tables" .= costTables seed
      ]

-- | All measurement tables, in the same order as the Markdown report.
costTables :: Int -> [Aeson.Value]
costTables seed =
  [ table "Script summary" ["Name"] ["Size (Bytes)"] $
      [ row [name] [fromIntegral scriptSize]
      | (name, scriptSize) <-
          [ ("νHead" :: Text, headScriptSize)
          , ("μHead", mintingScriptSize)
          , ("νDeposit", depositScriptSize)
          , ("νCRS", crsScriptSize)
          ]
      ]
  , parties "`Init` transaction costs" computeInitCost
  , parties "Cost of Increment Transaction" computeIncrementCost
  , parties "Cost of Decrement Transaction" computeDecrementCost
  , parties "`Close` transaction costs" computeCloseCost
  , parties "`Contest` transaction costs" computeContestCost
  , table "`FanOut` transaction costs" ["Parties", "UTxO"] valueColumns $
      [ row [show p, show n] [fromIntegral utxoSize, fromIntegral txSize, pct mem maxMem, pct cpu maxCpu, ada fee]
      | (p, n, utxoSize, txSize, mem, cpu, Coin fee) <- genFromSeed computeFanOutCost seed
      ]
  , distributed "`PartialFanOut` transaction costs" computePartialFanOutNominalCost
  , distributed "`PartialFanOut` transaction costs (with native tokens)" computePartialFanOutMixedCost
  , table "`FinalPartialFanOut` transaction costs" ["Distributed"] valueColumns $
      [ row [show n] [fromIntegral utxoSize, fromIntegral txSize, pct mem maxMem, pct cpu maxCpu, ada fee]
      | (n, utxoSize, txSize, mem, cpu, Coin fee) <- genFromSeed computeFinalPartialFanOutCost seed
      ]
  ]
 where
  valueColumns :: [Text]
  valueColumns = ["UTxO (bytes)", "Tx size", "% max Mem", "% max CPU", "Min fee ₳"]

  parties :: Text -> Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Aeson.Value
  parties title compute =
    table title ["Parties"] ["Tx size", "% max Mem", "% max CPU", "Min fee ₳"] $
      [ row [show p] [fromIntegral txSize, pct mem maxMem, pct cpu maxCpu, ada fee]
      | (p, txSize, mem, cpu, Coin fee) <- genFromSeed compute seed
      ]

  distributed :: Text -> Gen [(NumUTxO, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)] -> Aeson.Value
  distributed title compute =
    table title ["Total UTxO", "Distributed"] valueColumns $
      [ row
        [show numTotal, show (numTotal - numRemaining)]
        [fromIntegral utxoSize, fromIntegral txSize, pct mem maxMem, pct cpu maxCpu, ada fee]
      | (numTotal, numRemaining, utxoSize, txSize, mem, cpu, Coin fee) <- genFromSeed compute seed
      ]

  table :: Text -> [Text] -> [Text] -> [Aeson.Value] -> Aeson.Value
  table title index columns rows =
    Aeson.object
      [ "title" .= title
      , "index" .= index
      , "columns" .= columns
      , "rows" .= rows
      ]

  row :: [Text] -> [Double] -> Aeson.Value
  row key values = Aeson.object ["key" .= key, "values" .= values]

  pct :: (Real a, Real b) => a -> b -> Double
  pct x limit = realToFrac (x `percentOf` limit)

  ada :: Integer -> Double
  ada fee = realToFrac fee / 1_000_000

  HydraScriptCatalogue
    { mintingScriptSize
    , headScriptSize
    , depositScriptSize
    , crsScriptSize
    } = hydraScriptCatalogue

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
  , "| " <> "νHead" <> " | " <> serialiseToRawBytesHexText headScriptHash <> " | " <> show headScriptSize <> " | "
  , "| " <> "μHead" <> " | " <> serialiseToRawBytesHexText mintingScriptHash <> "* | " <> show mintingScriptSize <> " | "
  , "| " <> "νDeposit" <> " | " <> serialiseToRawBytesHexText depositScriptHash <> " | " <> show depositScriptSize <> " | "
  , "| " <> "νCRS" <> " | " <> serialiseToRawBytesHexText crsScriptHash <> " | " <> show crsScriptSize <> " | "
  , ""
  , "* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head."
  , ""
  ]
 where
  HydraScriptCatalogue
    { mintingScriptHash
    , mintingScriptSize
    , headScriptHash
    , headScriptSize
    , depositScriptHash
    , depositScriptSize
    , crsScriptHash
    , crsScriptSize
    } = hydraScriptCatalogue

genFromSeed :: Gen a -> Int -> a
genFromSeed (MkGen g) seed = g (mkQCGen seed) 30

costOfInit :: Int -> Text
costOfInit = markdownInitCost . genFromSeed computeInitCost
 where
  markdownInitCost :: [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Text
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

costOfIncrement :: Int -> Text
costOfIncrement = markdownIncrementCost . genFromSeed computeIncrementCost
 where
  markdownIncrementCost :: [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Text
  markdownIncrementCost stats =
    unlines $
      [ "## Cost of Increment Transaction"
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

costOfDecrement :: Int -> Text
costOfDecrement = markdownDecrementCost . genFromSeed computeDecrementCost
 where
  markdownDecrementCost :: [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Text
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
  markdownClose :: [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Text
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
  markdownContest :: [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Text
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

costOfFanOut :: Int -> Text
costOfFanOut = markdownFanOutCost . genFromSeed computeFanOutCost
 where
  markdownFanOutCost :: [(NumParties, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)] -> Text
  markdownFanOutCost stats =
    unlines $
      [ "## `FanOut` transaction costs"
      , "Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability."
      , "Rows first grow the UTxO set at a fixed 10 parties, then show the largest set that still fits per number of parties (burning more participation tokens leaves less room for outputs)."
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

costOfPartialFanOutNominal :: Int -> Text
costOfPartialFanOutNominal = markdownPartialFanOutNominalCost . genFromSeed computePartialFanOutNominalCost
 where
  markdownPartialFanOutNominalCost :: [(NumUTxO, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)] -> Text
  markdownPartialFanOutNominalCost stats =
    unlines $
      [ "## `PartialFanOut` transaction costs"
      , "Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. "
          <> "The last row is the maximum total UTxO count where at least one output can still be distributed."
      , ""
      , "| Total UTxO | Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| ---------: | ----------: | -----------: | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numTotal, numRemaining, utxoSize, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numTotal
                <> " | "
                <> show (numTotal - numRemaining)
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

costOfPartialFanOutMixed :: Int -> Text
costOfPartialFanOutMixed = markdownPartialFanOutMixedCost . genFromSeed computePartialFanOutMixedCost
 where
  markdownPartialFanOutMixedCost :: [(NumUTxO, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)] -> Text
  markdownPartialFanOutMixedCost stats =
    unlines $
      [ "## `PartialFanOut` transaction costs (with native tokens)"
      , "Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. "
          <> "The last row is the maximum total UTxO count where at least one output can still be distributed."
      , ""
      , "| Total UTxO | Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| ---------: | ----------: | -----------: | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numTotal, numRemaining, utxoSize, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numTotal
                <> " | "
                <> show (numTotal - numRemaining)
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

costOfFinalPartialFanOut :: Int -> Text
costOfFinalPartialFanOut = markdownFinalPartialFanOutCost . genFromSeed computeFinalPartialFanOutCost
 where
  markdownFinalPartialFanOutCost :: [(NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)] -> Text
  markdownFinalPartialFanOutCost stats =
    unlines $
      [ "## `FinalPartialFanOut` transaction costs (with native tokens)"
      , "Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. "
          <> "Burns all head tokens and proves accumulator exhaustion via BLS proof."
      , ""
      , "| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |"
      , "| ----------: | -----------: | ------: | --------: | --------: | --------: |"
      ]
        <> fmap
          ( \(numDistributed, utxoSize, txSize, mem, cpu, Coin minFee) ->
              "| "
                <> show numDistributed
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
