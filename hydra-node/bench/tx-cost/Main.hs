import Hydra.Prelude hiding (catch)

import Data.Aeson (encode, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (hPut)
import Data.Fixed (Centi, Fixed (MkFixed))
import Data.Text qualified as Text
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
  seed <- resolveSeed mseed
  hPut hdl . encodeUtf8 . unlines $
    pageHeader <> intercalate [""] (markdownTable <$> costTables seed)

-- | Emit the same measurements as 'writeTransactionCostMarkdown', but as JSON
-- keyed by table and row so they can be compared numerically.
--
-- Each table carries the names of its index columns (which identify a row) and
-- of its value columns; 'tx-cost-diff' matches rows across two revisions by
-- index and subtracts the values. Emitting this directly is what lets the diff
-- avoid rendering to HTML and scraping the tables back out positionally.
writeTransactionCostJson :: Maybe Int -> Handle -> IO ()
writeTransactionCostJson mseed hdl = do
  seed <- resolveSeed mseed
  hPut hdl . toStrict . encode $
    Aeson.object
      [ "maxMemoryUnits" .= maxMem
      , "maxCpuUnits" .= maxCpu
      , "maxTxSizeBytes" .= maxTxSize
      , "tables" .= (jsonTable <$> costTables seed)
      ]

resolveSeed :: Maybe Int -> IO Int
resolveSeed = maybe (generate chooseAny) pure

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

-- | One measurement table of the report.
--
-- Both outputs are renderings of this: 'markdownTable' for the published page
-- and 'jsonTable' for the JSON that 'tx-cost-diff' consumes. They used to be
-- written out separately, and had already drifted apart (the last table's title
-- differed between them), so a table added or renamed on one side went missing
-- from the other with nothing to catch it.
data Table = Table
  { title :: Text
  , notes :: [Text]
  -- ^ Prose under the heading. Markdown only.
  , index :: [Text]
  -- ^ Columns identifying a row. 'tx-cost-diff' matches rows across revisions
  -- on these, so they hold whatever the measurement was parameterised by.
  , annotations :: [Text]
  -- ^ Columns shown between the index and the measured values that carry no
  -- numeric meaning (the script hashes). Left out of the JSON: there is nothing
  -- to subtract, and putting a hash in the key would leave every row unmatched
  -- the moment a script changes.
  , columns :: [Column]
  -- ^ Measured columns, subtracted across revisions by 'tx-cost-diff'.
  , rows :: [Row]
  , footnote :: Maybe Text
  }

-- | A measured column: its heading, and how the report renders its value. The
-- JSON always carries the full 'Double' so that 'tx-cost-diff' can see a change
-- too small to show up in the rounded table.
data Column = Column Text Precision

data Precision = AsInteger | AsFixed2

data Row = Row
  { key :: [Text]
  , annotation :: [Text]
  , values :: [Double]
  }

-- | All measurement tables, in report order.
costTables :: Int -> [Table]
costTables seed =
  [ Table
      { title = "Script summary"
      , notes = []
      , index = ["Name"]
      , annotations = ["Hash"]
      , columns = [Column "Size (Bytes)" AsInteger]
      , rows =
          [ Row{key = [name], annotation = [serialiseToRawBytesHexText scriptHash <> marker], values = [fromIntegral scriptSize]}
          | (name, scriptHash, marker, scriptSize) <-
              [ ("νHead" :: Text, headScriptHash, "" :: Text, headScriptSize)
              , ("μHead", mintingScriptHash, "*", mintingScriptSize)
              , ("νDeposit", depositScriptHash, "", depositScriptSize)
              , ("νCRS", crsScriptHash, "", crsScriptSize)
              ]
          ]
      , footnote = Just "* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head."
      }
  , parties "`Init` transaction costs" computeInitCost
  , parties "Cost of Increment Transaction" computeIncrementCost
  , parties "Cost of Decrement Transaction" computeDecrementCost
  , parties "`Close` transaction costs" computeCloseCost
  , parties "`Contest` transaction costs" computeContestCost
  , Table
      { title = "`FanOut` transaction costs"
      , notes =
          [ "Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability."
          , "Rows first grow the UTxO set at a fixed 10 parties, then show the largest set that still fits per number of parties (burning more participation tokens leaves less room for outputs)."
          ]
      , index = ["Parties", "UTxO"]
      , annotations = []
      , columns = valueColumns
      , rows =
          [ measured [show p, show n] utxoSize txSize mem cpu fee
          | (p, n, utxoSize, txSize, mem, cpu, fee) <- genFromSeed computeFanOutCost seed
          ]
      , footnote = Nothing
      }
  , distributed "`PartialFanOut` transaction costs" "ada-only" computePartialFanOutNominalCost
  , distributed "`PartialFanOut` transaction costs (with native tokens)" "native-token" computePartialFanOutMixedCost
  , Table
      { title = "`FinalPartialFanOut` transaction costs (with native tokens)"
      , notes =
          [ "Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. "
              <> "Burns all head tokens and proves accumulator exhaustion via BLS proof."
          ]
      , index = ["Distributed"]
      , annotations = []
      , columns = valueColumns
      , rows =
          [ measured [show n] utxoSize txSize mem cpu fee
          | (n, utxoSize, txSize, mem, cpu, fee) <- genFromSeed computeFinalPartialFanOutCost seed
          ]
      , footnote = Nothing
      }
  ]
 where
  valueColumns :: [Column]
  valueColumns = Column "UTxO (bytes)" AsInteger : partyColumns

  partyColumns :: [Column]
  partyColumns =
    [ Column "Tx size" AsInteger
    , Column "% max Mem" AsFixed2
    , Column "% max CPU" AsFixed2
    , Column "Min fee ₳" AsFixed2
    ]

  parties :: Text -> Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)] -> Table
  parties title compute =
    Table
      { title
      , notes = []
      , index = ["Parties"]
      , annotations = []
      , columns = partyColumns
      , rows =
          [ Row{key = [show p], annotation = [], values = [fromIntegral txSize, pct mem maxMem, pct cpu maxCpu, ada fee]}
          | (p, txSize, mem, cpu, Coin fee) <- genFromSeed compute seed
          ]
      , footnote = Nothing
      }

  distributed :: Text -> Text -> Gen [(NumUTxO, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)] -> Table
  distributed title kind compute =
    Table
      { title
      , notes =
          [ "Largest chunk of "
              <> kind
              <> " outputs that can be distributed in one partial fanout step, computed dynamically. "
              <> "The last row is the maximum total UTxO count where at least one output can still be distributed."
          ]
      , index = ["Total UTxO", "Distributed"]
      , annotations = []
      , columns = valueColumns
      , rows =
          [ measured [show numTotal, show (numTotal - numRemaining)] utxoSize txSize mem cpu fee
          | (numTotal, numRemaining, utxoSize, txSize, mem, cpu, fee) <- genFromSeed compute seed
          ]
      , footnote = Nothing
      }

  measured :: [Text] -> Natural -> TxSize -> MemUnit -> CpuUnit -> Coin -> Row
  measured key utxoSize txSize mem cpu (Coin fee) =
    Row
      { key
      , annotation = []
      , values = [fromIntegral utxoSize, fromIntegral txSize, pct mem maxMem, pct cpu maxCpu, ada fee]
      }

  pct :: (Real a, Real b) => a -> b -> Double
  pct x limit = realToFrac (x `percentOf` limit)

  ada :: Integer -> Double
  ada fee = realToFrac fee / 1_000_000

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

-- | Render a table as Markdown: heading, any prose, the header and alignment
-- rows, then one row per measurement.
--
-- NOTE: Index and annotation columns are left-aligned and measured columns
-- right-aligned, rather than per-table as the hand-written renderers did. That
-- had drifted too, leaving some byte counts left-aligned.
markdownTable :: Table -> [Text]
markdownTable Table{title, notes, index, annotations, columns, rows, footnote} =
  ["## " <> title]
    <> notes
    <> [ ""
       , cells $ index <> annotations <> [name | Column name _ <- columns]
       , cells $ (":---" <$ (index <> annotations)) <> ("---:" <$ columns)
       ]
    <> fmap markdownRow rows
    <> maybe [] (\note -> ["", note]) footnote
 where
  markdownRow :: Row -> Text
  markdownRow Row{key, annotation, values} =
    cells $ key <> annotation <> zipWith render columns values

  render :: Column -> Double -> Text
  render (Column _ precision) value = case precision of
    AsInteger -> show (round value :: Integer)
    -- NOTE: Build the 'Centi' from a rounded count of hundredths rather than
    -- via 'realToFrac', for two reasons.
    --
    -- The percentages are already 'Centi' (see 'percentOf') and only pass
    -- through 'Double' to reach the JSON. 3.09 has no exact 'Double', so
    -- 'realToFrac' would floor it straight back to 3.08; rounding recovers the
    -- stored value exactly.
    --
    -- The fee is a full-precision 'Double', and rounding it is deliberate. The
    -- hand-written renderers this replaced truncated (a side effect of
    -- 'Fixed''s 'fromRational'), which under-reports a *minimum* fee. Rounding
    -- also matches 'tx-cost-diff''s 'roundTo2', so the report and the diff
    -- agree on what a 0.01 step means.
    AsFixed2 -> show (MkFixed (round (value * 100)) :: Centi)

  cells :: [Text] -> Text
  cells xs = "| " <> Text.intercalate " | " xs <> " |"

-- | Render a table as the JSON 'tx-cost-diff' consumes.
jsonTable :: Table -> Aeson.Value
jsonTable Table{title, index, columns, rows} =
  Aeson.object
    [ "title" .= title
    , "index" .= index
    , "columns" .= [name | Column name _ <- columns]
    , "rows" .= fmap jsonRow rows
    ]
 where
  jsonRow Row{key, values} = Aeson.object ["key" .= key, "values" .= values]

genFromSeed :: Gen a -> Int -> a
genFromSeed (MkGen g) seed = g (mkQCGen seed) 30

percentOf :: (Real a, Real b) => a -> b -> Centi
part `percentOf` total =
  100 * realToFrac part / realToFrac total
