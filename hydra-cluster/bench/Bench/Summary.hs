{-# LANGUAGE DuplicateRecordFields #-}

module Bench.Summary where

import Hydra.Prelude

import Data.Fixed (Nano)
import Data.List qualified as List
import Data.Text (pack)
import Data.Text qualified as T
import Data.Time (nominalDiffTimeToSeconds)
import Data.Vector (Vector, (!))
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Statistics.Quantile (def)
import Statistics.Quantile qualified as Statistics
import Test.HUnit.Lang (FailureReason, formatFailureReason)
import Test.Hydra.Prelude (HUnitFailure (..))
import Text.Printf (printf)

-- | System stats like memory consumption.
type SystemStats = [Text]

-- | Per hydra-node GHC RTS deltas over the tx-processing window, scraped from
-- the monitoring endpoint. Only available when nodes run with '+RTS -T'.
data NodeRtsStats = NodeRtsStats
  { allocatedBytes :: Double
  , mutatorCpuSeconds :: Double
  , gcCpuSeconds :: Double
  , maxLiveBytes :: Double
  -- ^ Peak live heap since process start, not a windowed delta.
  , majorGcs :: Double
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

data Summary = Summary
  { clusterSize :: Word64
  , totalTxs :: Int
  , numberOfTxs :: Int
  , numberOfInvalidTxs :: Int
  , averageConfirmationTime :: NominalDiffTime
  , summaryTitle :: Text
  , summaryDescription :: Text
  , quantiles :: Vector Double
  , numberOfFanoutOutputs :: Int
  , endToEndTps :: Double
  , runWallClockSeconds :: Double
  , sustainedTps :: Maybe Double
  -- ^ Confirmation rate over the middle ~80% of the run, trimmed on snapshot
  -- boundaries (see 'Bench.EndToEnd.sustainedSnapshotTps'). Nothing when too
  -- few snapshots were observed for the rate to be meaningful.
  , drainSeconds :: Double
  -- ^ Time from the last submission to the last confirmation: how long the
  -- head needed to work through the submitted backlog.
  , avgTxsPerSnapshot :: Double
  , validationP50Ms :: Maybe Double
  -- ^ Median time from submission to the TxValid server output.
  , peakNodeRssMb :: Maybe Double
  -- ^ Peak resident set size (VmHWM) across the scenario's hydra-node
  -- processes.
  , numberOfSnapshots :: Int
  , incrementalCommitTimes :: [NominalDiffTime]
  , incrementalDecommitTimes :: [NominalDiffTime]
  , runOutcome :: Maybe Text
  -- ^ Nothing when the run completed; a short failure reason otherwise.
  , loadMode :: Text
  -- ^ "open-loop" (fire and forget) or "closed-loop" (one in-flight tx per
  -- client).
  , snapshotSeries :: [(Double, Int)]
  -- ^ Per confirmed snapshot: (seconds since first submission, txs in the
  -- snapshot), in observation order. Raw series so derived estimators can be
  -- computed outside the compared binaries (see scripts/bench-e2e-diff.py).
  , confirmationTimesMs :: [Double]
  -- ^ Sorted per-transaction confirmation times in milliseconds.
  , nodeRtsStats :: [NodeRtsStats]
  -- ^ One entry per node; empty unless nodes ran with '+RTS -T'.
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

errorSummary :: Dataset -> HUnitFailure -> Summary
errorSummary Dataset{title, clientDatasets} (HUnitFailure sourceLocation reason) =
  Summary
    { clusterSize = fromIntegral $ length clientDatasets
    , totalTxs = length $ foldMap (\ClientDataset{txSequence} -> txSequence) clientDatasets
    , numberOfTxs = 0
    , numberOfInvalidTxs = 0
    , averageConfirmationTime = 0
    , summaryTitle = maybe "Failed scenario" (<> " (failed)") title
    , summaryDescription =
        "Benchmark failed " <> pack (formatLocation sourceLocation) <> ": " <> shortReason reason
    , quantiles = mempty
    , numberOfFanoutOutputs = 0
    , endToEndTps = 0
    , runWallClockSeconds = 0
    , sustainedTps = Nothing
    , drainSeconds = 0
    , avgTxsPerSnapshot = 0
    , validationP50Ms = Nothing
    , peakNodeRssMb = Nothing
    , numberOfSnapshots = 0
    , incrementalCommitTimes = []
    , incrementalDecommitTimes = []
    , runOutcome = Just $ shortReason reason
    , loadMode = "unknown"
    , snapshotSeries = []
    , confirmationTimesMs = []
    , nodeRtsStats = []
    }
 where
  formatLocation = maybe "" (\loc -> "at " <> prettySrcLoc loc)

  -- Take only the first line of the reason. waitMatch failures dump every
  -- "seen message" verbatim, which is hundreds of lines of JSON that mangle
  -- the markdown when this summary lands in a PR comment.
  shortReason :: FailureReason -> Text
  shortReason r =
    let full = pack (formatFailureReason r)
     in case T.lines full of
          [] -> "(no reason)"
          (l : rest) -> l <> if null rest then "" else " (full output omitted)"

makeQuantiles :: [NominalDiffTime] -> Vector Double
-- quantilesVec throws on empty input and report writers force this; renderers
-- already guard on the vector's length.
makeQuantiles [] = mempty
makeQuantiles times =
  Statistics.quantilesVec def (fromList [0 .. 99]) 100 (fromList $ map (fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds) times)

-- | Render a time value with at most one decimal place.
oneDec :: Real a => a -> Text
oneDec x = pack $ printf "%.1f" (realToFrac x :: Double)

-- | Confirmed snapshots per second over the run's wall clock. This is the
-- headline rate for snapshot-throughput comparisons: unlike TPS it does not
-- conflate the number of transactions batched into each snapshot.
snapshotsPerSecond :: Summary -> Double
snapshotsPerSecond Summary{numberOfSnapshots, runWallClockSeconds}
  | runWallClockSeconds > 0 = fromIntegral numberOfSnapshots / runWallClockSeconds
  | otherwise = 0

-- | Aggregated RTS work counters across nodes, normalized by confirmed txs
-- and snapshots: (alloc MB per tx, alloc MB per snapshot, mutator CPU s per
-- 1k txs, max live MB of the largest node). Mirrored by rts_metrics in
-- scripts/bench-e2e-diff.py; keep the two in sync.
rtsAggregates :: Summary -> Maybe (Double, Double, Double, Double)
rtsAggregates Summary{nodeRtsStats, numberOfTxs, numberOfSnapshots} = do
  guard (not (null nodeRtsStats) && numberOfTxs > 0 && numberOfSnapshots > 0)
  let mb = 1024 * 1024
      totalAllocMb = sum (map allocatedBytes nodeRtsStats) / mb
      totalMutCpu = sum (map mutatorCpuSeconds nodeRtsStats)
  pure
    ( totalAllocMb / fromIntegral numberOfTxs
    , totalAllocMb / fromIntegral numberOfSnapshots
    , totalMutCpu / (fromIntegral numberOfTxs / 1000)
    , List.maximum (map maxLiveBytes nodeRtsStats) / mb
    )

textReport :: (Summary, SystemStats) -> [Text]
textReport (summary@Summary{totalTxs, numberOfTxs, averageConfirmationTime, quantiles, validationP50Ms, numberOfInvalidTxs, numberOfFanoutOutputs, endToEndTps, sustainedTps, drainSeconds, avgTxsPerSnapshot, peakNodeRssMb, numberOfSnapshots, incrementalCommitTimes, incrementalDecommitTimes, runOutcome}, systemStats) =
  let frac :: Double
      frac = 100 * fromIntegral numberOfTxs / fromIntegral totalTxs
   in [ pack $ printf "Confirmed txs/Total expected txs: %d/%d (%.2f %%)" numberOfTxs totalTxs frac
      , "Average confirmation time (ms): " <> oneDec (nominalDiffTimeToMilliseconds averageConfirmationTime)
      ]
        ++ maybe [] (\reason -> ["Outcome: FAILED: " <> reason]) runOutcome
        ++ ( if length quantiles == 100
              then
                [ "P99: " <> oneDec (quantiles ! 99) <> "ms"
                , "P95: " <> oneDec (quantiles ! 95) <> "ms"
                , "P50: " <> oneDec (quantiles ! 50) <> "ms"
                ]
              else []
           )
        ++ maybe [] (\v -> [pack $ printf "Tx validation time p50 (ms): %.1f" v]) validationP50Ms
        ++ [pack $ printf "End-to-end TPS: %.2f tx/s" endToEndTps]
        ++ maybe [] (\tps -> [pack $ printf "Sustained TPS: %.2f tx/s" tps]) sustainedTps
        ++ [pack $ printf "Backlog drain time (s): %.1f" drainSeconds]
        ++ [pack $ printf "Snapshots observed: %d" numberOfSnapshots]
        ++ [pack $ printf "Snapshots per second: %.2f /s" (snapshotsPerSecond summary)]
        ++ [pack $ printf "Avg txs per snapshot: %.1f" avgTxsPerSnapshot]
        ++ maybe [] (\mb -> [pack $ printf "Peak node RSS (MB): %.1f" mb]) peakNodeRssMb
        ++ maybe
          []
          ( \(allocTx, allocSnap, cpu1k, live) ->
              [ pack $ printf "Alloc MB per confirmed tx: %.3f" allocTx
              , pack $ printf "Alloc MB per snapshot: %.1f" allocSnap
              , pack $ printf "Mutator CPU s per 1k txs: %.3f" cpu1k
              , pack $ printf "Max live MB (max node): %.1f" live
              ]
          )
          (rtsAggregates summary)
        ++ ["Invalid txs: " <> show numberOfInvalidTxs]
        ++ ["Fanout outputs: " <> show numberOfFanoutOutputs]
        ++ incrementalLines "Incremental commit" incrementalCommitTimes
        ++ incrementalLines "Incremental decommit" incrementalDecommitTimes
        ++ if null systemStats then [] else "\n### Memory data \n" : [unlines systemStats]
 where
  incrementalLines :: Text -> [NominalDiffTime] -> [Text]
  incrementalLines lbl = \case
    [] -> []
    ts ->
      let xs = map nominalDiffTimeToMilliseconds ts
          avg = sum xs / fromIntegral (length xs)
       in [ lbl <> " count: " <> show (length ts)
          , lbl <> " avg (ms): " <> oneDec avg
          , lbl <> " max (ms): " <> oneDec (List.maximum xs)
          ]

markdownReport :: UTCTime -> [(Summary, SystemStats)] -> [Text]
markdownReport now summaries =
  pageHeader <> concatMap formattedSummary summaries
 where
  pageHeader :: [Text]
  pageHeader =
    [ "--- "
    , "sidebar_label: 'End-to-end benchmarks' "
    , "sidebar_position: 4 "
    , "--- "
    , ""
    , "# End-to-end benchmark results "
    , ""
    , "This page is intended to collect the latest end-to-end benchmark \
      \ results produced by Hydra's continuous integration (CI) system from \
      \ the latest `master` code."
    , ""
    , ":::caution"
    , ""
    , "Please note that these results are approximate \
      \ as they are currently produced from limited cloud VMs and not controlled hardware. \
      \ Rather than focusing on the absolute results, \
      \  the emphasis should be on relative results, \
      \ such as how the timings for a scenario evolve as the code changes."
    , ""
    , ":::"
    , ""
    , "_Generated at_  " <> show now
    , ""
    ]

formattedSummary :: (Summary, SystemStats) -> [Text]
formattedSummary (summary@Summary{clusterSize, numberOfTxs, averageConfirmationTime, quantiles, validationP50Ms, summaryTitle, summaryDescription, numberOfInvalidTxs, numberOfFanoutOutputs, endToEndTps, sustainedTps, drainSeconds, avgTxsPerSnapshot, peakNodeRssMb, numberOfSnapshots, incrementalCommitTimes, incrementalDecommitTimes, runOutcome, loadMode}, systemStats)
  | numberOfTxs == 0 =
      -- Failed cell: no confirmations, so all the latency / TPS rows would be
      -- zeros or empty quantiles. Render a short failure block instead of the
      -- full table to keep the matrix report readable.
      [ ""
      , "## " <> summaryTitle
      , ""
      , summaryDescription
      , ""
      , "| Number of nodes | " <> show clusterSize <> " |"
      , "| -- | -- |"
      , "| _Outcome_ | did not complete, no measurements |"
      , "      "
      ]
  | otherwise =
      [ ""
      , "## " <> summaryTitle
      , ""
      , summaryDescription
      , ""
      , "| Number of nodes |  " <> show clusterSize <> " | "
      , "| -- | -- |"
      , "| _Number of txs_ | " <> show numberOfTxs <> " |"
      , "| _Load mode_ | " <> loadMode <> " |"
      ]
        ++ maybe [] (\reason -> ["| _Outcome_ | FAILED: " <> reason <> " |"]) runOutcome
        ++ [ "| _Avg. Confirmation Time (ms)_ | " <> oneDec (nominalDiffTimeToMilliseconds averageConfirmationTime) <> " |"
           ]
        ++ ( if length quantiles == 100
              then
                [ "| _P99_ | " <> oneDec (quantiles ! 99) <> "ms |"
                , "| _P95_ | " <> oneDec (quantiles ! 95) <> "ms |"
                , "| _P50_ | " <> oneDec (quantiles ! 50) <> "ms |"
                ]
              else []
           )
        ++ maybe [] (\v -> [pack $ printf "| _Tx validation time p50 (ms)_ | %.1f |" v]) validationP50Ms
        ++ [pack $ printf "| _End-to-end TPS_ | %.2f tx/s |" endToEndTps]
        ++ maybe [] (\tps -> [pack $ printf "| _Sustained TPS_ | %.2f tx/s |" tps]) sustainedTps
        ++ [ pack $ printf "| _Backlog drain time (s)_ | %.1f |" drainSeconds
           , "| _Snapshots observed_ | " <> show numberOfSnapshots <> " |"
           , pack $ printf "| _Snapshots per second_ | %.2f /s |" (snapshotsPerSecond summary)
           , pack $ printf "| _Avg txs per snapshot_ | %.1f |" avgTxsPerSnapshot
           ]
        ++ maybe [] (\mb -> [pack $ printf "| _Peak node RSS (MB)_ | %.1f |" mb]) peakNodeRssMb
        ++ maybe
          []
          ( \(allocTx, allocSnap, cpu1k, live) ->
              [ pack $ printf "| _Alloc MB per confirmed tx_ | %.3f |" allocTx
              , pack $ printf "| _Alloc MB per snapshot_ | %.1f |" allocSnap
              , pack $ printf "| _Mutator CPU s per 1k txs_ | %.3f |" cpu1k
              , pack $ printf "| _Max live MB (max node)_ | %.1f |" live
              ]
          )
          (rtsAggregates summary)
        ++ [ "| _Number of Invalid txs_ | " <> show numberOfInvalidTxs <> " |"
           ]
        ++ [ "| _Fanout outputs_        | " <> show numberOfFanoutOutputs <> " |"
           ]
        ++ markdownIncremental "Incremental commit" incrementalCommitTimes
        ++ markdownIncremental "Incremental decommit" incrementalDecommitTimes
        ++ ["      "]
        ++ if null systemStats then [] else "\n### Memory data \n" : [unlines systemStats]
 where
  markdownIncremental :: Text -> [NominalDiffTime] -> [Text]
  markdownIncremental lbl = \case
    [] -> []
    ts ->
      let xs = map nominalDiffTimeToMilliseconds ts
          avg = sum xs / fromIntegral (length xs)
       in [ "| _" <> lbl <> " count_ | " <> show (length ts) <> " |"
          , "| _" <> lbl <> " avg (ms)_ | " <> oneDec avg <> " |"
          , "| _" <> lbl <> " max (ms)_ | " <> oneDec (List.maximum xs) <> " |"
          ]

-- | Markdown report for the matrix runner. Same per-scenario details as
-- 'markdownReport' but with a 'scenarios.md'-flavoured page header and a
-- leading comparison table summarising every cell in one view.
matrixMarkdownReport :: UTCTime -> [(Summary, SystemStats)] -> [Text]
matrixMarkdownReport now summaries =
  pageHeader <> comparisonTable summaries <> concatMap formattedSummary summaries
 where
  pageHeader :: [Text]
  pageHeader =
    [ "--- "
    , "sidebar_label: 'Scenario benchmarks' "
    , "sidebar_position: 5 "
    , "--- "
    , ""
    , "# Scenario benchmark results "
    , ""
    , "This page collects results from the scenario matrix: every combination \
      \ of cluster size, UTxO shape, and incremental-ops mode is exercised by \
      \ CI from the latest `master` code and reported below."
    , ""
    , ":::caution"
    , ""
    , "Numbers are approximate. They come from cloud VMs rather than \
      \ controlled hardware, so the useful signal is the relative change \
      \ between cells and between commits, not the absolute throughput."
    , ""
    , ":::"
    , ""
    , "_Generated at_  " <> show now
    , ""
    ]

comparisonTable :: [(Summary, SystemStats)] -> [Text]
comparisonTable summaries =
  [ ""
  , "## Summary across cells"
  , ""
  , "TPS columns are rates (transactions per second); _Wall clock (s)_ is the \
    \measured elapsed time from the first tx submission to the last \
    \confirmation. Times are rounded to one decimal."
  , ""
  , "| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |"
  , "| -- | -- | -- | -- | -- | -- | -- |"
  ]
    <> map row summaries
    <> [""]
 where
  row :: (Summary, SystemStats) -> Text
  row (Summary{numberOfTxs, summaryTitle, averageConfirmationTime, quantiles, endToEndTps, runWallClockSeconds, sustainedTps}, _)
    | numberOfTxs == 0 =
        "| "
          <> summaryTitle
          <> " | 0 | n/a | n/a | n/a | n/a | n/a |"
    | otherwise =
        let p95Conf = if length quantiles == 100 then oneDec (quantiles ! 95) else "n/a"
            sustained = maybe "n/a" (pack . printf "%.2f") sustainedTps
            wallClock =
              if runWallClockSeconds > 0
                then oneDec runWallClockSeconds
                else "n/a"
         in "| "
              <> summaryTitle
              <> " | "
              <> show numberOfTxs
              <> " | "
              <> wallClock
              <> " | "
              <> pack (printf "%.2f" endToEndTps)
              <> " | "
              <> sustained
              <> " | "
              <> oneDec (nominalDiffTimeToMilliseconds averageConfirmationTime)
              <> " | "
              <> p95Conf
              <> " |"

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Nano
nominalDiffTimeToMilliseconds = fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds
