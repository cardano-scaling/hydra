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

type Percent = Double

-- | System stats like memory consumption.
type SystemStats = [Text]

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
  , snapshotTpsQuantiles :: Vector Double
  , numberOfSnapshots :: Int
  , incrementalCommitTimes :: [NominalDiffTime]
  , incrementalDecommitTimes :: [NominalDiffTime]
  }
  deriving stock (Generic, Eq, Show)

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
    , snapshotTpsQuantiles = mempty
    , numberOfSnapshots = 0
    , incrementalCommitTimes = []
    , incrementalDecommitTimes = []
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
-- makeQuantiles [] = mempty -- No confirmations, no quantiles.
makeQuantiles times =
  Statistics.quantilesVec def (fromList [0 .. 99]) 100 (fromList $ map (fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds) times)

makeDoubleQuantiles :: [Double] -> Vector Double
makeDoubleQuantiles [] = mempty
makeDoubleQuantiles xs =
  Statistics.quantilesVec def (fromList [0 .. 99]) 100 (fromList xs)

-- | Render a time value with at most one decimal place.
oneDec :: Real a => a -> Text
oneDec x = pack $ printf "%.1f" (realToFrac x :: Double)

textReport :: (Summary, SystemStats) -> [Text]
textReport (Summary{totalTxs, numberOfTxs, averageConfirmationTime, quantiles, numberOfInvalidTxs, numberOfFanoutOutputs, endToEndTps, snapshotTpsQuantiles, numberOfSnapshots, incrementalCommitTimes, incrementalDecommitTimes}, systemStats) =
  let frac :: Double
      frac = 100 * fromIntegral numberOfTxs / fromIntegral totalTxs
   in [ pack $ printf "Confirmed txs/Total expected txs: %d/%d (%.2f %%)" numberOfTxs totalTxs frac
      , "Average confirmation time (ms): " <> oneDec (nominalDiffTimeToMilliseconds averageConfirmationTime)
      ]
        ++ ( if length quantiles == 100
              then
                [ "P99: " <> oneDec (quantiles ! 99) <> "ms"
                , "P95: " <> oneDec (quantiles ! 95) <> "ms"
                , "P50: " <> oneDec (quantiles ! 50) <> "ms"
                ]
              else []
           )
        ++ [pack $ printf "End-to-end TPS: %.2f tx/s" endToEndTps]
        ++ [pack $ printf "Snapshots observed: %d" numberOfSnapshots]
        ++ ( if length snapshotTpsQuantiles == 100
              then
                [ pack $ printf "Per-snapshot TPS P50: %.2f tx/s" (snapshotTpsQuantiles ! 50)
                , pack $ printf "Per-snapshot TPS P95: %.2f tx/s" (snapshotTpsQuantiles ! 95)
                , pack $ printf "Per-snapshot TPS max: %.2f tx/s" (snapshotTpsQuantiles ! 99)
                ]
              else []
           )
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
formattedSummary (Summary{clusterSize, numberOfTxs, averageConfirmationTime, quantiles, summaryTitle, summaryDescription, numberOfInvalidTxs, numberOfFanoutOutputs, endToEndTps, snapshotTpsQuantiles, numberOfSnapshots, incrementalCommitTimes, incrementalDecommitTimes}, systemStats)
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
      , "| _Avg. Confirmation Time (ms)_ | " <> oneDec (nominalDiffTimeToMilliseconds averageConfirmationTime) <> " |"
      ]
        ++ ( if length quantiles == 100
              then
                [ "| _P99_ | " <> oneDec (quantiles ! 99) <> "ms |"
                , "| _P95_ | " <> oneDec (quantiles ! 95) <> "ms |"
                , "| _P50_ | " <> oneDec (quantiles ! 50) <> "ms |"
                ]
              else []
           )
        ++ [ pack $ printf "| _End-to-end TPS_ | %.2f tx/s |" endToEndTps
           , "| _Snapshots observed_ | " <> show numberOfSnapshots <> " |"
           ]
        ++ ( if length snapshotTpsQuantiles == 100
              then
                [ pack $ printf "| _Per-snapshot TPS P50_ | %.2f tx/s |" (snapshotTpsQuantiles ! 50)
                , pack $ printf "| _Per-snapshot TPS P95_ | %.2f tx/s |" (snapshotTpsQuantiles ! 95)
                , pack $ printf "| _Per-snapshot TPS max_ | %.2f tx/s |" (snapshotTpsQuantiles ! 99)
                ]
              else []
           )
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
    \matching elapsed time, computed as `Txs / End-to-end TPS`, so a row with \
    \50 txs and 50.58 tx/s ran in about 1.0 s. Times are rounded to one decimal."
  , ""
  , "| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |"
  , "| -- | -- | -- | -- | -- | -- | -- |"
  ]
    <> map row summaries
    <> [""]
 where
  row :: (Summary, SystemStats) -> Text
  row (Summary{numberOfTxs, summaryTitle, averageConfirmationTime, quantiles, endToEndTps, snapshotTpsQuantiles}, _)
    | numberOfTxs == 0 =
        "| "
          <> summaryTitle
          <> " | 0 | n/a | n/a | n/a | n/a | n/a |"
    | otherwise =
        let p95Conf = if length quantiles == 100 then oneDec (quantiles ! 95) else "n/a"
            p50Tps =
              if length snapshotTpsQuantiles == 100
                then pack $ printf "%.2f" (snapshotTpsQuantiles ! 50)
                else "n/a"
            wallClock =
              if endToEndTps > 0
                then oneDec (fromIntegral numberOfTxs / endToEndTps :: Double)
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
              <> p50Tps
              <> " | "
              <> oneDec (nominalDiffTimeToMilliseconds averageConfirmationTime)
              <> " | "
              <> p95Conf
              <> " |"

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Nano
nominalDiffTimeToMilliseconds = fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds
