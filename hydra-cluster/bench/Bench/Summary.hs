{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bench.Summary where

import Hydra.Prelude

import Data.Fixed (Nano)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Vector (Vector, (!))
import Statistics.Quantile (def)
import Statistics.Quantile qualified as Statistics

type Percent = Double

data Summary = Summary
  { clusterSize :: Word64
  , numberOfTxs :: Int
  , numberOfInvalidTxs :: Int
  , averageConfirmationTime :: NominalDiffTime
  , summaryTitle :: Text
  , summaryDescription :: Text
  , quantiles :: Vector Double
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

makeQuantiles :: [NominalDiffTime] -> Vector Double
makeQuantiles times =
  Statistics.quantilesVec def (fromList [0 .. 99]) 100 (fromList $ map (fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds) times)

textReport :: Summary -> [Text]
textReport Summary{numberOfTxs, averageConfirmationTime, quantiles, numberOfInvalidTxs} =
  [ "Confirmed txs: " <> show numberOfTxs
  , "Average confirmation time (ms): " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime)
  , "P99: " <> show (quantiles ! 99) <> "ms"
  , "P95: " <> show (quantiles ! 95) <> "ms"
  , "P50: " <> show (quantiles ! 50) <> "ms"
  , "Invalid txs: " <> show numberOfInvalidTxs
  ]

markdownReport :: UTCTime -> [Summary] -> [Text]
markdownReport now summaries =
  pageHeader <> concatMap formattedSummary summaries
 where
  pageHeader :: [Text]
  pageHeader =
    [ "--- "
    , "sidebar_label: 'End-to-End Benchmarks' "
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

  formattedSummary :: Summary -> [Text]
  formattedSummary Summary{clusterSize, numberOfTxs, averageConfirmationTime, quantiles, summaryTitle, summaryDescription, numberOfInvalidTxs} =
    [ ""
    , "## " <> summaryTitle
    , ""
    , summaryDescription
    , ""
    , "| Number of nodes |  " <> show clusterSize <> " | "
    , "| -- | -- |"
    , "| _Number of txs_ | " <> show numberOfTxs <> " |"
    , "| _Avg. Confirmation Time (ms)_ | " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime) <> " |"
    , "| _P99_ | " <> show (quantiles ! 99) <> "ms |"
    , "| _P95_ | " <> show (quantiles ! 95) <> "ms |"
    , "| _P50_ | " <> show (quantiles ! 50) <> "ms |"
    , "| _Number of Invalid txs_ | " <> show numberOfInvalidTxs <> " |"
    ]

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Nano
nominalDiffTimeToMilliseconds = fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds
