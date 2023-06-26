{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Bench.Summary where

import Hydra.Prelude

import Data.Time (nominalDiffTimeToSeconds)
import Data.Fixed (Nano)

type Percent = Double

data Summary = Summary
  { clusterSize :: Word64
  , numberOfTxs :: Int
  , averageConfirmationTime :: NominalDiffTime
  , percentBelow100ms :: Percent
  , summaryTitle :: Text
  , summaryDescription :: Text
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

textReport :: Summary -> [Text]
textReport Summary{numberOfTxs, averageConfirmationTime, percentBelow100ms}  =
  [ "Confirmed txs: " <> show numberOfTxs
  , "Average confirmation time (ms): " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime)
  , "Confirmed below 100ms: " <> show percentBelow100ms <> "%"
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
    , "# End-To-End Benchmark Results "
    , ""
    , "This page is intended to collect the latest end-to-end benchmarks \
      \ results produced by Hydra's Continuous Integration system from \
      \ the latest `master` code."
    , ""
    , ":::caution"
    , ""
    , "Please take those results with a grain of \
      \ salt as they are currently produced from very limited cloud VMs and not controlled \
      \ hardware. Instead of focusing on the _absolute_ results, the emphasis \
      \ should be on relative results, eg. how the timings for a scenario \
      \ evolve as the code changes."
    , ""
    , ":::"
    , ""
    , "_Generated at_  " <> show now
    , ""
    ]

  formattedSummary :: Summary -> [Text]
  formattedSummary Summary{clusterSize, numberOfTxs, averageConfirmationTime, percentBelow100ms, summaryTitle, summaryDescription} =
    [ ""
    , "## " <> summaryTitle
    , ""
    , summaryDescription
    , ""
    , "| Number of nodes |  " <> show clusterSize <> " | "
    , "| -- | -- |"
    , "| _Number of txs_ | " <> show numberOfTxs <> " |"
    , "| _Avg. Confirmation Time (ms)_ | " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime) <> " |"
    , "| _Share of Txs (%) < 100ms_ | " <> show percentBelow100ms <> " |"
    ]

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Nano
nominalDiffTimeToMilliseconds = fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds
