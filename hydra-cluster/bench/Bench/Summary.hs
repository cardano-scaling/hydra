{-# LANGUAGE DuplicateRecordFields #-}

module Bench.Summary where

import Hydra.Prelude

import Data.Fixed (Nano)
import Data.Text (pack)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Vector (Vector, (!))
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Statistics.Quantile (def)
import Statistics.Quantile qualified as Statistics
import Test.HUnit.Lang (formatFailureReason)
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
    , summaryTitle = maybe "Error Summary" ("Error Summary " <>) title
    , summaryDescription =
        pack $ "Benchmark failed " <> formatLocation sourceLocation <> ": " <> formatFailureReason reason
    , quantiles = mempty
    , numberOfFanoutOutputs = 0
    }
 where
  formatLocation = maybe "" (\loc -> "at " <> prettySrcLoc loc)

makeQuantiles :: [NominalDiffTime] -> Vector Double
-- makeQuantiles [] = mempty -- No confirmations, no quantiles.
makeQuantiles times =
  Statistics.quantilesVec def (fromList [0 .. 99]) 100 (fromList $ map (fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds) times)

textReport :: (Summary, SystemStats) -> [Text]
textReport (Summary{totalTxs, numberOfTxs, averageConfirmationTime, quantiles, numberOfInvalidTxs, numberOfFanoutOutputs}, systemStats) =
  let frac :: Double
      frac = 100 * fromIntegral numberOfTxs / fromIntegral totalTxs
   in [ pack $ printf "Confirmed txs/Total expected txs: %d/%d (%.2f %%)" numberOfTxs totalTxs frac
      , "Average confirmation time (ms): " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime)
      ]
        ++ ( if length quantiles == 100
              then
                [ "P99: " <> show (quantiles ! 99) <> "ms"
                , "P95: " <> show (quantiles ! 95) <> "ms"
                , "P50: " <> show (quantiles ! 50) <> "ms"
                ]
              else []
           )
        ++ ["Invalid txs: " <> show numberOfInvalidTxs]
        ++ ["Fanout outputs: " <> show numberOfFanoutOutputs]
        ++ if null systemStats then [] else "\n### Memory data \n" : [unlines systemStats]

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
  formattedSummary (Summary{clusterSize, numberOfTxs, averageConfirmationTime, quantiles, summaryTitle, summaryDescription, numberOfInvalidTxs}, systemStats) =
    [ ""
    , "## " <> summaryTitle
    , ""
    , summaryDescription
    , ""
    , "| Number of nodes |  " <> show clusterSize <> " | "
    , "| -- | -- |"
    , "| _Number of txs_ | " <> show numberOfTxs <> " |"
    , "| _Avg. Confirmation Time (ms)_ | " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime) <> " |"
    ]
      ++ ( if length quantiles == 100
            then
              [ "| _P99_ | " <> show (quantiles ! 99) <> "ms |"
              , "| _P95_ | " <> show (quantiles ! 95) <> "ms |"
              , "| _P50_ | " <> show (quantiles ! 50) <> "ms |"
              ]
            else []
         )
      ++ [ "| _Number of Invalid txs_ | " <> show numberOfInvalidTxs <> " |"
         ]
      ++ ["      "]
      ++ if null systemStats then [] else "\n### Memory data \n" : [unlines systemStats]

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Nano
nominalDiffTimeToMilliseconds = fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds
