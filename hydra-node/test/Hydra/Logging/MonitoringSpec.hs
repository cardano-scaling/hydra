module Hydra.Logging.MonitoringSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Text qualified as Text

import Control.Tracer.JSON (Tracer, nullTracer, traceWith)
import GHC.Stats (getRTSStatsEnabled)
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.API.ServerOutput (ClientMessage (RejectedInputBecauseBroadcastStalled))
import Hydra.HeadLogic.Outcome (Effect (ClientEffect), Outcome (..), StateChanged (..))
import Hydra.HeadLogicSpec (receiveMessage, testSnapshot)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Logging.Monitoring
import Hydra.Network (Host (Host), StallReason (..))
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node (HydraNodeLog (..))

-- import Network.Socket (PortNumber(PortNumber))
import Network.HTTP.Req (GET (..), NoReqBody (..), bsResponse, defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Test.Hydra.Ledger.Simple (aValidTx, utxoRefs)
import Test.Hydra.Tx.Fixture (alice, testHeadId)
import Test.Network.Ports (randomUnusedTCPPorts)

spec :: Spec
spec = do
  it "records snapshot round and tx confirmation times on the normal signing path" $ do
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
        let tx = aValidTx 42
            snapshot = testSnapshot 1 1 [tx] (utxoRefs [1])
        traceWith tracer (Node $ BeginInput alice 0 (receiveMessage (ReqTx tx)))
        threadDelay 0.1
        traceWith tracer (Node $ LogicOutcome alice (Continue [SnapshotRequested snapshot mempty Nothing] mempty))
        threadDelay 0.1
        -- The normal signing path confirms without a snapshot in the event
        traceWith tracer (Node $ LogicOutcome alice (Continue [SnapshotConfirmed testHeadId Nothing mempty] mempty))

        metrics <- Text.lines <$> scrapeMetrics p

        metrics `shouldContain` ["hydra_head_confirmed_tx  1"]
        metrics `shouldContain` ["hydra_head_snapshot_confirmation_time_ms_bucket{le=\"1000.0\"} 1.0"]
        metrics `shouldContain` ["hydra_head_tx_confirmation_time_ms_bucket{le=\"1000.0\"} 1.0"]

  -- These names are a public interface: Prometheus scrapes them, and several
  -- are referenced by the dashboards under demo/grafana, so dropping or
  -- renaming one breaks operators without breaking any other test. All are
  -- registered when monitoring starts, so a scrape carries every one of them
  -- before anything has been observed.
  it "registers every documented metric on start up" $ do
    failAfter 10 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \(_ :: Tracer IO (HydraLog SimpleTx)) -> do
        scraped <- scrapedMetricNames <$> scrapeMetrics p
        filter (`notElem` scraped) protocolMetrics `shouldBe` []
        -- The RTS gauges are registered only when the process runs with
        -- '+RTS -T', which this suite does not, so they are expected absent
        -- here and present anywhere that does.
        rtsEnabled <- getRTSStatsEnabled
        filter (`elem` scraped) rtsMetrics
          `shouldBe` (if rtsEnabled then rtsMetrics else [])

  it "reports a stalled broadcast, its backlog and what it refused" $ do
    -- The metrics an operator diagnosing GHSA-3mmr-q43p-g6p2 reads: whether
    -- the hand-off is stalled, how much it holds and for how long, and how
    -- many client inputs that cost.
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
        let scrape =
              Text.lines . decodeUtf8 . responseBody
                <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))

        traceWith tracer (Node $ BroadcastBacklog 7 4)
        traceWith tracer (Node $ LogicOutcome alice (Continue [NetworkBroadcastStalled 7 NoProgress] mempty))
        traceWith tracer . Node . LogicOutcome alice $
          Continue mempty [ClientEffect (RejectedInputBecauseBroadcastStalled (NewTx (aValidTx 42)) 7 NoProgress)]

        stalledMetrics <- scrape
        stalledMetrics `shouldContain` ["hydra_head_broadcast_stalled  1.0"]
        stalledMetrics `shouldContain` ["hydra_head_pending_broadcasts  7.0"]
        stalledMetrics `shouldContain` ["hydra_head_broadcast_no_progress_seconds  4.0"]
        stalledMetrics `shouldContain` ["hydra_head_inputs_refused_broadcast_stalled  1"]
        -- Which of the two conditions it is, so an operator can tell an
        -- unreachable network from one merely being outrun (review: vrom911).
        stalledMetrics `shouldContain` ["hydra_head_broadcast_stalled_no_progress  1.0"]
        stalledMetrics `shouldContain` ["hydra_head_broadcast_stalled_backlog_full  0.0"]

        -- The other cause must move the other gauge, and only that one.
        traceWith tracer (Node $ LogicOutcome alice (Continue [NetworkBroadcastStalled 1000 BacklogFull] mempty))
        backlogMetrics <- scrape
        backlogMetrics `shouldContain` ["hydra_head_broadcast_stalled  1.0"]
        backlogMetrics `shouldContain` ["hydra_head_broadcast_stalled_no_progress  0.0"]
        backlogMetrics `shouldContain` ["hydra_head_broadcast_stalled_backlog_full  1.0"]

        -- Draining again must take them all back down, or a cleared stall
        -- looks like a current one forever.
        traceWith tracer (Node $ BroadcastBacklog 0 0)
        traceWith tracer (Node $ LogicOutcome alice (Continue [NetworkBroadcastResumed] mempty))

        resumedMetrics <- scrape
        resumedMetrics `shouldContain` ["hydra_head_broadcast_stalled  0.0"]
        resumedMetrics `shouldContain` ["hydra_head_broadcast_stalled_no_progress  0.0"]
        resumedMetrics `shouldContain` ["hydra_head_broadcast_stalled_backlog_full  0.0"]
        resumedMetrics `shouldContain` ["hydra_head_pending_broadcasts  0.0"]
        resumedMetrics `shouldContain` ["hydra_head_broadcast_no_progress_seconds  0.0"]

  it "provides prometheus metrics from traces" $ do
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
        let tx1 = aValidTx 42
        let tx2 = aValidTx 43
        traceWith tracer (Node $ BeginInput alice 0 (receiveMessage (ReqTx tx1)))
        traceWith tracer (Node $ BeginInput alice 1 (receiveMessage (ReqTx tx2)))
        threadDelay 0.1
        traceWith tracer (Node $ LogicOutcome alice (Continue [SnapshotConfirmed testHeadId (Just (testSnapshot 1 1 [tx2, tx1] (utxoRefs [1]))) mempty] mempty))
        traceWith tracer (Node $ LogicOutcome alice (Continue [PeerConnected (Host "a" 1)] mempty))
        traceWith tracer (Node $ LogicOutcome alice (Continue [PeerConnected (Host "b" 2)] mempty))
        traceWith tracer (Node $ LogicOutcome alice (Continue [PeerDisconnected (Host "b" 2)] mempty))

        metrics <- Text.lines <$> scrapeMetrics p

        metrics `shouldContain` ["hydra_head_confirmed_tx  2"]
        metrics `shouldContain` ["hydra_head_peers_connected  1.0"]
        metrics `shouldContain` ["hydra_head_tx_confirmation_time_ms_bucket{le=\"1000.0\"} 2.0"]

        traceWith tracer (Node $ LogicOutcome alice (Continue [NetworkDisconnected] mempty))

        m <- Text.lines <$> scrapeMetrics p

        m `shouldContain` ["hydra_head_peers_connected  0.0"]

-- | The metrics 'withMonitoring' always registers.
protocolMetrics :: [Text]
protocolMetrics =
  [ "hydra_head_inputs"
  , "hydra_head_requested_tx"
  , "hydra_head_confirmed_tx"
  , "hydra_head_tx_confirmation_time_ms"
  , "hydra_head_snapshot_confirmation_time_ms"
  , "hydra_head_peers_connected"
  , "hydra_chain_drift_seconds"
  , "hydra_chain_last_block_timestamp_seconds"
  ]

-- | Registered only when the RTS collects statistics, see 'registerRtsMetrics'.
rtsMetrics :: [Text]
rtsMetrics =
  [ "hydra_rts_allocated_bytes"
  , "hydra_rts_mutator_cpu_seconds"
  , "hydra_rts_gc_cpu_seconds"
  , "hydra_rts_max_live_bytes"
  , "hydra_rts_major_gcs"
  ]

-- | The metric names a scrape actually exposes: the identifier starting each
-- sample line, with the suffixes Prometheus appends to a histogram removed.
--
-- Matching whole names rather than substrings is the point: renaming a counter
-- to, say, @hydra_head_inputs_total@ has to be caught, and the old name is a
-- prefix of the new one.
scrapedMetricNames :: Text -> [Text]
scrapedMetricNames body =
  ordNub
    [ stripHistogramSuffix ident
    | line <- Text.lines body
    , not ("#" `Text.isPrefixOf` line)
    , let ident = Text.takeWhile (\c -> c /= ' ' && c /= '{') line
    , not (Text.null ident)
    ]
 where
  stripHistogramSuffix name =
    fromMaybe name . asum $
      [Text.stripSuffix suffix name | suffix <- ["_bucket", "_sum", "_count"]]

-- | 'withMonitoring' forks its server and returns immediately, so a scrape can
-- arrive before the socket is listening.
scrapeMetrics :: Int -> IO Text
scrapeMetrics p = go (100 :: Int)
 where
  go n =
    try scrape >>= \case
      Right body -> pure body
      Left (e :: SomeException)
        | n <= 0 -> throwIO e
        | otherwise -> threadDelay 0.05 >> go (n - 1)

  scrape =
    decodeUtf8
      . responseBody
      <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))
