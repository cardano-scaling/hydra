module Hydra.Logging.MonitoringSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Text qualified as Text

import Control.Tracer.JSON (Tracer, nullTracer, traceWith)
import Hydra.HeadLogic.Outcome (Outcome (..), StateChanged (..))
import Hydra.HeadLogicSpec (receiveMessage, testSnapshot)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Logging.Monitoring
import Hydra.Network (Host (Host))
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

  -- These names are a public interface: Prometheus scrapes them and the
  -- dashboards under demo/grafana refer to them, so dropping or renaming one
  -- breaks operators without breaking any other test. They are all registered
  -- when monitoring starts, so a scrape carries every one of them before
  -- anything has been observed.
  it "registers every documented metric on start up" $ do
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \(_ :: Tracer IO (HydraLog SimpleTx)) -> do
        metrics <- scrapeMetrics p
        let missing = filter (not . (`Text.isInfixOf` metrics)) documentedMetrics
        missing `shouldBe` []

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

-- | Every metric registered by 'withMonitoring'.
documentedMetrics :: [Text]
documentedMetrics =
  [ "hydra_head_inputs"
  , "hydra_head_requested_tx"
  , "hydra_head_confirmed_tx"
  , "hydra_head_tx_confirmation_time_ms"
  , "hydra_head_snapshot_confirmation_time_ms"
  , "hydra_head_peers_connected"
  , "hydra_chain_drift_seconds"
  , "hydra_chain_last_block_timestamp_seconds"
  ]

scrapeMetrics :: Int -> IO Text
scrapeMetrics p =
  decodeUtf8
    . responseBody
    <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))
