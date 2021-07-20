{-# LANGUAGE TypeApplications #-}

module Hydra.Logging.MonitoringSpec where

import qualified Data.Text as Text
import Hydra.HeadLogic (
  Effect (ClientEffect),
  Event (NetworkEvent),
  HydraMessage (ReqTx),
  ServerOutput (SnapshotConfirmed),
  Snapshot (..),
 )
import Hydra.Ledger.Simple (aValidTx, utxoRefs)
import Hydra.Logging (nullTracer, traceWith)
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Logging.Monitoring
import Hydra.Network.Ports (withFreePort)
import Hydra.Node (HydraNodeLog (ProcessedEffect, ProcessingEvent))
import Hydra.Prelude
import Network.HTTP.Req (GET (..), NoReqBody (..), bsResponse, defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Test.Hspec
import Test.Util (failAfter)

spec :: Spec
spec = describe "Prometheus Metrics" $ do
  it "provides count of confirmed transactions from traces" $ do
    failAfter 3 $
      withFreePort $ \p ->
        withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
          traceWith tracer (Node $ ProcessingEvent 1 (NetworkEvent (ReqTx 1 (aValidTx 42))))
          traceWith tracer (Node $ ProcessingEvent 1 (NetworkEvent (ReqTx 1 (aValidTx 43))))
          traceWith tracer (Node $ ProcessedEffect 1 (ClientEffect (SnapshotConfirmed $ Snapshot 1 (utxoRefs [1]) [aValidTx 43, aValidTx 42])))

          response <- runReq @IO defaultHttpConfig $ req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p)

          Text.lines (decodeUtf8 $ responseBody response) `shouldContain` ["hydra_head_confirmed_tx  2"]

  it "provides histogram of txs confirmation time from traces" $ do
    failAfter 3 $
      withFreePort $ \p ->
        withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
          traceWith tracer (Node $ ProcessingEvent 1 (NetworkEvent (ReqTx 1 (aValidTx 42))))
          threadDelay 0.1
          traceWith tracer (Node $ ProcessedEffect 1 (ClientEffect (SnapshotConfirmed $ Snapshot 1 (utxoRefs [1]) [aValidTx 42])))

          response <- runReq @IO defaultHttpConfig $ req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p)

          Text.lines (decodeUtf8 $ responseBody response) `shouldContain` ["hydra_head_tx_confirmation_time_ms_bucket{le=\"1000.0\"} 1.0"]
