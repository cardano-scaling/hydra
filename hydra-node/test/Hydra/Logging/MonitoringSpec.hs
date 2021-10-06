{-# LANGUAGE TypeApplications #-}

module Hydra.Logging.MonitoringSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.Text as Text
import Hydra.HeadLogic (
  Effect (ClientEffect),
  Event (NetworkEvent),
 )
import Hydra.Ledger.Simple (aValidTx, utxoRefs)
import Hydra.Logging (nullTracer, traceWith)
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Logging.Monitoring
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node (HydraNodeLog (ProcessedEffect, ProcessingEvent))
import Hydra.ServerOutput (ServerOutput (SnapshotConfirmed))
import Hydra.Snapshot (Snapshot (Snapshot))
import Network.HTTP.Req (GET (..), NoReqBody (..), bsResponse, defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Test.Network.Ports (randomUnusedTCPPorts)

spec :: Spec
spec = do
  it "provides prometheus metrics from traces" $ do
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
        traceWith tracer (Node $ ProcessingEvent 1 (NetworkEvent (ReqTx 1 (aValidTx 42))))
        traceWith tracer (Node $ ProcessingEvent 1 (NetworkEvent (ReqTx 1 (aValidTx 43))))
        threadDelay 0.1
        traceWith tracer (Node $ ProcessedEffect 1 (ClientEffect (SnapshotConfirmed $ Snapshot 1 (utxoRefs [1]) [aValidTx 43, aValidTx 42])))

        metrics <-
          Text.lines . decodeUtf8 . responseBody
            <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))

        metrics `shouldContain` ["hydra_head_confirmed_tx  2"]
        metrics `shouldContain` ["hydra_head_tx_confirmation_time_ms_bucket{le=\"1000.0\"} 2.0"]
