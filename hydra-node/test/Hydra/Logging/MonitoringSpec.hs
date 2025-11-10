module Hydra.Logging.MonitoringSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Text qualified as Text

import Hydra.HeadLogic.Outcome (Outcome (..), StateChanged (..))
import Hydra.HeadLogicSpec (receiveMessage, testSnapshot)
import Hydra.Ledger.Simple (aValidTx, utxoRefs)
import Hydra.Logging (nullTracer, traceWith)
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Logging.Monitoring
import Hydra.Network (Host (Host))
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node (HydraNodeLog (..))

-- import Network.Socket (PortNumber(PortNumber))
import Network.HTTP.Req (GET (..), NoReqBody (..), bsResponse, defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Test.Hydra.Tx.Fixture (alice, testHeadId)
import Test.Network.Ports (randomUnusedTCPPorts)

spec :: Spec
spec =
  it "provides prometheus metrics from traces" $ do
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
        let tx1 = aValidTx 42
        let tx2 = aValidTx 43
        traceWith tracer (Node $ BeginInput alice 0 (receiveMessage (ReqTx tx1)))
        traceWith tracer (Node $ BeginInput alice 1 (receiveMessage (ReqTx tx2)))
        threadDelay 100_000
        traceWith tracer (Node $ LogicOutcome alice (Continue [SnapshotConfirmed testHeadId (testSnapshot 1 1 [tx2, tx1] (utxoRefs [1])) mempty] mempty))
        traceWith tracer (Node $ LogicOutcome alice (Continue [PeerConnected (Host "a" 1)] mempty))
        traceWith tracer (Node $ LogicOutcome alice (Continue [PeerConnected (Host "b" 2)] mempty))
        traceWith tracer (Node $ LogicOutcome alice (Continue [PeerDisconnected (Host "b" 2)] mempty))

        metrics <-
          Text.lines
            . decodeUtf8
            . responseBody
            <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))

        metrics `shouldContain` ["hydra_head_confirmed_tx  2"]
        metrics `shouldContain` ["hydra_head_peers_connected  1.0"]
        metrics `shouldContain` ["hydra_head_tx_confirmation_time_ms_bucket{le=\"1000.0\"} 2.0"]

        traceWith tracer (Node $ LogicOutcome alice (Continue [NetworkDisconnected] mempty))

        m <-
          Text.lines
            . decodeUtf8
            . responseBody
            <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))

        m `shouldContain` ["hydra_head_peers_connected  0.0"]
