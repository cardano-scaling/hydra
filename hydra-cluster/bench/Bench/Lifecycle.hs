{-# LANGUAGE DuplicateRecordFields #-}

-- | A scripted lifecycle, with observable faults, rather than a throughput run.
module Bench.Lifecycle where

import Hydra.Prelude

import Bench.EndToEnd (withDatasetCluster)
import Control.Lens ((^..), (^?))
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Lens (key, values)
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (Tx)
import Hydra.Cluster.HeadLifecycle (closeHead, fanoutHead, openHead)
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Hydra.Network (Host (..))
import Hydra.Tx (txId)
import HydraNode (HydraClient (..), input, send, waitMatch, waitNext, withSuspendedNode)
import System.FilePath ((</>))
import System.Timeout qualified as Timeout

data LifecycleSettings = LifecycleSettings
  { phaseSeconds :: Natural
  , peerPauseSeconds :: Natural
  , transactionIntervalMs :: Natural
  }
  deriving stock (Eq, Show)

-- | JSONL is flushed as each event happens, so recording can follow the same
-- timeline that remains available when a run fails or is interrupted.
runLifecycle :: Int -> NominalDiffTime -> LifecycleSettings -> FilePath -> Dataset -> IO ()
runLifecycle firstNode timeoutSeconds settings workDir dataset@Dataset{clientDatasets} = do
  when (length clientDatasets /= 3) $ fail "The peer-disappearance scenario requires three participants"
  when (peerPauseSeconds settings == 0) $ fail "--peer-pause-seconds must be positive"
  chain <- case clientDatasets of
    ClientDataset{txSequence} : _ | length txSequence >= 3 -> pure txSequence
    _ -> fail "The lifecycle scenario requires at least three transactions"
  withFile (workDir </> "timeline.jsonl") WriteMode $ \journal -> do
    hSetBuffering journal LineBuffering
    started <- getMonotonicTime
    let event phase status message = do
          timestamp <- getCurrentTime
          now <- getMonotonicTime
          let entry =
                object
                  [ "schemaVersion" .= (1 :: Int)
                  , "timestamp" .= timestamp
                  , "elapsedSeconds" .= (realToFrac (diffTime now started) :: Double)
                  , "phase" .= (phase :: Text)
                  , "status" .= (status :: Text)
                  , "message" .= (message :: Text)
                  ]
          LBS.hPutStr journal (encode entry <> "\n")
          putTextLn $ phase <> ": " <> message
        hold = threadDelay (fromIntegral $ phaseSeconds settings)
        interval = threadDelay (fromIntegral (transactionIntervalMs settings) / 1000)
        submit :: HydraClient -> Tx -> IO ()
        submit client tx = send client $ input "NewTx" ["transaction" .= toJSON tx]
        confirm tx client = void $ waitMatch 120 client (confirms tx)
        runTransactions client clients txs = forM_ txs $ \tx -> do
          submit client tx
          mapM_ (confirm tx) clients
          interval
    event "setup" "started" "Starting a local Cardano devnet and three Hydra participants"
    let run = void $ withDatasetCluster firstNode timeoutSeconds False workDir dataset $ \tracer timing opts nodes _ -> do
          let clients = toList nodes
              leader = head nodes
              peer = last nodes
              (before, remaining) = splitAt (length chain `div` 2) chain
          (faultTx, after) <- case remaining of
            tx : txs -> pure (tx, txs)
            [] -> fail "Missing transaction for the peer fault"
          let targets =
                [ object ["targets" .= [hostname (apiHost c) <> ":" <> show p], "labels" .= object ["node" .= (show (hydraNodeId c) :: Text)]]
                | c <- clients
                , Just p <- [monitoringPort c]
                ]
          LBS.writeFile (workDir </> "targets.json") (encode targets)
          LBS.writeFile (workDir </> "endpoints.json") $
            encode
              [object ["node" .= hydraNodeId c, "api" .= (show (apiHost c) :: Text), "monitoringPort" .= monitoringPort c] | c <- clients]
          event "ready" "observed" "Participants are connected; Prometheus targets are available"
          hold
          event "opening" "started" "Opening the Head and depositing the benchmark dataset"
          headId <- openHead timing opts clientDatasets nodes
          event "opening" "observed" "The Head is open and all deposits have settled"
          hold
          event "steady" "started" "Sending a valid transaction chain, waiting for confirmation on every participant"
          runTransactions leader clients before
          event "steady" "observed" "Baseline transactions are confirmed everywhere"
          hold
          event "peer-disappearance" "started" $ "Suspending Hydra participant " <> show (hydraNodeId peer)
          withSuspendedNode peer $ do
            event "peer-disappearance" "applied" "The participant and its etcd process are suspended"
            submit leader faultTx
            forM_ (take 2 clients) $ \client ->
              void $ waitMatch 30 client $ \v -> do
                guard $ v ^? key "tag" == Just "TxValid"
                guard $ v ^? key "transactionId" == Just (toJSON $ txId faultTx)
            event "peer-disappearance" "observed" "The remaining participants received and validated the transaction"
            result <-
              Timeout.timeout (fromIntegral (peerPauseSeconds settings) * 1_000_000) $
                let go = waitNext leader >>= maybe go pure . confirms faultTx
                 in go
            when (isJust result) $ fail "A transaction confirmed while a signing participant was suspended"
            event "peer-disappearance" "observed" "The submitted transaction remains unconfirmed during the outage"
          event "recovery" "applied" "The participant and its etcd process have been resumed"
          mapM_ (confirm faultTx) clients
          event "recovery" "observed" "Every participant confirmed the transaction held during the outage"
          hold
          runTransactions leader clients after
          event "recovered-traffic" "observed" "Further transactions confirm after recovery"
          hold
          event "closing" "started" "Closing with the latest confirmed snapshot"
          deadline <- closeHead timing headId leader
          event "closing" "observed" "Close observed; waiting for the contestation deadline"
          event "fanout" "started" "Waiting for the deadline and distributing the Head UTxO"
          outputs <- fanoutHead tracer deadline headId leader
          when (outputs == 0) $ fail "The Head finalized without distributing the dataset outputs"
          event "fanout" "observed" $ "Head finalized with " <> show outputs <> " outputs"
          hold
          event "complete" "observed" "Lifecycle, peer disappearance and recovery completed successfully"
    run
      `catch` \(err :: SomeException) -> do
        event "failed" "failed" (show err)
        throwIO err

confirms :: Tx -> Value -> Maybe ()
confirms tx value = do
  guard $ value ^? key "tag" == Just "SnapshotConfirmed"
  let ids = value ^.. key "snapshot" . key "confirmed" . values . key "txId"
  guard $ toJSON (txId tx) `elem` ids
  pure ()
