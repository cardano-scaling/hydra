{-# LANGUAGE DuplicateRecordFields #-}

-- | A scripted lifecycle, with observable faults, rather than a throughput run.
module Bench.Lifecycle where

import Hydra.Prelude

import Bench.EndToEnd (withDatasetCluster)
import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (runBackend)
import Control.Lens ((^..), (^?))
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Lens (key, members, values, _JSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Hydra.Cardano.Api (Tx, UTxO, makeSignedTransaction, txBody, txOutAddress, txOutValue)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Cluster.HeadLifecycle (closeHead, fanoutHead, openHead)
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Network (Host (..))
import Hydra.Options (ChainBackendOptions)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (signTx)
import HydraNode (HydraClient (..), findChildProcess, getMetrics, getSnapshotUTxO, input, requestCommitTx, send, waitMatch, waitNext, withSuspendedEtcd, withSuspendedNode, withSuspendedProcess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Timeout qualified as Timeout

data LifecycleSettings = LifecycleSettings
  { phaseSeconds :: Natural
  , peerPauseSeconds :: Natural
  , transactionIntervalMs :: Natural
  , metricsShowcase :: Bool
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
    let run = void $ withDatasetCluster firstNode timeoutSeconds (metricsShowcase settings) workDir dataset $ \tracer timing opts nodes sideUTxOs -> do
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
          when (metricsShowcase settings) $
            showcaseMetrics event hold workDir opts nodes dataset sideUTxOs
          event "closing" "started" "Closing with the latest confirmed snapshot"
          deadline <- closeHead timing headId leader
          event "closing" "observed" "Close observed; waiting for the contestation deadline"
          event "fanout" "started" "Waiting for the deadline and distributing the Head UTxO"
          outputs <-
            if metricsShowcase settings
              then selectiveFanout event hold deadline nodes
              else fanoutHead tracer deadline headId leader
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

-- | Exercise the production instrumentation with bounded faults on processes
-- owned by this local devnet. Every intervention unwinds before node cleanup.
showcaseMetrics :: (Text -> Text -> Text -> IO ()) -> IO () -> FilePath -> ChainBackendOptions -> NonEmpty HydraClient -> Dataset -> [UTxO] -> IO ()
showcaseMetrics event hold directory opts nodes Dataset{clientDatasets} sideUTxOs = do
  (firstDataset, unusedDataset) <- case clientDatasets of
    primary : secondary : _ -> pure (primary, secondary)
    _ -> fail "The metrics showcase requires at least two datasets"
  unusedChain <- maybe (fail "The second dataset has no transactions") pure $ nonEmpty $ txSequence unusedDataset
  let leader = head nodes
      clients = toList nodes
      peers = drop 1 clients
      invalid = makeSignedTransaction [] $ txBody $ head unusedChain
      submit :: HydraClient -> Tx -> IO ()
      submit client tx = send client $ input "NewTx" ["transaction" .= tx]
      waitInvalid tx = void $ waitMatch 60 leader $ \v -> do
        guard $ v ^? key "tag" == Just "TxInvalid"
        guard $ v ^? key "transaction" == Just (toJSON tx)
      evidence name = do
        createDirectoryIfMissing True (directory </> "evidence")
        getMetrics leader >>= BS.writeFile (directory </> "evidence" </> name <> ".prom")
  event "invalid-transaction" "started" "Submitting a transaction without its required signature"
  submit leader invalid
  waitInvalid invalid
  event "invalid-transaction" "observed" "The ledger rejected the unsigned transaction; the Head remains usable"
  evidence "invalid-transaction"
  hold

  let dependent = last unusedChain
      backlogTx = head unusedChain
  event "input-retries" "started" "Submitting a dependent transaction before its prerequisite transactions"
  submit leader dependent
  waitInvalid dependent
  event "input-retries" "observed" "The transaction waited for missing inputs and expired without being confirmed"
  evidence "input-retries"
  hold

  side <- case sideUTxOs of
    utxo : _ -> pure utxo
    [] -> fail "The metrics showcase requires a reserved incremental deposit"
  event "deposit" "started" "Depositing additional funds into the open Head"
  deposit <- signTx (paymentKey firstDataset) <$> requestCommitTx leader side
  runBackend opts $ submitTransaction deposit
  waitMetric leader "hydra_head_pending_deposits" (> 0)
  event "deposit" "applied" "The additional deposit is pending on chain"
  evidence "pending-deposit"
  forM_ clients $ \client -> void $ waitMatch 120 client $ \v -> do
    guard $ v ^? key "tag" == Just "CommitFinalized"
    guard $ v ^? key "depositTxId" == Just (toJSON $ txId deposit)
  event "deposit" "observed" "The incremental deposit has settled on every participant"
  hold

  (txIn, txOut) <- case UTxO.toList side of
    entry : _ -> pure entry
    [] -> fail "The reserved deposit is empty"
  decommit <- either (fail . show) pure $ mkSimpleTx (txIn, txOut) (txOutAddress txOut, txOutValue txOut) (paymentKey firstDataset)
  cardano <- getProcessID >>= (`findChildProcess` "cardano-node")
  event "l1-pause" "started" "Pausing the owned Cardano devnet while a decommit is submitted"
  withSuspendedProcess cardano $ do
    send leader $ input "Decommit" ["decommitTx" .= decommit]
    waitMetric leader "hydra_chain_operations_inflight" (> 0)
    event "l1-pause" "applied" "The decommit is approved and an L1 operation is waiting for the paused backend"
    replicateM_ 30 $ submit leader invalid
    waitMetric leader "hydra_node_input_queue_depth" (>= 20)
    event "input-pressure" "observed" "Inputs are queued behind the L1 call while the monitoring endpoint remains responsive"
    evidence "input-pressure"
    threadDelay 5
  event "l1-recovery" "applied" "The Cardano devnet resumes; the queued inputs can drain"
  forM_ clients $ \client -> void $ waitMatch 120 client $ \v ->
    guard $ v ^? key "tag" == Just "DecommitFinalized"
  waitMetric leader "hydra_chain_operations_inflight" (== 0)
  waitMetric leader "hydra_node_input_queue_depth" (== 0)
  event "l1-recovery" "observed" "The decommit finalized and the L1 operation returned"
  evidence "l1-recovery"
  hold

  event "quorum-loss" "started" "Pausing two etcd services while keeping all Hydra APIs and metrics running"
  case peers of
    [p1, p2] -> withSuspendedEtcd p1 $ withSuspendedEtcd p2 $ do
      replicateM_ 240 $ submit leader backlogTx
      waitMetric leader "hydra_head_broadcast_stalled" (== 1)
      event "quorum-loss" "applied" "The persisted broadcast queue filled and the outer hand-off reported a stall"
      submit leader invalid
      void $ waitMatch 30 leader $ \v ->
        guard $ v ^? key "tag" == Just "RejectedInputBecauseBroadcastStalled"
      event "broadcast-refusal" "observed" "A new transaction was explicitly refused while the broadcast hand-off was stalled"
      evidence "broadcast-stall"
      hold
    _ -> fail "The quorum-loss scenario requires exactly two followers"
  event "quorum-recovery" "applied" "Both etcd services resume and the quorum can acknowledge broadcasts again"
  forM_ clients $ \client -> void $ waitMatch 120 client (confirms backlogTx)
  waitMetric leader "hydra_head_broadcast_stalled" (== 0)
  waitMetric leader "hydra_network_persistent_queue_depth" (== 0)
  event "quorum-recovery" "observed" "The queued transaction confirmed on every participant and the broadcast stall cleared"
  evidence "quorum-recovery"
  hold
  event "network-health" "observed" "Successful etcd acknowledgements and SQLite batch completion are observable after recovery"
  hold

-- | Wait on the sum of matching live gauges, so zero requires every labelled
-- operation to have finished. Delays alone do not establish a fault's effect.
waitMetric :: HydraClient -> Text -> (Double -> Bool) -> IO ()
waitMetric client family predicate = do
  result <- Timeout.timeout 45_000_000 go
  unless (isJust result) $ fail $ "Metric did not reach the required state: " <> toString family
 where
  go = do
    rows <- Text.lines . decodeUtf8 <$> getMetrics client
    let samples =
          [ value
          | row <- rows
          , let fields = Text.words row
          , name : _ <- [fields]
          , name == family || (family <> "{") `Text.isPrefixOf` name
          , Just valueText <- [viaNonEmpty last fields]
          , Just value <- [readMaybe @Double $ toString valueText]
          ]
    if not (null samples) && predicate (sum samples) then pure () else threadDelay 0.2 >> go

-- | Deliberately leave part of a small Head undistributed so the fanout gauges
-- remain observable, then explicitly distribute the remainder.
selectiveFanout :: (Text -> Text -> Text -> IO ()) -> IO () -> UTCTime -> NonEmpty HydraClient -> IO Int
selectiveFanout event hold deadline clients = do
  let leader = head clients
  remaining <- diffUTCTime deadline <$> getCurrentTime
  threadDelay $ realToFrac $ max 0 remaining + 1
  utxo <- getSnapshotUTxO leader
  when (UTxO.size utxo < 2) $ fail "Selective fanout requires at least two outputs"
  let selection = UTxO.fromList $ take 1 $ UTxO.toList utxo
  send leader $ input "PartialFanout" ["utxoToFanout" .= selection]
  rest <- waitMatch 120 leader $ \v -> do
    guard $ v ^? key "tag" == Just "HeadPartiallyFannedOut"
    v ^? key "remainingUTxO" . _JSON
  event "partial-fanout" "observed" "One output has been distributed; the remaining outputs deliberately wait for the next selection"
  hold
  send leader $ input "PartialFanout" ["utxoToFanout" .= (rest :: UTxO)]
  waitMatch 120 leader $ \v -> do
    guard $ v ^? key "tag" == Just "HeadIsFinalized"
    finalized <- v ^? key "finalizedUTxO"
    pure $ length (finalized ^.. members)
