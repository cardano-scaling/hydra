{-# LANGUAGE RecordWildCards #-}

-- | Prometheus exposition and trace-driven accounting. Counters describe this
-- process; state gauges are sampled independently of the processing queue.
module Hydra.Logging.Monitoring (
  withMonitoring,
  withMonitoringForBackend,
  prepareRegistry,
  isMetricSample,
) where

import Hydra.Prelude hiding (bool, handle, state)

import Control.Concurrent.Class.MonadSTM (putTMVar, takeTMVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Class.MonadAsync (link)
import Control.Tracer (Tracer (Tracer), emit, traceWith)
import Data.EventSource.SQLite (SQLiteLog (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Hydra.API.APIServerLog qualified as API
import Hydra.API.ClientInput qualified as Client
import Hydra.API.ServerOutput qualified as Output
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))
import Hydra.Chain.Direct.Handlers qualified as ChainLog
import Hydra.HeadLogic (Input (..))
import Hydra.HeadLogic.Outcome (Effect (ClientEffect), Outcome (..), StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.State qualified as Head
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring.Accounting qualified as Accounting
import Hydra.Logging.Monitoring.Metrics (Metrics (..), at, registerMetrics)
import Hydra.Logging.Monitoring.Registry qualified as Registry
import Hydra.Network (PortNumber, StallReason (..))
import Hydra.Network.Etcd qualified as Etcd
import Hydra.Network.Message (Message (ReqTx), NetworkEvent (..))
import Hydra.Node (HydraNodeLog (..))
import Hydra.Node.Network qualified as Network
import Hydra.Node.State qualified as NodeState
import Hydra.Node.Stats qualified as Stats
import Hydra.Tx (HeadId, IsTx (..), Snapshot (..), getSnapshot)
import Network.HTTP.Types (status200, status404)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import System.Metrics.Prometheus.Metric.Counter qualified as Counter
import System.Metrics.Prometheus.Metric.Gauge qualified as Gauge
import System.Metrics.Prometheus.Metric.Histogram qualified as Histogram

withMonitoring :: IsTx tx => Maybe PortNumber -> Tracer IO (HydraLog tx) -> (Tracer IO (HydraLog tx) -> IO a) -> IO a
withMonitoring = withMonitoringForBackend "unknown"

withMonitoringForBackend :: IsTx tx => Text -> Maybe PortNumber -> Tracer IO (HydraLog tx) -> (Tracer IO (HydraLog tx) -> IO a) -> IO a
withMonitoringForBackend _ Nothing tracer action = action tracer
withMonitoringForBackend backend (Just port) tracer action = do
  (traceMetric, registry, refresh) <- prepareRegistry backend
  refreshRuntime <- registerRtsMetrics registry
  scrapeLock <- MVar.newMVar ()
  listening <- newLabelledEmptyTMVarIO "monitoring-listening"
  let settings = setPort (fromIntegral port) $ setBeforeMainLoop (atomically $ putTMVar listening ()) defaultSettings
      app request respond
        | pathInfo request == ["metrics"] = do
            body <- MVar.withMVar scrapeLock $ \_ -> refresh >> refreshRuntime >> Registry.sample registry
            respond $ responseLBS status200 [("Content-Type", "text/plain; version=0.0.4; charset=utf-8")] body
        | otherwise = respond $ responseLBS status404 [] "Not found"
      wrapped = Tracer . emit $ \msg -> traceMetric msg >> traceWith tracer msg
  withAsyncLabelled ("monitoring-server", runSettings settings app) $ \server -> do
    -- Bind errors and later server failures are fatal, and callers may scrape
    -- as soon as their continuation starts.
    link server
    atomically $ takeTMVar listening
    action wrapped

-- | Samples bypass the JSON queue in production, so a blocked log output does
-- not freeze the health workers.
isMetricSample :: HydraLog tx -> Bool
isMetricSample = \case
  Node InputDequeued{} -> True
  Node ChainPostStarted{} -> True
  Node ChainPostFinished{} -> True
  Node NodeHealth{} -> True
  Node BroadcastHealth{} -> True
  Node BroadcastBacklog{} -> True
  Network (Network.Etcd Etcd.PersistentQueueStats{}) -> True
  Network (Network.Etcd Etcd.PersistentQueueStatFailed{}) -> True
  Network (Network.Etcd Etcd.BroadcastCompleted{}) -> True
  Network (Network.Etcd Etcd.NetworkConnectivity{}) -> True
  SQLite SQLiteWriteStarted -> True
  SQLite SQLiteWriteCompleted{} -> True
  SQLite SQLiteWriteFailed -> True
  APIServer API.APIL1Started{} -> True
  APIServer API.APIL1Finished{} -> True
  SQLite SQLiteStats{} -> True
  _ -> False

-- | The refresh action expires unused timers even when no messages arrive.
prepareRegistry :: forall tx. IsTx tx => Text -> IO (HydraLog tx -> IO (), Registry.Registry, IO ())
prepareRegistry backend = do
  registry <- Registry.new
  Metrics{..} <- registerMetrics registry backend
  accounting <- MVar.newMVar Accounting.empty
  inputStarted <- newIORef Nothing
  peerSet <- newIORef Set.empty
  backendSlot <- newIORef 0
  let set = flip Gauge.set
      tick = Counter.inc
      add = flip Counter.add
      observe = flip Histogram.observe
      bool gauge value = set gauge (if value then 1 else 0)
      timestamp gauge = getCurrentTime >>= set gauge . realToFrac . utcTimeToPOSIXSeconds
      mono = realToFrac . (\(Time t) -> t) <$> getMonotonicTime
      oneHot :: Map Text Gauge.Gauge -> Text -> IO ()
      oneHot gauges value = forM_ (Map.toList gauges) $ \(key, gauge) -> bool gauge (key == value)
      refresh = do
        now <- mono
        MVar.modifyMVar_ accounting $ \state -> do
          let (removed, next) = Accounting.prune 100000 3600 now state
          add evictedTimings removed
          pure next
      clearPending = MVar.modifyMVar_ accounting (pure . Accounting.clearPending)
      resetHead =
        MVar.modifyMVar_ accounting $ \state -> pure (Accounting.clearPending state){Accounting.confirmed = Nothing}
      seed ns = do
        let (confirmed, pending) = case NodeState.headState ns of
              Head.Open Head.OpenState{coordinatedHeadState = Head.CoordinatedHeadState{confirmedSnapshot, seenSnapshot}} ->
                (Just $ getSnapshot confirmedSnapshot, case seenSnapshot of Head.SeenSnapshot{snapshot} -> Just snapshot; _ -> Nothing)
              Head.Closed Head.ClosedState{confirmedSnapshot} -> (Just $ getSnapshot confirmedSnapshot, Nothing)
              Head.FanoutProgress Head.PartialFanoutState{confirmedSnapshot} -> (Just $ getSnapshot confirmedSnapshot, Nothing)
              Head.Idle{} -> (Nothing, Nothing)
        MVar.modifyMVar_ accounting $ \state ->
          pure $
            (Accounting.clearPending state)
              { Accounting.confirmed = snapshotKey <$> confirmed
              , Accounting.inFlight = (\snapshot -> (snapshotKey snapshot, Nothing, txId <$> Hydra.Tx.confirmed snapshot)) <$> pending
              }
      confirm supplied = do
        now <- mono
        MVar.modifyMVar accounting $ \state -> do
          let candidate = case supplied of
                Just snapshot -> Just (snapshotKey snapshot, txId <$> Hydra.Tx.confirmed snapshot, "adopted")
                Nothing -> (\(key, _, ids) -> (key, ids, "signed")) <$> Accounting.inFlight state
          case candidate of
            Nothing -> pure (state, False)
            Just (key, ids, source) -> do
              let (newNumber, duration, txDurations, next) = Accounting.confirmSnapshot now key ids state
              when newNumber $ do
                add headConfirmedTx (length ids)
                tick $ snapshots `at` source
                traverse_ (observe snapshotConfirmationTime) duration
                traverse_ (observe txConfirmationTime) txDurations
              pure (next, newNumber)
      stateChange = \case
        PeerConnected{peer} -> do
          peers <- atomicModifyIORef' peerSet $ \peers -> let next = Set.insert peer peers in (next, next)
          set peersConnected (fromIntegral $ Set.size peers)
        PeerDisconnected{peer} -> do
          peers <- atomicModifyIORef' peerSet $ \peers -> let next = Set.delete peer peers in (next, next)
          set peersConnected (fromIntegral $ Set.size peers)
        NetworkDisconnected -> writeIORef peerSet mempty >> set peersConnected 0
        TickObserved{chainTime} -> do
          now <- getCurrentTime
          set chainDriftSeconds (realToFrac $ now `diffUTCTime` chainTime)
          timestamp chainLastBlockTimestampSeconds
        HeadOpened{headId} -> do
          resetHead
          MVar.modifyMVar_ accounting $ \state -> pure state{Accounting.confirmed = Just $ Accounting.SnapshotKey headId 0 0}
        HeadClosed{} -> clearPending
        HeadFannedOut{} -> resetHead
        LocalStateCleared{} -> clearPending
        Checkpoint{state} -> seed state
        SnapshotRequested{requestedSnapshot} -> do
          now <- mono
          MVar.modifyMVar_ accounting $ pure . Accounting.startSnapshot (snapshotKey requestedSnapshot) (Just now) (txId <$> Hydra.Tx.confirmed requestedSnapshot)
        SnapshotConfirmed{snapshot} -> void $ confirm snapshot
        TxInvalid{transaction} -> do
          MVar.modifyMVar_ accounting (pure . Accounting.forget (txId transaction))
          tick $ txOutcomes `at` "invalid"
        TransactionAppliedToLocalUTxO{} -> tick $ txOutcomes `at` "applied"
        DecommitInvalid{} -> tick (txOutcomes `at` "decommit_invalid")
        _ -> pure ()
      transactionRefused :: Text -> Client.ClientInput tx -> IO ()
      transactionRefused outcome input = when (isTransactionInput input) $ tick (txOutcomes `at` outcome)
      effect = \case
        ClientEffect Output.RejectedInputBecauseBroadcastStalled{clientInput} -> tick inputsRefusedBroadcastStalled >> transactionRefused "refused_stalled" clientInput
        ClientEffect Output.RejectedInputBecauseUnsynced{clientInput} -> transactionRefused "refused_unsynced" clientInput
        ClientEffect Output.CommandFailed{clientInput} -> transactionRefused "rejected_state" clientInput
        _ -> pure ()
      networkEvent event = tick $ networkEvents `at` event
      handle = \case
        Node LoadedState{nodeState} -> seed nodeState
        Node BeginInput{inputId, input} -> do
          now <- mono
          writeIORef inputStarted $ Just (now, inputSource input)
          case input of
            NetworkInput _ ReceivedMessage{msg = ReqTx tx} -> MVar.modifyMVar_ accounting $ \state -> do
              let (fresh, removed, next) = Accounting.request 100000 3600 now inputId (txId tx) state
              when fresh $ tick headRequestedTx
              add evictedTimings removed
              pure next
            _ -> pure ()
        Node EndInput{} -> do
          tick headInputs
          now <- mono
          readIORef inputStarted >>= traverse_ (\(start, source) -> observe (inputProcessing `at` source) (max 0 $ now - start))
          writeIORef inputStarted Nothing
        Node InputDequeued{queueWait} -> observe inputQueueWait (realToFrac queueWait)
        Node LogicOutcome{outcome} -> case outcome of
          Continue{stateChanges, effects} -> traverse_ stateChange stateChanges >> traverse_ effect effects
          Wait{reason, stateChanges} -> tick (inputRetries `at` waitReason reason) >> traverse_ stateChange stateChanges
          Error{} -> pure ()
        Node DroppedFromQueue{input} -> do
          tick $ inputDrops `at` inputSource input
          case input of
            NetworkInput _ ReceivedMessage{msg = ReqTx tx} -> MVar.modifyMVar_ accounting (pure . Accounting.forget (txId tx))
            _ -> pure ()
        Node BroadcastBacklog{pendingBroadcasts = pending, noProgressSeconds} -> set pendingBroadcasts (fromIntegral pending) >> set broadcastNoProgressSeconds (realToFrac noProgressSeconds)
        Node BroadcastHealth{stalledReason} -> do
          bool broadcastStalled (isJust stalledReason)
          bool broadcastStalledNoProgress (stalledReason == Just NoProgress)
          bool broadcastStalledBacklogFull (stalledReason == Just BacklogFull)
        Node NodeHealth{stats} -> do
          bool synced (Stats.synchronized stats)
          oneHot phase (Stats.phase stats)
          set queuedInputs (fromIntegral $ Stats.queuedInputs stats)
          set parkedInputs (fromIntegral $ Stats.parkedInputs stats)
          set snapshotNumber (fromIntegral $ Stats.confirmedNumber stats)
          bool snapshotPending (Stats.snapshotPending stats)
          set missingSignatures (fromIntegral $ Stats.missingSignatures stats)
          set outstandingTransactions (fromIntegral $ Stats.pendingTransactions stats)
          set (deposits `at` "inactive") (fromIntegral $ Stats.inactiveDeposits stats)
          set (deposits `at` "active") (fromIntegral $ Stats.activeDeposits stats)
          set (deposits `at` "expired") (fromIntegral $ Stats.expiredDeposits stats)
          oneHot decommit (Stats.decommitState stats)
          set fanoutRemaining (fromIntegral $ Stats.fanoutRemainingOutputs stats)
          oneHot fanoutMode (Stats.fanoutMode stats)
        Node ChainPostStarted{operation} -> Gauge.inc (chainInflight `at` operation)
        Node ChainPostFinished{operation, accepted, duration} -> do
          Gauge.dec (chainInflight `at` operation)
          observe (chainDuration `at` operation) (realToFrac duration)
          unless accepted $ tick $ chainFailures `at` operation
        Network (Network.Authenticate _) -> networkEvent "authentication_failure"
        Network (Network.Etcd message) -> case message of
          Etcd.PersistentQueueStats{depth, oldestAgeSeconds} -> set persistentDepth (fromIntegral depth) >> set persistentAge oldestAgeSeconds
          Etcd.BroadcastCompleted{duration} -> observe putLatency (realToFrac duration)
          Etcd.NetworkConnectivity{connected = online} -> bool networkConnected online
          Etcd.Reconnecting -> networkEvent "reconnect"
          Etcd.BroadcastFailed{} -> networkEvent "broadcast_failure"
          Etcd.WatchFailed{} -> networkEvent "watch_failure"
          Etcd.FailedToDecodeValue{} -> networkEvent "decode_failure"
          Etcd.FailedToDecodeLog{} -> networkEvent "decode_failure"
          Etcd.PersistentQueueStatFailed{} -> networkEvent "queue_stat_failure"
          Etcd.PersistentQueueLoadFailed{} -> networkEvent "queue_storage_failure"
          Etcd.PersistentQueueWriteFailed{} -> networkEvent "queue_storage_failure"
          Etcd.PersistentQueueDeleteFailed{} -> networkEvent "queue_storage_failure"
          Etcd.PersistentQueueFull -> networkEvent "queue_full"
          Etcd.LowLeaseTTL{} -> networkEvent "lease_warning"
          Etcd.NoKeepAliveResponse -> networkEvent "lease_warning"
          _ -> pure ()
        APIServer message -> case message of
          API.APIL1Started{operation} -> Gauge.inc (chainInflight `at` operation)
          API.APIL1Finished{operation, successful, duration} -> do
            Gauge.dec (chainInflight `at` operation)
            observe (chainDuration `at` operation) (realToFrac duration)
            unless successful $ tick (chainFailures `at` operation)
          _ -> pure ()
        DirectChain message -> case message of
          ChainLog.RolledForward{point} -> timestamp chainBackendTimestamp >> writeIORef backendSlot (pointSlot point)
          ChainLog.RolledBackward{point} -> do
            previous <- atomicModifyIORef' backendSlot (pointSlot point,)
            observe rollbackDepth (max 0 $ previous - pointSlot point)
          ChainLog.BlockfrostTransientError{} -> tick blockfrostRetries
          ChainLog.BlockfrostRateLimit -> tick blockfrostRateLimits
          _ -> pure ()
        SQLite message -> case message of
          SQLiteStats{queueDepth} -> set sqliteQueue (fromIntegral queueDepth)
          SQLiteWriteStarted -> set sqliteInflight 1
          SQLiteWriteCompleted{writeSeconds} -> set sqliteInflight 0 >> observe sqliteDuration writeSeconds
          SQLiteWriteFailed -> set sqliteInflight 0
          _ -> pure ()
        _ -> pure ()
  pure (handle, registry, refresh)

snapshotKey :: Snapshot tx -> Accounting.SnapshotKey Hydra.Tx.HeadId
snapshotKey Snapshot{headId, version, number} = Accounting.SnapshotKey headId (fromIntegral version) (fromIntegral number)

-- | Only inputs carrying a transaction contribute to transaction refusals.
isTransactionInput :: Client.ClientInput tx -> Bool
isTransactionInput = \case
  Client.NewTx{} -> True
  Client.Decommit{} -> True
  _ -> False

inputSource :: Input tx -> Text
inputSource = \case ClientInput{} -> "client"; NetworkInput{} -> "network"; ChainInput{} -> "chain"

waitReason :: WaitReason tx -> Text
waitReason = \case
  WaitOnNotApplicableTx{} -> "transaction"
  WaitOnSnapshotNumber{} -> "snapshot_number"
  WaitOnSnapshotVersion{} -> "snapshot_version"
  WaitOnSeenSnapshot -> "seen_snapshot"
  WaitOnTxs{} -> "transactions"
  WaitOnContestationDeadline -> "contestation_deadline"
  WaitOnNotApplicableDecommitTx{} -> "decommit"
  WaitOnUnresolvedCommit{} -> "unresolved_commit"
  WaitOnUnresolvedDecommit{} -> "unresolved_decommit"
  WaitOnDepositObserved{} -> "deposit_observation"
  WaitOnDepositActivation{} -> "deposit_activation"
  WaitOnNodeInSync{} -> "sync"

pointSlot :: ChainPoint -> Double
pointSlot = \case ChainPointAtGenesis -> 0; ChainPoint (SlotNo slot) _ -> fromIntegral slot

registerRtsMetrics :: Registry.Registry -> IO (IO ())
registerRtsMetrics registry = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then pure (pure ())
    else do
      allocated <- counter "hydra_rts_allocated_bytes" "Cumulative bytes allocated by the GHC runtime."
      mutatorCpu <- counter "hydra_rts_mutator_cpu_seconds" "Cumulative GHC mutator CPU seconds."
      gcCpu <- counter "hydra_rts_gc_cpu_seconds" "Cumulative GHC garbage collector CPU seconds."
      maxLive <- Registry.gauge registry "hydra_rts_max_live_bytes" "Maximum live heap bytes observed at a major GC since process start." []
      cumulativeLive <- counter "hydra_rts_cumulative_live_bytes" "Sum of live bytes measured at each major GC; divide its rate by the major GC rate for mean live heap."
      majorGcs <- counter "hydra_rts_major_gcs" "Cumulative major GHC garbage collections."
      pure $ do
        stats <- getRTSStats
        Gauge.set (fromIntegral $ allocated_bytes stats) allocated
        Gauge.set (fromIntegral (mutator_cpu_ns stats) / 1.0e9) mutatorCpu
        Gauge.set (fromIntegral (gc_cpu_ns stats) / 1.0e9) gcCpu
        Gauge.set (fromIntegral $ max_live_bytes stats) maxLive
        Gauge.set (fromIntegral $ cumulative_live_bytes stats) cumulativeLive
        Gauge.set (fromIntegral $ major_gcs stats) majorGcs
 where
  counter legacy help = do
    handle <- Registry.sampledCounter registry (legacy <> "_total") help
    Registry.alias registry (legacy <> "_total") legacy 1
    pure handle
