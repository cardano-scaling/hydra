{-# LANGUAGE CPP #-}
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
import Control.Tracer.JSON (LogEvent (..))
import Data.EventSource.SQLite (SQLiteLog (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Version (showVersion)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Hydra.API.APIServerLog qualified as API
import Hydra.API.ClientInput qualified as Client
import Hydra.API.ServerOutput qualified as Output
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))
import Hydra.Chain qualified as Chain
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
import Hydra.Network.Message (Message (ReqSn, ReqTx), NetworkEvent (..))
import Hydra.NetworkVersions (hydraNodeVersion)
import Hydra.Node (HydraNodeLog (..))
import Hydra.Node.Network qualified as Network
import Hydra.Node.State qualified as NodeState
import Hydra.Node.Stats qualified as Stats
import Hydra.Tx (HeadId, IsTx (..), Snapshot (..), getSnapshot)
import Network.HTTP.Types (status200, status404)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
#ifndef mingw32_HOST_OS
import System.Posix.Unistd (SysVar (ClockTick), getSysVar)
#endif
import System.Metrics.Prometheus.Metric.Counter qualified as Counter
import System.Metrics.Prometheus.Metric.Gauge qualified as Gauge
import System.Metrics.Prometheus.Metric.Histogram qualified as Histogram

withMonitoring :: IsTx tx => Maybe PortNumber -> Tracer IO (HydraLog tx) -> (Tracer IO (HydraLog tx) -> IO a) -> IO a
withMonitoring = withMonitoringForBackend "unknown"

withMonitoringForBackend :: IsTx tx => Text -> Maybe PortNumber -> Tracer IO (HydraLog tx) -> (Tracer IO (HydraLog tx) -> IO a) -> IO a
withMonitoringForBackend _ Nothing tracer action = action tracer
withMonitoringForBackend backend (Just port) tracer action = do
  (traceMetric, registry, refresh) <- prepareRegistry backend
  refreshRuntime <- registerRuntimeMetrics registry
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
-- not freeze the health workers or recursively log logging's own statistics.
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
  Network (Network.Etcd Etcd.WatchResponseReceived) -> True
  Network (Network.Etcd Etcd.NetworkConnectivity{}) -> True
  SQLite SQLiteWriteStarted -> True
  SQLite SQLiteWriteCompleted{} -> True
  SQLite SQLiteRotationCompleted{} -> True
  SQLite SQLiteReplayCompleted{} -> True
  APIServer API.APIL1Started{} -> True
  APIServer API.APIL1Finished{} -> True
  APIServer API.APIHTTPStarted{} -> True
  APIServer API.APIHTTPFinished -> True
  APIServer API.APIHTTPCompleted{} -> True
  SQLite SQLiteStats{} -> True
  SQLite SQLiteStatsFailed -> True
  Logging{} -> True
  _ -> False

-- | The refresh action expires unused timers even when no messages arrive.
prepareRegistry :: forall tx. IsTx tx => Text -> IO (HydraLog tx -> IO (), Registry.Registry, IO ())
prepareRegistry backend = do
  registry <- Registry.new
  Metrics{..} <- registerMetrics registry backend
  accounting <- MVar.newMVar Accounting.empty
  inputStarted <- newIORef Nothing
  snapshotInputStarted <- newIORef Nothing
  latestClientInput <- newIORef Nothing
  peerSet <- newIORef Set.empty
  loopStarted <- newIORef False
  connected <- newIORef False
  stalled <- newIORef False
  syncState <- newIORef False
  roundStarted <- newIORef Nothing
  backendSlot <- newIORef 0
  let set = flip Gauge.set
      tick = Counter.inc
      add = flip Counter.add
      observe = flip Histogram.observe
      bool gauge value = set gauge (if value then 1 else 0)
      timestamp gauge = getCurrentTime >>= set gauge . realToFrac . utcTimeToPOSIXSeconds
      mono = realToFrac . (\(Time t) -> t) <$> getMonotonicTime
      updateReady = do
        running <- readIORef loopStarted
        inSync <- readIORef syncState
        online <- readIORef connected
        blocked <- readIORef stalled
        bool ready (running && inSync && online && not blocked)
      oneHot :: Map Text Gauge.Gauge -> Text -> IO ()
      oneHot gauges value = forM_ (Map.toList gauges) $ \(key, gauge) -> bool gauge (key == value)
      resetRound = writeIORef roundStarted Nothing >> set snapshotStarted 0 >> set snapshotAge 0
      startRound =
        readIORef roundStarted >>= \case
          Just _ -> pure ()
          Nothing -> mono >>= writeIORef roundStarted . Just >> timestamp snapshotStarted
      refresh = do
        now <- mono
        MVar.modifyMVar_ accounting $ \state -> do
          let (removed, next) = Accounting.prune 100000 3600 now state
          add evictedTimings removed
          set trackedTransactions (fromIntegral $ Map.size $ Accounting.timers next)
          pure next
        start <- readIORef roundStarted
        set snapshotAge (maybe 0 (max 0 . (now -)) start)
      clearPending = MVar.modifyMVar_ accounting (pure . Accounting.clearPending) >> resetRound
      resetHead = do
        MVar.modifyMVar_ accounting $ \state -> pure (Accounting.clearPending state){Accounting.confirmed = Nothing}
        resetRound
        set decommitStarted 0
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
        resetRound
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
                observe txsPerSnapshot (fromIntegral $ length ids)
                traverse_ (observe snapshotConfirmationTime) duration
                traverse_ (observe txConfirmationTime) txDurations
                timestamp lastSnapshot
              when (isNothing $ Accounting.inFlight next) resetRound
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
        HeadFannedOut{} -> resetHead >> fanoutProgress
        HeadPartialFannedOut{} -> fanoutProgress
        LocalStateCleared{} -> clearPending
        Checkpoint{state} -> seed state
        SnapshotRequestDecided{} -> startRound
        SnapshotRequested{requestedSnapshot} -> do
          now <- mono
          readIORef snapshotInputStarted >>= traverse_ (observe snapshotRequestProcessing . max 0 . (now -))
          MVar.modifyMVar_ accounting $ pure . Accounting.startSnapshot (snapshotKey requestedSnapshot) (Just now) (txId <$> Hydra.Tx.confirmed requestedSnapshot)
          startRound
        SnapshotConfirmed{snapshot} -> void $ confirm snapshot
        TxInvalid{transaction} -> do
          MVar.modifyMVar_ accounting (pure . Accounting.forget (txId transaction))
          tick $ txOutcomes `at` "invalid"
        TransactionAppliedToLocalUTxO{} -> tick $ txOutcomes `at` "applied"
        DepositRecorded{} -> lifecycle "deposit_recorded"
        DepositActivated{} -> lifecycle "deposit_activated"
        DepositExpired{} -> lifecycle "deposit_expired"
        DepositRecovered{} -> lifecycle "deposit_recovered"
        CommitApproved{} -> lifecycle "commit_approved"
        CommitFinalized{} -> lifecycle "commit_finalized"
        DecommitRecorded{} -> lifecycle "decommit_recorded" >> timestamp decommitStarted
        DecommitApproved{} -> lifecycle "decommit_approved"
        DecommitInvalid{} -> lifecycle "decommit_invalid" >> tick (txOutcomes `at` "decommit_invalid")
        DecommitFinalized{} -> lifecycle "decommit_finalized" >> set decommitStarted 0
        _ -> pure ()
      lifecycle event = tick $ incrementalEvents `at` event
      fanoutProgress = tick fanoutChunks >> timestamp lastFanout
      effect = \case
        ClientEffect Output.RejectedInputBecauseBroadcastStalled{} -> tick inputsRefusedBroadcastStalled >> tick (txOutcomes `at` "refused_stalled")
        ClientEffect Output.RejectedInputBecauseUnsynced{} -> tick $ txOutcomes `at` "refused_unsynced"
        ClientEffect Output.CommandFailed{} -> tick $ txOutcomes `at` "command_failed"
        _ -> pure ()
      networkEvent event = tick $ networkEvents `at` event
      handle = \case
        EnteringMainloop -> writeIORef loopStarted True >> updateReady
        Node LoadedState{nodeState} -> seed nodeState
        Node BeginInput{inputId, input} -> do
          now <- mono
          writeIORef inputStarted $ Just (now, inputSource input)
          writeIORef snapshotInputStarted $ case input of
            NetworkInput _ ReceivedMessage{msg = ReqSn{}} -> Just now
            _ -> Nothing
          tick $ inputAttempts `at` inputSource input
          case input of
            NetworkInput _ ReceivedMessage{msg = ReqTx tx} -> MVar.modifyMVar_ accounting $ \state -> do
              let (fresh, removed, next) = Accounting.request 100000 3600 now inputId (txId tx) state
              when fresh $ tick headRequestedTx
              add evictedTimings removed
              pure next
            ClientInput command -> do
              fresh <- atomicModifyIORef' latestClientInput $ \previous ->
                (max previous (Just inputId), maybe True (inputId >) previous)
              when fresh $ tick $ commands `at` commandName command
            ChainInput Chain.Observation{observedTx} -> tick $ chainObservations `at` observationName observedTx
            _ -> pure ()
        Node EndInput{} -> do
          tick headInputs
          now <- mono
          readIORef inputStarted >>= traverse_ (\(start, source) -> observe (inputProcessing `at` source) (max 0 $ now - start))
          writeIORef inputStarted Nothing
          writeIORef snapshotInputStarted Nothing
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
          writeIORef stalled (isJust stalledReason)
          updateReady
        Node NodeHealth{stats} -> do
          writeIORef syncState (Stats.synchronized stats)
          bool synced (Stats.synchronized stats)
          oneHot phase (Stats.phase stats)
          set processedSlot (Stats.processedSlot stats)
          set queuedInputs (fromIntegral $ Stats.queuedInputs stats)
          set parkedInputs (fromIntegral $ Stats.parkedInputs stats)
          set delayedInputs (fromIntegral $ Stats.delayedInputs stats)
          set snapshotNumber (fromIntegral $ Stats.confirmedNumber stats)
          set snapshotVersion (fromIntegral $ Stats.confirmedVersion stats)
          bool snapshotPending (Stats.snapshotPending stats)
          set missingSignatures (fromIntegral $ Stats.missingSignatures stats)
          set outstandingTransactions (fromIntegral $ Stats.pendingTransactions stats)
          set (deposits `at` "inactive") (fromIntegral $ Stats.inactiveDeposits stats)
          set (deposits `at` "active") (fromIntegral $ Stats.activeDeposits stats)
          set (deposits `at` "expired") (fromIntegral $ Stats.expiredDeposits stats)
          set oldestDeposit (Stats.oldestDepositTimestamp stats)
          set depositDeadline (Stats.nextDepositDeadline stats)
          oneHot decommit (Stats.decommitState stats)
          when (Stats.decommitState stats == "none") $ set decommitStarted 0
          set fanoutRemaining (fromIntegral $ Stats.fanoutRemainingOutputs stats)
          oneHot fanoutMode (Stats.fanoutMode stats)
          set contestationDeadline (Stats.contestationDeadlineTimestamp stats)
          updateReady
        Node ChainPostStarted{operation} -> tick (chainOperations `at` operation) >> Gauge.inc chainInflight
        Node ChainPostFinished{operation, accepted, duration} -> do
          Gauge.dec chainInflight
          observe (chainDuration `at` operation) (realToFrac duration)
          unless accepted $ do
            tick $ chainFailures `at` operation
            when (operation `elem` ["fanout", "partial_fanout", "final_fanout"]) $ tick fanoutFailures
        Network (Network.Authenticate _) -> networkEvent "authentication_failure"
        Network (Network.Etcd message) -> case message of
          Etcd.PersistentQueueStats{depth, oldestAgeSeconds} -> set persistentDepth (fromIntegral depth) >> set persistentAge oldestAgeSeconds
          Etcd.BroadcastCompleted{messages, duration} -> timestamp lastPut >> add putMessages messages >> observe putBatch (fromIntegral messages) >> observe putLatency (realToFrac duration)
          Etcd.WatchResponseReceived -> timestamp lastWatch
          Etcd.NetworkConnectivity{connected = online} -> writeIORef connected online >> bool networkConnected online >> updateReady
          Etcd.Reconnecting -> networkEvent "reconnect"
          Etcd.BroadcastFailed{} -> networkEvent "broadcast_failure"
          Etcd.WatchFailed{} -> networkEvent "watch_failure"
          Etcd.WatchMessagesFallbackTo{} -> networkEvent "watch_compacted"
          Etcd.BroadcastDeduped{} -> networkEvent "deduplicated"
          Etcd.FailedToDecodeValue{} -> networkEvent "decode_value"
          Etcd.FailedToDecodeLog{} -> networkEvent "decode_log"
          Etcd.PersistentQueueStatFailed{} -> networkEvent "queue_stat_failure"
          Etcd.PersistentQueueLoadFailed{} -> networkEvent "queue_load_failure"
          Etcd.PersistentQueueWriteFailed{} -> networkEvent "queue_write_failure"
          Etcd.PersistentQueueDeleteFailed{} -> networkEvent "queue_delete_failure"
          Etcd.PersistentQueueFull -> networkEvent "queue_full"
          Etcd.LowLeaseTTL{} -> networkEvent "lease_low"
          Etcd.NoKeepAliveResponse -> networkEvent "lease_no_response"
          _ -> pure ()
        APIServer message -> case message of
          API.APIL1Started{operation} -> tick (chainOperations `at` operation) >> Gauge.inc chainInflight
          API.APIL1Finished{operation, successful, duration} -> do
            Gauge.dec chainInflight
            observe (chainDuration `at` operation) (realToFrac duration)
            unless successful $ tick (chainFailures `at` operation)
          API.APIHTTPStarted{} -> Gauge.inc httpInflight
          API.APIHTTPFinished -> Gauge.dec httpInflight
          API.APIHTTPCompleted{route, status, duration} -> tick (httpResponses `at` (route, statusClass status)) >> observe (httpDuration `at` route) (realToFrac duration)
          API.APIHTTPException{} -> tick $ apiErrors `at` "http_exception"
          API.APIOperationTimedOut{command} -> tick $ apiTimeouts `at` command
          API.NewAPIConnection -> Gauge.inc wsConnections
          API.APIConnectionClosed -> Gauge.dec wsConnections
          API.APIInvalidInput{} -> tick $ apiErrors `at` "invalid_input"
          API.APIConnectionError{} -> tick $ apiErrors `at` "connection"
          API.APIReturnedError{} -> tick $ apiErrors `at` "internal"
          _ -> pure ()
        DirectChain message -> case message of
          ChainLog.RolledForward{point} -> timestamp chainBackendTimestamp >> writeIORef backendSlot (pointSlot point)
          ChainLog.RolledBackward{point} -> do
            previous <- atomicModifyIORef' backendSlot (pointSlot point,)
            tick rollbacks
            observe rollbackDepth (max 0 $ previous - pointSlot point)
          ChainLog.PostedTx{} -> tick $ chainSubmissions `at` "accepted"
          ChainLog.PostingFailed{} -> tick $ chainSubmissions `at` "failed"
          ChainLog.BlockfrostTransientError{} -> tick blockfrostRetries
          ChainLog.BlockfrostRateLimit -> tick blockfrostRateLimits
          ChainLog.PartialFanoutFailed{} -> tick fanoutEvaluationFailures
          _ -> pure ()
        SQLite message -> case message of
          SQLiteStats{queueDepth, databaseBytes, walBytes} -> set sqliteQueue (fromIntegral queueDepth) >> set sqliteDatabase (fromIntegral databaseBytes) >> set sqliteWal (fromIntegral walBytes)
          SQLiteStatsFailed -> tick sqliteStatsFailures
          SQLiteWriteStarted -> set sqliteInflight 1
          SQLiteWriteCompleted{eventsWritten, writeSeconds, oldestEventSeconds} -> set sqliteInflight 0 >> add sqliteEvents eventsWritten >> observe sqliteDuration writeSeconds >> observe sqliteLatency oldestEventSeconds >> observe sqliteBatch (fromIntegral eventsWritten)
          SQLiteWriteFailed -> set sqliteInflight 0 >> tick sqliteFailures
          SQLiteRotationCompleted{durationSeconds} -> observe sqliteRotation durationSeconds
          SQLiteReplayCompleted{durationSeconds} -> observe sqliteReplay durationSeconds
          _ -> pure ()
        Logging message -> case message of
          LogQueueDepth{depth} -> set logQueue (fromIntegral depth)
          LogBatchWritten{entries, seconds} -> add logEntries entries >> observe logWriteDuration seconds >> observe logBatch (fromIntegral entries)
          LogWriteFailed{unconfirmedEntries} -> tick logWriteFailures >> add logUnconfirmed unconfirmedEntries
          LogUnencodable -> tick logEncodingFailures
          LogEnqueueWait{seconds} -> observe logEnqueueWait seconds
        _ -> pure ()
  pure (handle, registry, refresh)

snapshotKey :: Snapshot tx -> Accounting.SnapshotKey Hydra.Tx.HeadId
snapshotKey Snapshot{headId, version, number} = Accounting.SnapshotKey headId (fromIntegral version) (fromIntegral number)

inputSource :: Input tx -> Text
inputSource = \case ClientInput{} -> "client"; NetworkInput{} -> "network"; ChainInput{} -> "chain"

commandName :: Client.ClientInput tx -> Text
commandName = \case
  Client.Init -> "init"
  Client.NewTx{} -> "new_tx"
  Client.Recover{} -> "recover"
  Client.Decommit{} -> "decommit"
  Client.Close -> "close"
  Client.SafeClose -> "safe_close"
  Client.Contest -> "contest"
  Client.Fanout -> "fanout"
  Client.PartialFanout{} -> "partial_fanout"
  Client.SideLoadSnapshot{} -> "side_load"

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

observationName :: Chain.OnChainTx tx -> Text
observationName = \case
  Chain.OnInitTx{} -> "init"
  Chain.OnDepositTx{} -> "deposit"
  Chain.OnRecoverTx{} -> "recover"
  Chain.OnIncrementTx{} -> "increment"
  Chain.OnDecrementTx{} -> "decrement"
  Chain.OnCloseTx{} -> "close"
  Chain.OnContestTx{} -> "contest"
  Chain.OnFanoutTx{} -> "fanout"
  Chain.OnPartialFanoutTx{} -> "partial_fanout"

pointSlot :: ChainPoint -> Double
pointSlot = \case ChainPointAtGenesis -> 0; ChainPoint (SlotNo slot) _ -> fromIntegral slot

statusClass :: Int -> Text
statusClass status
  | status >= 100 && status < 600 = show (status `div` 100) <> "xx"
  | otherwise = "other"

registerRuntimeMetrics :: Registry.Registry -> IO (IO ())
registerRuntimeMetrics registry = do
  build <- Registry.gauge registry "hydra_build_info" "Build version of the running node." [("version", toText $ showVersion hydraNodeVersion)]
  Gauge.set 1 build
  cpu <- Registry.sampledCounter registry "hydra_process_cpu_seconds_total" "Total process user and system CPU time."
  started <- Registry.gauge registry "hydra_monitoring_start_time_seconds" "Wall-clock time this process started its monitoring endpoint." []
  getCurrentTime >>= flip Gauge.set started . realToFrac . utcTimeToPOSIXSeconds
  hasProc <- doesFileExist "/proc/self/status"
  rss <- if hasProc then Just <$> Registry.gauge registry "hydra_process_resident_memory_bytes" "Current resident memory reported by the operating system." [] else pure Nothing
  registerProcessStart registry hasProc
  rts <- registerRtsMetrics registry
  pure $ do
    getCPUTime >>= flip Gauge.set cpu . (/ 1.0e12) . fromIntegral
    forM_ rss $ \gauge -> do
      status <- decodeUtf8 <$> readFileBS "/proc/self/status"
      forM_ (find (Text.isPrefixOf "VmRSS:") $ Text.lines status) $ \line ->
        case Text.words line of
          [_, value, _] -> traverse_ (flip Gauge.set gauge . (* 1024)) (readMaybe $ toString value)
          _ -> pure ()
    rts

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

registerProcessStart :: Registry.Registry -> Bool -> IO ()
#ifndef mingw32_HOST_OS
registerProcessStart registry hasProc = when hasProc $ do
  ticks <- getSysVar ClockTick
  processStat <- decodeUtf8 <$> readFileBS "/proc/self/stat"
  uptime <- decodeUtf8 <$> readFileBS "/proc/uptime"
  now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime
  -- The comm field may contain spaces and parentheses; fields after its
  -- closing parenthesis start at field 3. starttime is field 22.
  let startTicks = listToMaybe (drop 19 $ Text.words $ snd $ Text.breakOnEnd ")" processStat) >>= readMaybe . toString
      bootAge = listToMaybe (Text.words uptime) >>= readMaybe . toString
  forM_ ((,) <$> startTicks <*> bootAge) $ \(start, age) -> do
    processStart <- Registry.gauge registry "hydra_process_start_time_seconds" "Operating-system process start time as Unix seconds; Linux only." []
    Gauge.set (now - age + start / fromIntegral ticks) processStart
#else
registerProcessStart _ _ = pure ()
#endif
