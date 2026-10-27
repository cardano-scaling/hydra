{-# LANGUAGE RecordWildCards #-}

-- | Typed handles and bounded label domains for the public metrics interface.
module Hydra.Logging.Monitoring.Metrics where

import Data.Map.Strict qualified as Map
import Hydra.Logging.Monitoring.Registry qualified as Registry
import Hydra.Prelude
import System.Metrics.Prometheus.Metric.Counter (Counter)
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

data Metrics = Metrics
  { headInputs :: Counter
  , headRequestedTx :: Counter
  , headConfirmedTx :: Counter
  , txConfirmationTime :: Histogram
  , snapshotConfirmationTime :: Histogram
  , snapshotRequestProcessing :: Histogram
  , snapshots :: Map Text Counter
  , txsPerSnapshot :: Histogram
  , trackedTransactions :: Gauge
  , evictedTimings :: Counter
  , peersConnected :: Gauge
  , chainDriftSeconds :: Gauge
  , chainLastBlockTimestampSeconds :: Gauge
  , chainBackendTimestamp :: Gauge
  , broadcastStalled :: Gauge
  , broadcastStalledNoProgress :: Gauge
  , broadcastStalledBacklogFull :: Gauge
  , pendingBroadcasts :: Gauge
  , broadcastNoProgressSeconds :: Gauge
  , inputsRefusedBroadcastStalled :: Counter
  , inputAttempts :: Map Text Counter
  , inputRetries :: Map Text Counter
  , inputDrops :: Map Text Counter
  , inputProcessing :: Map Text Histogram
  , inputQueueWait :: Histogram
  , queuedInputs :: Gauge
  , parkedInputs :: Gauge
  , delayedInputs :: Gauge
  , ready :: Gauge
  , synced :: Gauge
  , phase :: Map Text Gauge
  , processedSlot :: Gauge
  , networkConnected :: Gauge
  , snapshotNumber :: Gauge
  , snapshotVersion :: Gauge
  , snapshotPending :: Gauge
  , snapshotStarted :: Gauge
  , snapshotAge :: Gauge
  , missingSignatures :: Gauge
  , outstandingTransactions :: Gauge
  , lastSnapshot :: Gauge
  , txOutcomes :: Map Text Counter
  , commands :: Map Text Counter
  , persistentDepth :: Gauge
  , persistentAge :: Gauge
  , lastPut :: Gauge
  , lastWatch :: Gauge
  , putLatency :: Histogram
  , putMessages :: Counter
  , putBatch :: Histogram
  , networkEvents :: Map Text Counter
  , chainOperations :: Map Text Counter
  , chainFailures :: Map Text Counter
  , chainDuration :: Map Text Histogram
  , chainInflight :: Gauge
  , chainSubmissions :: Map Text Counter
  , chainObservations :: Map Text Counter
  , rollbacks :: Counter
  , rollbackDepth :: Histogram
  , blockfrostRetries :: Counter
  , blockfrostRateLimits :: Counter
  , deposits :: Map Text Gauge
  , oldestDeposit :: Gauge
  , depositDeadline :: Gauge
  , decommit :: Map Text Gauge
  , decommitStarted :: Gauge
  , incrementalEvents :: Map Text Counter
  , fanoutRemaining :: Gauge
  , fanoutMode :: Map Text Gauge
  , fanoutChunks :: Counter
  , fanoutFailures :: Counter
  , fanoutEvaluationFailures :: Counter
  , lastFanout :: Gauge
  , contestationDeadline :: Gauge
  , httpInflight :: Gauge
  , httpResponses :: Map (Text, Text) Counter
  , httpDuration :: Map Text Histogram
  , apiTimeouts :: Map Text Counter
  , wsConnections :: Gauge
  , apiErrors :: Map Text Counter
  , sqliteQueue :: Gauge
  , sqliteInflight :: Gauge
  , sqliteEvents :: Counter
  , sqliteFailures :: Counter
  , sqliteStatsFailures :: Counter
  , sqliteDuration :: Histogram
  , sqliteLatency :: Histogram
  , sqliteBatch :: Histogram
  , sqliteRotation :: Histogram
  , sqliteReplay :: Histogram
  , sqliteDatabase :: Gauge
  , sqliteWal :: Gauge
  , logQueue :: Gauge
  , logEntries :: Counter
  , logUnconfirmed :: Counter
  , logEncodingFailures :: Counter
  , logWriteFailures :: Counter
  , logWriteDuration :: Histogram
  , logEnqueueWait :: Histogram
  , logBatch :: Histogram
  }

registerMetrics :: Registry.Registry -> Text -> IO Metrics
registerMetrics registry backend = do
  headInputs <- Registry.counter registry "hydra_head_inputs_total" "Completed input processing attempts, including retries." []
  headRequestedTx <- Registry.counter registry "hydra_head_requested_tx_total" "ReqTx network messages first processed by this node; retries excluded and external duplicate messages included." []
  headConfirmedTx <- Registry.counter registry "hydra_head_confirmed_tx_total" "Transactions in newly confirmed or adopted snapshot numbers observed in this process; repeated adoption excluded." []
  txConfirmationTime <- Registry.histogram registry "hydra_head_tx_confirmation_seconds" "Time from first local processing of ReqTx to confirmation; excludes queue admission and unavailable timings." latencyBuckets []
  snapshotRequestProcessing <- Registry.histogram registry "hydra_head_snapshot_request_processing_seconds" "BeginInput ReqSn to its accepted SnapshotRequested outcome; excludes queue wait and subsequent effects." latencyBuckets []
  snapshotConfirmationTime <- Registry.histogram registry "hydra_head_snapshot_confirmation_seconds" "Time from accepted SnapshotRequested after validation to confirmation of the same head, version and number." latencyBuckets []
  snapshots <- vector ["signed", "adopted"] $ \value -> Registry.counter registry "hydra_head_snapshots_total" "New snapshot numbers observed, split by signed confirmation or adoption." [("source", value)]
  txsPerSnapshot <- Registry.histogram registry "hydra_head_snapshot_transactions" "Transaction count in each newly confirmed or adopted snapshot." [0, 1, 10, 50, 100, 500, 1000, 5000] []
  trackedTransactions <- Registry.gauge registry "hydra_monitoring_tracked_transactions" "Transaction timing entries retained by monitoring, capped at 100000 and expired after one hour." []
  evictedTimings <- Registry.counter registry "hydra_monitoring_timing_evictions_total" "Transaction timings discarded due to age or capacity; confirmation counters are unaffected." []
  peersConnected <- Registry.gauge registry "hydra_head_peers_connected" "Distinct connected peers observed by the processing loop." []
  chainDriftSeconds <- Registry.gauge registry "hydra_chain_drift_seconds" "Wall clock minus chain time at the latest processed tick; may be negative with clock skew." []
  chainLastBlockTimestampSeconds <- Registry.gauge registry "hydra_chain_last_block_timestamp_seconds" "Wall-clock time of the last tick processed by the main loop; zero before the first tick." []
  chainBackendTimestamp <- Registry.gauge registry "hydra_chain_backend_last_block_timestamp_seconds" "Wall-clock time of the last raw backend roll-forward; independent of main-loop progress." []
  broadcastStalled <- Registry.gauge registry "hydra_head_broadcast_stalled" "Live outbox stall predicate; independent of queued client notifications." []
  broadcastStalledNoProgress <- Registry.gauge registry "hydra_head_broadcast_stalled_no_progress" "Live outbox stall caused by no progress." []
  broadcastStalledBacklogFull <- Registry.gauge registry "hydra_head_broadcast_stalled_backlog_full" "Live outbox stall caused by the backlog limit." []
  pendingBroadcasts <- Registry.gauge registry "hydra_head_pending_broadcasts" "Messages in the outer network hand-off; excludes the persistent network queue." []
  broadcastNoProgressSeconds <- Registry.gauge registry "hydra_head_broadcast_no_progress_seconds" "Seconds without outer hand-off progress while it has pending messages." []
  inputsRefusedBroadcastStalled <- Registry.counter registry "hydra_head_inputs_refused_broadcast_stalled_total" "Client inputs refused because the outbound hand-off is stalled." []
  inputAttempts <- vector inputSources $ \value -> Registry.counter registry "hydra_node_input_attempts_total" "Input processing attempts by source, including retries." [("source", value)]
  inputRetries <- vector waitReasons $ \value -> Registry.counter registry "hydra_node_input_waits_total" "Wait outcomes by bounded reason, including inputs parked during catch-up." [("reason", value)]
  inputDrops <- vector inputSources $ \value -> Registry.counter registry "hydra_node_input_drops_total" "Inputs discarded after exhausting retry eligibility." [("source", value)]
  inputProcessing <- vector inputSources $ \value -> Registry.histogram registry "hydra_node_input_processing_seconds" "BeginInput-to-EndInput duration, including inline effects and synchronous trace handling." latencyBuckets [("source", value)]
  inputQueueWait <- Registry.histogram registry "hydra_node_input_queue_wait_seconds" "Time spent waiting for admission and dequeue on each attempt; retry delay and parked time excluded." latencyBuckets []
  queuedInputs <- Registry.gauge registry "hydra_node_input_queue_depth" "Inputs currently admitted to the bounded processing queue." []
  parkedInputs <- Registry.gauge registry "hydra_node_parked_inputs" "Network inputs parked pending chain sync or awaiting readmission after sync." []
  delayedInputs <- Registry.gauge registry "hydra_node_delayed_input_workers" "Delayed retry and parked-release workers, including workers blocked on admission." []
  ready <- Registry.gauge registry "hydra_node_ready" "One when the main loop has started, the node is synced, the network is connected and the outbox is not stalled." []
  synced <- Registry.gauge registry "hydra_node_synced" "One when the current node state is in sync." []
  phase <- vector ["idle", "open", "closed", "fanout"] $ \value -> Registry.gauge registry "hydra_head_phase" "Current head phase as a one-hot state set." [("phase", value)]
  processedSlot <- Registry.gauge registry "hydra_chain_processed_slot" "Latest chain slot in current node state; rewinds after rollback." []
  networkConnected <- Registry.gauge registry "hydra_network_connected" "Live network connectivity callback status; zero before a successful connection." []
  snapshotNumber <- Registry.gauge registry "hydra_head_confirmed_snapshot_number" "Current confirmed snapshot number; zero with no head." []
  snapshotVersion <- Registry.gauge registry "hydra_head_confirmed_snapshot_version" "Version of the current confirmed snapshot; zero with no head." []
  snapshotPending <- Registry.gauge registry "hydra_head_snapshot_inflight" "One while a snapshot request is outstanding or signatures are being collected." []
  snapshotStarted <- Registry.gauge registry "hydra_head_snapshot_started_timestamp_seconds" "Wall-clock start of the outstanding round observed in this process; zero when absent or restored with unknown start." []
  snapshotAge <- Registry.gauge registry "hydra_head_snapshot_inflight_age_seconds" "Age of the outstanding round observed in this process; zero when its start is unknown." []
  missingSignatures <- Registry.gauge registry "hydra_head_snapshot_missing_signatures" "Participants whose signatures are still missing from the outstanding snapshot." []
  outstandingTransactions <- Registry.gauge registry "hydra_head_outstanding_transactions" "Current allTxs entries awaiting inclusion in a snapshot." []
  lastSnapshot <- Registry.gauge registry "hydra_head_last_snapshot_timestamp_seconds" "Wall-clock time of the last newly confirmed or adopted snapshot in this process." []
  txOutcomes <- vector ["applied", "invalid", "refused_stalled", "refused_unsynced", "command_failed", "decommit_invalid"] $ \value -> Registry.counter registry "hydra_head_transaction_outcomes_total" "Transaction and command outcomes observed by this node, with bounded reason labels." [("outcome", value)]
  commands <- vector commandsValues $ \value -> Registry.counter registry "hydra_node_commands_total" "Client commands first processed by the main loop, by command type." [("command", value)]
  persistentDepth <- Registry.gauge registry "hydra_network_persistent_queue_depth" "Messages awaiting successful etcd broadcast, including the pinned in-flight batch." []
  persistentAge <- Registry.gauge registry "hydra_network_persistent_queue_oldest_age_seconds" "Age of the oldest persisted queued message, using its file timestamp across restart." []
  lastPut <- Registry.gauge registry "hydra_network_last_put_timestamp_seconds" "Wall-clock time of the last successful etcd put or deduplicated retry." []
  lastWatch <- Registry.gauge registry "hydra_network_last_watch_timestamp_seconds" "Wall-clock time of the last watch response, including empty responses; idle traffic may leave it unchanged." []
  putLatency <- Registry.histogram registry "hydra_network_put_seconds" "Duration of successful etcd put attempts, including deduplicated retries." latencyBuckets []
  putMessages <- Registry.counter registry "hydra_network_broadcast_messages_total" "Messages in batches acknowledged by etcd, including deduplicated retries once." []
  putBatch <- Registry.histogram registry "hydra_network_broadcast_batch_messages" "Messages per acknowledged network batch." [1, 5, 10, 25, 50] []
  networkEvents <- vector networkEventsValues $ \value -> Registry.counter registry "hydra_network_events_total" "Network failures, retries and recovery events by bounded category." [("event", value)]
  chainOperations <- vector chainOperationsValues $ \value -> Registry.counter registry "hydra_chain_operations_total" "L1 protocol posts, API deposit drafts and user transaction submissions attempted." [("operation", value), ("backend", backend)]
  chainFailures <- vector chainOperationsValues $ \value -> Registry.counter registry "hydra_chain_operation_failures_total" "Failed L1 protocol posts, API deposit drafts and user transaction submissions." [("operation", value), ("backend", backend)]
  chainDuration <- vector chainOperationsValues $ \value -> Registry.histogram registry "hydra_chain_operation_seconds" "L1 operation construction and backend submission duration; successful return does not imply on-chain inclusion." latencyBuckets [("operation", value), ("backend", backend)]
  chainInflight <- Registry.gauge registry "hydra_chain_operations_inflight" "L1 protocol posts, API deposit drafts and user transaction submissions currently executing." [("backend", backend)]
  chainSubmissions <- vector ["accepted", "failed"] $ \value -> Registry.counter registry "hydra_chain_submissions_total" "Actual backend transaction submission responses, separate from on-chain observations." [("outcome", value), ("backend", backend)]
  chainObservations <- vector ["init", "deposit", "recover", "increment", "decrement", "close", "contest", "fanout", "partial_fanout", "other"] $ \value -> Registry.counter registry "hydra_chain_observations_total" "Protocol transactions observed by the node; re-observation after rollback is counted." [("operation", value), ("backend", backend)]
  rollbacks <- Registry.counter registry "hydra_chain_rollbacks_total" "Raw backend rollback notifications." [("backend", backend)]
  rollbackDepth <- Registry.histogram registry "hydra_chain_rollback_slots" "Slots from the last backend roll-forward to the rollback point." [0, 1, 10, 100, 1000, 10000, 100000] [("backend", backend)]
  blockfrostRetries <- Registry.counter registry "hydra_chain_blockfrost_retries_total" "Retryable Blockfrost errors observed by the outer retry loop." [("backend", backend)]
  blockfrostRateLimits <- Registry.counter registry "hydra_chain_blockfrost_rate_limits_total" "HTTP 429 responses across node startup, wallet queries, chain polling and transaction submissions." [("backend", backend)]
  deposits <- vector ["inactive", "active", "expired"] $ \value -> Registry.gauge registry "hydra_head_pending_deposits" "Current unconsumed deposits tracked by this node, corrected by rollback." [("state", value)]
  oldestDeposit <- Registry.gauge registry "hydra_head_oldest_deposit_timestamp_seconds" "Creation time of the oldest unconsumed deposit; zero if none." []
  depositDeadline <- Registry.gauge registry "hydra_head_next_deposit_deadline_timestamp_seconds" "Earliest deadline of unconsumed deposits, including expired recoverable deposits; zero if none." []
  decommit <- vector ["none", "requested", "approved"] $ \value -> Registry.gauge registry "hydra_head_decommit_state" "Current decommit state as a one-hot set." [("state", value)]
  decommitStarted <- Registry.gauge registry "hydra_head_decommit_started_timestamp_seconds" "Time this process observed a new decommit; zero if restored or absent." []
  incrementalEvents <- vector incrementalEventsValues $ \value -> Registry.counter registry "hydra_head_incremental_events_total" "Deposit and decommit lifecycle observations in this process; re-observation after rollback counts again." [("event", value)]
  fanoutRemaining <- Registry.gauge registry "hydra_head_fanout_remaining_outputs" "Outputs still awaiting distribution in the current partial fanout state." []
  fanoutMode <- vector ["none", "automatic", "selected", "deferred"] $ \value -> Registry.gauge registry "hydra_head_fanout_mode" "Current fanout mode as a one-hot set; deferred work is intentionally waiting for selection." [("mode", value)]
  fanoutChunks <- Registry.counter registry "hydra_head_fanout_chunks_total" "Observed partial and final fanout transactions; re-observation after rollback counts again." []
  fanoutEvaluationFailures <- Registry.counter registry "hydra_head_fanout_evaluation_failures_total" "Failed fanout candidate evaluations; the builder may recover by selecting a smaller chunk." []
  fanoutFailures <- Registry.counter registry "hydra_head_fanout_failures_total" "Failed fanout protocol operation attempts." []
  lastFanout <- Registry.gauge registry "hydra_head_last_fanout_timestamp_seconds" "Wall-clock time of the last observed fanout chunk in this process." []
  contestationDeadline <- Registry.gauge registry "hydra_head_contestation_deadline_timestamp_seconds" "Current contestation deadline; zero for idle or open heads." []
  httpInflight <- Registry.gauge registry "hydra_api_http_inflight" "HTTP requests whose handler and response callback have not finished." []
  httpResponses <- vectorPairs routes ["1xx", "2xx", "3xx", "4xx", "5xx", "other"] $ \route status -> Registry.counter registry "hydra_api_http_responses_total" "Completed HTTP responses by normalized route and status class." [("route", route), ("status", status)]
  httpDuration <- vector routes $ \value -> Registry.histogram registry "hydra_api_http_seconds" "HTTP request duration through completion of the WAI response callback, by normalized route." latencyBuckets [("route", value)]
  apiTimeouts <- vector ["transaction", "decommit", "recover", "side_load"] $ \value -> Registry.counter registry "hydra_api_operation_timeouts_total" "Operations returning HTTP 202 because their response wait expired; distinct from accepted submissions." [("command", value)]
  wsConnections <- Registry.gauge registry "hydra_api_websocket_connections" "WebSocket handlers currently active, including handshake and history replay." []
  apiErrors <- vector ["invalid_input", "connection", "http_exception", "internal"] $ \value -> Registry.counter registry "hydra_api_errors_total" "API failures by bounded category." [("reason", value)]
  sqliteQueue <- Registry.gauge registry "hydra_persistence_queue_depth" "SQLite queue items awaiting the writer, including flush markers." []
  sqliteInflight <- Registry.gauge registry "hydra_persistence_write_inflight" "One while a SQLite event batch is being encoded and committed." []
  sqliteEvents <- Registry.counter registry "hydra_persistence_written_events_total" "Events whose SQLite transaction has completed; synchronous NORMAL does not promise power-loss durability." []
  sqliteStatsFailures <- Registry.counter registry "hydra_persistence_stat_failures_total" "Failures collecting database or WAL file sizes; previous size samples remain visible." []
  sqliteFailures <- Registry.counter registry "hydra_persistence_write_failures_total" "Failed SQLite event batches." []
  sqliteDuration <- Registry.histogram registry "hydra_persistence_write_seconds" "SQLite batch encoding and transaction completion duration." latencyBuckets []
  sqliteLatency <- Registry.histogram registry "hydra_persistence_oldest_event_seconds" "Enqueue-call-to-commit latency of the oldest event in each SQLite batch, including blocked admission." latencyBuckets []
  sqliteBatch <- Registry.histogram registry "hydra_persistence_batch_events" "Events per completed SQLite batch." [1, 10, 50, 100, 500, 1000] []
  sqliteRotation <- Registry.histogram registry "hydra_persistence_rotation_seconds" "Completed archive and checkpoint rotation duration, including queue flush." latencyBuckets []
  sqliteReplay <- Registry.histogram registry "hydra_persistence_replay_seconds" "Lifetime of SQLite event stream reads, including consumer processing until closure." latencyBuckets []
  sqliteDatabase <- Registry.gauge registry "hydra_persistence_database_bytes" "Current SQLite database file size." []
  sqliteWal <- Registry.gauge registry "hydra_persistence_wal_bytes" "Current SQLite WAL file size; zero if no WAL file exists." []
  logQueue <- Registry.gauge registry "hydra_logging_queue_depth" "Log envelopes awaiting the writer." []
  logEntries <- Registry.counter registry "hydra_logging_written_entries_total" "Log lines in batches whose output flush completed." []
  logUnconfirmed <- Registry.counter registry "hydra_logging_unconfirmed_entries_total" "Entries in batches with a write failure; some lines may have been partially written." []
  logEncodingFailures <- Registry.counter registry "hydra_logging_encoding_failures_total" "Log entries replaced by an unencodable-entry diagnostic." []
  logWriteFailures <- Registry.counter registry "hydra_logging_write_failures_total" "Log batches with an output write or flush failure." []
  logWriteDuration <- Registry.histogram registry "hydra_logging_write_seconds" "Log batch encoding, write and flush duration." latencyBuckets []
  logEnqueueWait <- Registry.histogram registry "hydra_logging_enqueue_seconds" "Time spent admitting a log envelope to its bounded queue." latencyBuckets []
  logBatch <- Registry.histogram registry "hydra_logging_batch_entries" "Entries per successfully flushed log batch." [1, 10, 50, 100, 500] []
  Registry.alias registry "hydra_head_inputs_total" "hydra_head_inputs" 1
  Registry.alias registry "hydra_head_requested_tx_total" "hydra_head_requested_tx" 1
  Registry.alias registry "hydra_head_confirmed_tx_total" "hydra_head_confirmed_tx" 1
  Registry.alias registry "hydra_head_tx_confirmation_seconds" "hydra_head_tx_confirmation_time_ms" 1000
  Registry.alias registry "hydra_head_snapshot_confirmation_seconds" "hydra_head_snapshot_confirmation_time_ms" 1000
  Registry.alias registry "hydra_head_inputs_refused_broadcast_stalled_total" "hydra_head_inputs_refused_broadcast_stalled" 1
  pure Metrics{..}

latencyBuckets :: [Double]
latencyBuckets = [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2.5, 5, 10, 30, 60, 120, 300]

vector :: [Text] -> (Text -> IO a) -> IO (Map Text a)
vector values action = Map.fromList <$> traverse (\value -> (value,) <$> action value) values

vectorPairs :: [Text] -> [Text] -> (Text -> Text -> IO a) -> IO (Map (Text, Text) a)
vectorPairs xs ys action = Map.fromList <$> traverse (\(x, y) -> ((x, y),) <$> action x y) [(x, y) | x <- xs, y <- ys]

at :: (Ord key, Show key) => Map key a -> key -> a
at values key = fromMaybe (error $ "Unregistered metric label: " <> show key) (Map.lookup key values)

inputSources :: [Text]
inputSources = ["client", "network", "chain"]

waitReasons :: [Text]
waitReasons = ["transaction", "snapshot_number", "snapshot_version", "seen_snapshot", "transactions", "contestation_deadline", "decommit", "unresolved_commit", "unresolved_decommit", "deposit_observation", "deposit_activation", "sync"]

commandsValues :: [Text]
commandsValues = ["init", "new_tx", "recover", "decommit", "close", "safe_close", "contest", "fanout", "partial_fanout", "side_load"]

networkEventsValues :: [Text]
networkEventsValues = ["reconnect", "broadcast_failure", "watch_failure", "watch_compacted", "deduplicated", "authentication_failure", "decode_value", "decode_log", "queue_load_failure", "queue_stat_failure", "queue_write_failure", "queue_delete_failure", "queue_full", "lease_low", "lease_no_response"]

chainOperationsValues :: [Text]
chainOperationsValues = ["draft_deposit", "submit", "init", "increment", "recover", "decrement", "close", "contest", "fanout", "partial_fanout", "final_fanout"]

incrementalEventsValues :: [Text]
incrementalEventsValues = ["deposit_recorded", "deposit_activated", "deposit_expired", "deposit_recovered", "commit_approved", "commit_finalized", "decommit_recorded", "decommit_approved", "decommit_invalid", "decommit_finalized"]

routes :: [Text]
routes = ["/config", "/head", "/snapshot", "/snapshot/utxo", "/snapshot/last-seen", "/commit", "/commits", "/commits/:id", "/decommit", "/protocol-parameters", "/cardano-transaction", "/transaction", "unknown"]
