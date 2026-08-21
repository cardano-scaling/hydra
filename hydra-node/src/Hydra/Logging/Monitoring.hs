{-# LANGUAGE RecordWildCards #-}

-- | Provides Prometheus-based metrics server based on `Tracer` collection.
--
-- To add a new metric, one needs to:
--
--  * Add a field to 'Metrics' and register it in 'registerMetrics',
--  * Update the 'monitor' function to handle relevant 'HydraLog' entries and
--    update the underlying Prometheus metric. Nested helpers are provided to
--    increase a 'Counter' by one (@tick@) and to 'observe' some value in a
--    'Histogram'.
--
-- The metric handles are typed, so a metric can only be updated in the way it
-- was registered and a name can only be referred to if it was registered.
module Hydra.Logging.Monitoring (
  withMonitoring,
) where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO, writeTVar)
import Control.Tracer (Tracer (Tracer))
import Data.Map.Strict as Map
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Hydra.HeadLogic (
  Input (NetworkInput),
 )
import Hydra.HeadLogic.Outcome (Outcome (..), StateChanged (..))
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network (PortNumber)
import Hydra.Network.Message (Message (ReqTx), NetworkEvent (..))
import Hydra.Node (HydraNodeLog (..))
import Hydra.Tx (IsTx (TxIdType), Snapshot (..), SnapshotNumber, txId)
import System.Metrics.Prometheus.Concurrent.Registry (
  Registry,
  new,
  registerCounter,
  registerGauge,
  registerHistogram,
  sample,
 )
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import System.Metrics.Prometheus.Metric.Counter (Counter, add, inc)
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import System.Metrics.Prometheus.Metric.Gauge qualified as Gauge
import System.Metrics.Prometheus.Metric.Histogram (Histogram, observe)
import System.Metrics.Prometheus.MetricId (Name (Name))

-- | Handles to all metrics hydra-node exposes.
--
-- NOTE: The 'Name's below are a public interface: they are scraped by
-- Prometheus and referenced by the Grafana dashboards in @demo/grafana@, so
-- renaming one is a breaking change for operators.
data Metrics = Metrics
  { headInputs :: Counter
  , headRequestedTx :: Counter
  , headConfirmedTx :: Counter
  , txConfirmationTime :: Histogram
  , snapshotConfirmationTime :: Histogram
  , peersConnected :: Gauge
  , chainDriftSeconds :: Gauge
  , chainLastBlockTimestampSeconds :: Gauge
  }

-- | Wraps a monadic action using a `Tracer` and capture metrics based on traces.
-- Given a `portNumber`, this wrapper starts a Prometheus-compliant server on this port.
-- This is a no-op if given `Nothing`. This function is not polymorphic over the type of
-- messages because it needs to understand them in order to provide meaningful metrics.
withMonitoring ::
  (MonadIO m, MonadAsync m, IsTx tx, MonadMonotonicTime m, MonadTime m, MonadLabelledSTM m) =>
  Maybe PortNumber ->
  Tracer m (HydraLog tx) ->
  (Tracer m (HydraLog tx) -> m ()) ->
  m ()
withMonitoring Nothing tracer action = action tracer
withMonitoring (Just monitoringPort) (Tracer tracer) action = do
  (traceMetric, registry) <- prepareRegistry
  withAsyncLabelled
    ("monitoring-serveMetrics", serveMetrics (fromIntegral monitoringPort) ["metrics"] (sample registry))
    $ \_ ->
      let wrappedTracer = Tracer $ \msg -> do
            traceMetric msg
            tracer msg
       in action wrappedTracer

-- | Register all relevant metrics.
-- Returns the `Registry` which is needed to `serveMetrics` or any other form of publication
-- of metrics, whether push or pull, and a function for updating metrics given some trace event.
prepareRegistry :: forall m tx. (MonadIO m, MonadMonotonicTime m, MonadTime m, IsTx tx, MonadLabelledSTM m) => m (HydraLog tx -> m (), Registry)
prepareRegistry = do
  transactionsMap <- newLabelledTVarIO "monitoring-txs-map-registry" mempty
  snapshotsMap <- newLabelledTVarIO "monitoring-snapshots-map-registry" mempty
  registry <- liftIO new
  metrics <- registerMetrics registry
  pure (monitor transactionsMap snapshotsMap metrics, registry)

registerMetrics :: MonadIO m => Registry -> m Metrics
registerMetrics registry = liftIO $ do
  headInputs <- counter "hydra_head_inputs"
  headRequestedTx <- counter "hydra_head_requested_tx"
  headConfirmedTx <- counter "hydra_head_confirmed_tx"
  txConfirmationTime <-
    histogram "hydra_head_tx_confirmation_time_ms" [5, 10, 50, 100, 1000]
  snapshotConfirmationTime <-
    histogram "hydra_head_snapshot_confirmation_time_ms" [5, 10, 50, 100, 500, 1000, 5000, 10000, 30000]
  peersConnected <- gaugeMetric "hydra_head_peers_connected"
  chainDriftSeconds <- gaugeMetric "hydra_chain_drift_seconds"
  chainLastBlockTimestampSeconds <- gaugeMetric "hydra_chain_last_block_timestamp_seconds"
  pure Metrics{..}
 where
  counter name = registerCounter (Name name) mempty registry
  gaugeMetric name = registerGauge (Name name) mempty registry
  histogram name buckets = registerHistogram (Name name) mempty buckets registry

-- | Main monitoring function that updates metrics store given some log entries.
monitor ::
  forall m tx.
  (MonadIO m, MonadSTM m, MonadMonotonicTime m, MonadTime m, IsTx tx) =>
  TVar m (Map (TxIdType tx) Time) ->
  TVar m (Map SnapshotNumber (Time, [TxIdType tx])) ->
  Metrics ->
  HydraLog tx ->
  m ()
monitor transactionsMap snapshotsMap Metrics{..} = \case
  (Node BeginInput{input = NetworkInput _ (ReceivedMessage{msg = ReqTx tx})}) -> do
    t <- getMonotonicTime
    -- NOTE: If a requested transaction never gets confirmed, it might stick
    -- forever in the transactions map which could lead to unbounded growth and
    -- memory leak. We might want to have a 'cleaner' thread run that will remove
    -- transactions after some timeout expires
    atomically $ modifyTVar' transactionsMap (Map.insert (txId tx) t)
    tick headRequestedTx
  (Node LogicOutcome{outcome = Continue{stateChanges}}) -> do
    forM_ stateChanges $ \case
      PeerConnected{} -> gauge Gauge.inc peersConnected
      PeerDisconnected{} -> gauge Gauge.dec peersConnected
      NetworkDisconnected{} -> gaugeN peersConnected 0
      -- On every observed tick, report how far behind the chain we are and when
      -- we last heard from the backend. The latter is a wall-clock timestamp, so
      -- monitoring can alert on `time() - hydra_chain_last_block_timestamp_seconds`
      -- and detect a stalled backend even while the drift gauge is frozen (#2749).
      TickObserved{chainTime} -> do
        now <- getCurrentTime
        gaugeN chainDriftSeconds (realToFrac (now `diffUTCTime` chainTime))
        gaugeN chainLastBlockTimestampSeconds (realToFrac (utcTimeToPOSIXSeconds now))
      SnapshotRequested{requestedSnapshot = Snapshot{number, confirmed}} -> do
        t <- getMonotonicTime
        atomically $ modifyTVar' snapshotsMap (Map.insert number (t, txId <$> confirmed))
      SnapshotConfirmed{snapshot = mSnapshot} -> do
        t <- getMonotonicTime
        -- On the normal signing path the event carries no snapshot; the
        -- protocol confirms snapshots strictly in order, so the oldest
        -- in-flight request is the confirmed one.
        mEntry <- atomically $ do
          inFlight <- readTVar snapshotsMap
          forM (Map.lookupMin inFlight) $ \(number, entry) -> do
            writeTVar snapshotsMap (Map.delete number inFlight)
            pure entry
        confirmedIds <- case (mSnapshot, mEntry) of
          -- Side-load path: no preceding SnapshotRequested, but the event
          -- carries the snapshot.
          (Just Snapshot{confirmed}, _) -> pure (txId <$> confirmed)
          (Nothing, Just (_, txIds)) -> pure txIds
          (Nothing, Nothing) -> pure []
        forM_ mEntry $ \(start, _) ->
          histo snapshotConfirmationTime (diffTime t start)
        tickN headConfirmedTx (length confirmedIds)
        forM_ confirmedIds $ \i -> do
          txsStartTime <- readTVarIO transactionsMap
          case Map.lookup i txsStartTime of
            Just start -> do
              atomically $ modifyTVar' transactionsMap $ Map.delete i
              histo txConfirmationTime (diffTime t start)
            Nothing -> pure ()
      _ -> pure ()
  (Node (EndInput _ _)) ->
    tick headInputs
  _ -> pure ()
 where
  gaugeN :: Gauge -> Double -> m ()
  gaugeN g num = liftIO $ Gauge.set num g

  gauge :: (Gauge -> IO ()) -> Gauge -> m ()
  gauge f g = liftIO $ f g

  tick :: Counter -> m ()
  tick c = liftIO $ inc c

  tickN :: Counter -> Int -> m ()
  tickN c num = liftIO $ add num c

  histo :: Histogram -> DiffTime -> m ()
  histo h time = liftIO $ observe (fromRational $ toRational $ time * 1000) h
