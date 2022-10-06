-- | Provides Prometheus-based metrics server based on `Tracer` collection.
--
-- To add a new metric, one needs to:
--
--  * Add a 'MetricDefinition' to the 'allMetrics' list, providing a unique 'Name', the
--    relevant constructor for the 'Metric' value and a registration function,
--  * Update the 'monitor' function to Handle relevant 'HydraLog' entries and update
--    underlying Prometheus metrics store. Nested helpers are provided to increase a
--    'Counter' by one (@tick@), by some integral value (@tickN@), and to 'observe'
--    some value in an 'Histogram'.
module Hydra.Logging.Monitoring (
  withMonitoring,
) where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Tracer (Tracer (Tracer))
import Data.Map.Strict as Map
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.HeadLogic (
  Effect (ClientEffect),
  Event (NetworkEvent),
 )
import Hydra.Ledger (IsTx (TxIdType), txId)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network (PortNumber)
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node (HydraNodeLog (BeginEvent, EndEffect, EndEvent))
import Hydra.Snapshot (Snapshot (confirmed))
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import System.Metrics.Prometheus.Metric (Metric (CounterMetric, HistogramMetric))
import System.Metrics.Prometheus.Metric.Counter (add, inc)
import System.Metrics.Prometheus.Metric.Histogram (observe)
import System.Metrics.Prometheus.MetricId (Name (Name))
import System.Metrics.Prometheus.Registry (Registry, new, registerCounter, registerHistogram, sample)

-- | Wraps a monadic action using a `Tracer` and capture metrics based on traces.
-- Given a `portNumber`, this wrapper starts a Prometheus-compliant server on this port.
-- This is a no-op if given `Nothing`. This function is not polymorphic over the type of
-- messages because it needs to understand them in order to provide meaningful metrics.
withMonitoring ::
  (MonadIO m, MonadAsync m, IsTx tx, MonadMonotonicTime m) =>
  Maybe PortNumber ->
  Tracer m (HydraLog tx net) ->
  (Tracer m (HydraLog tx net) -> m ()) ->
  m ()
withMonitoring Nothing tracer action = action tracer
withMonitoring (Just monitoringPort) (Tracer tracer) action = do
  (traceMetric, registry) <- prepareRegistry
  withAsync (serveMetrics (fromIntegral monitoringPort) ["metrics"] (sample registry)) $ \_ ->
    let wrappedTracer = Tracer $ \msg -> do
          traceMetric msg
          tracer msg
     in action wrappedTracer

-- | Register all relevant metrics.
-- Returns an updated `Registry` which is needed to `serveMetrics` or any other form of publication
-- of metrics, whether push or pull, and a function for updating metrics given some trace event.
prepareRegistry :: (MonadIO m, MonadSTM m, MonadMonotonicTime m, IsTx tx) => m (HydraLog tx net -> m (), Registry)
prepareRegistry = do
  transactionsMap <- newTVarIO mempty
  first (monitor transactionsMap) <$> registerMetrics
 where
  registerMetrics = foldlM registerMetric (mempty, new) allMetrics

  registerMetric (metricsMap, registry) (MetricDefinition name ctor registration) = do
    (metric, registry') <- liftIO $ registration name registry
    pure (Map.insert name (ctor metric) metricsMap, registry')

-- | Existential wrapper around different kind of metrics construction logic.
data MetricDefinition where
  MetricDefinition :: forall a. Name -> (a -> Metric) -> (Name -> Registry -> IO (a, Registry)) -> MetricDefinition

-- |All custom 'MetricDefinition's for Hydra
allMetrics :: [MetricDefinition]
allMetrics =
  [ MetricDefinition (Name "hydra_head_events") CounterMetric $ flip registerCounter mempty
  , MetricDefinition (Name "hydra_head_requested_tx") CounterMetric $ flip registerCounter mempty
  , MetricDefinition (Name "hydra_head_confirmed_tx") CounterMetric $ flip registerCounter mempty
  , MetricDefinition (Name "hydra_head_tx_confirmation_time_ms") HistogramMetric $ \n -> registerHistogram n mempty [5, 10, 50, 100, 1000]
  ]

-- | Main monitoring function that updates metrics store given some log entries.
monitor ::
  (MonadIO m, MonadSTM m, MonadMonotonicTime m, IsTx tx) =>
  TVar m (Map (TxIdType tx) Time) ->
  Map Name Metric ->
  HydraLog tx net ->
  m ()
monitor transactionsMap metricsMap = \case
  (Node (BeginEvent _ (NetworkEvent _ (ReqTx _ tx)))) -> do
    t <- getMonotonicTime
    -- NOTE: If a requested transaction never gets confirmed, it might stick
    -- forever in the transactions map which could lead to unbounded growth and
    -- memory leak. We might want to have a 'cleaner' thread run that will remove
    -- transactions after some timeout expires
    atomically $ modifyTVar' transactionsMap (Map.insert (txId tx) t)
    tick "hydra_head_requested_tx"
  (Node (EndEffect _ (ClientEffect (SnapshotConfirmed snapshot _)))) -> do
    t <- getMonotonicTime
    forM_ (confirmed snapshot) $ \tx -> do
      let tid = txId tx
      txsStartTime <- readTVarIO transactionsMap
      case Map.lookup tid txsStartTime of
        Just start -> do
          atomically $ modifyTVar' transactionsMap $ Map.delete tid
          histo "hydra_head_tx_confirmation_time_ms" (diffTime t start)
        Nothing -> pure ()
    tickN "hydra_head_confirmed_tx" (length $ confirmed snapshot)
  (Node (EndEvent _ _)) ->
    tick "hydra_head_events"
  _ -> pure ()
 where
  tick metricName =
    case Map.lookup metricName metricsMap of
      (Just (CounterMetric c)) -> liftIO $ inc c
      _ -> pure ()

  tickN metricName num =
    case Map.lookup metricName metricsMap of
      (Just (CounterMetric c)) -> liftIO $ add num c
      _ -> pure ()

  histo metricName time =
    case Map.lookup metricName metricsMap of
      (Just (HistogramMetric h)) -> liftIO $ observe (fromRational $ toRational $ time * 1000) h
      _ -> pure ()
