-- | Provides Prometheus-based metrics server based on `Tracer` collection.
module Hydra.Logging.Monitoring (
  withMonitoring,
) where

import Cardano.Prelude hiding (Async, withAsync)
import Control.Monad.Class.MonadAsync (MonadAsync, withAsync)
import Control.Tracer (Tracer (Tracer))
import qualified Data.Map as Map
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network (PortNumber)
import Hydra.Node (HydraNodeLog (ProcessedEvent))
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import System.Metrics.Prometheus.Metric (Metric (CounterMetric))
import System.Metrics.Prometheus.Metric.Counter (inc)
import System.Metrics.Prometheus.MetricId (Name (Name))
import System.Metrics.Prometheus.Registry (Registry, new, registerCounter, sample)

-- | Wraps a monadic action using a `Tracer` and capture metrics based on traces.
-- Given a `portNumber`, this wrapper starts a Prometheus-compliant server on this port.
-- This is a no-op if given `Nothing`. This function is not polymorphic over the type of
-- messages because it needs to understand them in order to provide meaningful metrics.
withMonitoring ::
  (MonadIO m, MonadAsync m) =>
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
prepareRegistry :: MonadIO m => m (HydraLog tx net -> m (), Registry)
prepareRegistry = first monitor <$> registerMetrics
 where
  monitor metricsMap (Node (ProcessedEvent _)) =
    case Map.lookup "hydra_head_events" metricsMap of
      (Just (CounterMetric c)) -> liftIO $ inc c
      _ -> pure ()
  monitor _ _ = pure ()

  registerMetrics = foldM registerMetric (mempty, new) allMetrics

  allMetrics = [(Name "hydra_head_events", CounterMetric, flip registerCounter mempty)]

  registerMetric (metricsMap, registry) (name, ctor, registration) = do
    (metric, registry') <- liftIO $ registration name registry
    pure (Map.insert name (ctor metric) metricsMap, registry')
