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
  Tracer m (HydraLog tx) ->
  (Tracer m (HydraLog tx) -> m ()) ->
  m ()
withMonitoring Nothing tracer action = action tracer
withMonitoring (Just monitoringPort) (Tracer tracer) action = do
  (metricsMap, registry) <- prepareRegistry
  withAsync (serveMetrics (fromIntegral monitoringPort) ["metrics"] (sample registry)) $ \_ ->
    let wrappedTracer = Tracer $ \msg -> do
          monitor metricsMap msg
          tracer msg
     in action wrappedTracer

type MetricsMap = Map.Map Text Metric

monitor ::
  MonadIO m =>
  MetricsMap ->
  HydraLog tx ->
  m ()
monitor metricsMap (Node (ProcessedEvent _)) =
  case Map.lookup "hydra_head_events" metricsMap of
    (Just (CounterMetric c)) -> liftIO $ inc c
    _ -> pure ()
monitor _ _ = pure ()

prepareRegistry :: MonadIO m => m (MetricsMap, Registry)
prepareRegistry = do
  let registry = new
      name = Name "hydra_head_events"
  (counter, registry') <- liftIO $ registerCounter name mempty registry
  let metricsMap = Map.insert "hydra_head_events" (CounterMetric counter) mempty
  pure (metricsMap, registry')
