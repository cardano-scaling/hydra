-- | Described metrics and compatibility aliases backed by the Prometheus client.
module Hydra.Logging.Monitoring.Registry where

import Hydra.Prelude hiding (toList)

import Data.ByteString.Builder (Builder, byteString, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Metrics.Prometheus.Concurrent.Registry qualified as Prom
import System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import System.Metrics.Prometheus.Metric (MetricSample (..))
import System.Metrics.Prometheus.Metric.Counter (Counter)
import System.Metrics.Prometheus.Metric.Gauge (Gauge, GaugeSample (..))
import System.Metrics.Prometheus.Metric.Histogram (Histogram, HistogramSample (..))
import System.Metrics.Prometheus.MetricId (Labels (..), MetricId (..), Name (..))
import System.Metrics.Prometheus.Registry (RegistrySample (..))

data Registry = Registry
  { underlying :: Prom.Registry
  , descriptions :: IORef (Map Text (Text, Text))
  , aliases :: IORef [(Text, Text, Double)]
  }

new :: IO Registry
new = Registry <$> Prom.new <*> newIORef mempty <*> newIORef []

describe :: Registry -> Text -> Text -> Text -> IO ()
describe Registry{descriptions} name kind help =
  atomicModifyIORef' descriptions $ \ds -> (Map.insert name (kind, help) ds, ())

counter :: Registry -> Text -> Text -> [(Text, Text)] -> IO Counter
counter registry@Registry{underlying} name help labels = do
  describe registry name "counter" help
  Prom.registerCounter (Name name) (Labels $ Map.fromList labels) underlying

gauge :: Registry -> Text -> Text -> [(Text, Text)] -> IO Gauge
gauge registry@Registry{underlying} name help labels = do
  describe registry name "gauge" help
  Prom.registerGauge (Name name) (Labels $ Map.fromList labels) underlying

-- | Absolute, possibly fractional counters (e.g. RTS CPU seconds). The client's
-- incrementable Counter stores Int; sample these through a Gauge handle and
-- publish their counter type. Callers supply monotonically increasing values.
sampledCounter :: Registry -> Text -> Text -> IO Gauge
sampledCounter registry@Registry{underlying} name help = do
  describe registry name "counter" help
  Prom.registerGauge (Name name) mempty underlying

histogram :: Registry -> Text -> Text -> [Double] -> [(Text, Text)] -> IO Histogram
histogram registry@Registry{underlying} name help buckets labels = do
  describe registry name "histogram" help
  Prom.registerHistogram (Name name) (Labels $ Map.fromList labels) buckets underlying

-- | Both names are rendered from exactly the same sample. Histogram aliases
-- scale the sum and bucket boundaries together; counts remain unchanged.
alias :: Registry -> Text -> Text -> Double -> IO ()
alias Registry{aliases} canonical legacy scale =
  atomicModifyIORef' aliases $ \as -> ((canonical, legacy, scale) : as, ())

sample :: Registry -> IO LBS.ByteString
sample Registry{underlying, descriptions, aliases} = do
  RegistrySample values <- Prom.sample underlying
  ds <- readIORef descriptions
  as <- readIORef aliases
  let extra =
        [ (MetricId (Name legacy) labels, scaleSample scale value)
        | (canonical, legacy, scale) <- as
        , (MetricId (Name name) labels, value) <- Map.toList values
        , name == canonical
        ]
      allDescriptions = foldl' (\acc (canonical, legacy, _) -> maybe acc (\d -> Map.insert legacy d acc) (Map.lookup canonical ds)) ds as
      groups = Map.fromListWith (<>) [(unName name, [(mid, value)]) | (mid@MetricId{name}, value) <- Map.toList values <> extra]
  pure . toLazyByteString . foldMap (render allDescriptions) $ Map.toList groups
 where
  render :: Map Text (Text, Text) -> (Text, [(MetricId, MetricSample)]) -> Builder
  render ds (name, values) =
    case Map.lookup name ds of
      Nothing -> encodeMetrics (RegistrySample $ Map.fromList values)
      Just (kind, help) ->
        byteString (encodeUtf8 $ "# HELP " <> name <> " " <> escape help <> "\n# TYPE " <> name <> " " <> kind <> "\n")
          -- The library supplies samples, label escaping and histogram encoding.
          -- Replace only its TYPE header, adding the description it cannot store.
          <> lazyByteString (LBS.drop 1 . LBS.dropWhile (/= 10) . toLazyByteString . encodeMetrics . RegistrySample $ Map.fromList values)

  escape :: Text -> Text
  escape = Text.replace "\n" "\\n" . Text.replace "\\" "\\\\"

scaleSample :: Double -> MetricSample -> MetricSample
scaleSample scale = \case
  HistogramMetricSample HistogramSample{histBuckets, histSum, histCount} ->
    HistogramMetricSample HistogramSample{histBuckets = Map.mapKeysMonotonic (* scale) histBuckets, histSum = histSum * scale, histCount}
  GaugeMetricSample (GaugeSample value) -> GaugeMetricSample (GaugeSample $ value * scale)
  value -> value
