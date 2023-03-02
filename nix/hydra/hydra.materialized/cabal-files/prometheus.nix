{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "prometheus"; version = "2.2.3"; };
      license = "BSD-3-Clause";
      copyright = "Bitnomial, Inc. (c) 2016-2019";
      maintainer = "luke@bitnomial.com, opensource@bitnomial.com";
      author = "Luke Hoersten";
      homepage = "http://github.com/bitnomial/prometheus";
      url = "";
      synopsis = "Prometheus Haskell Client";
      description = "[Prometheus Haskell Client]\n\nA simple and modern, type safe, performance focused, idiomatic Haskell client\nfor <http://prometheus.io Prometheus> monitoring. Specifically there is no\nuse of unsafe IO or manual ByteString construction from lists of\nbytes. Batteries-included web server.\n\nA key design element of this library is that the RegistryT monad transformer\nis only required for registering new time series. Once the time series is\nregistered, new data samples may just be added in the IO monad.\n\nNote: Version 0.* supports Prometheus v1.0 and version 2.* supports Prometheus v2.0.\n\n[Usage Example]\n\n> module Example where\n>\n> import           Control.Monad.IO.Class                         (liftIO)\n> import           System.Metrics.Prometheus.Http.Scrape          (serveMetricsT)\n> import           System.Metrics.Prometheus.Concurrent.RegistryT\n> import           System.Metrics.Prometheus.Metric.Counter       (inc)\n> import           System.Metrics.Prometheus.MetricId\n>\n> main :: IO ()\n> main = runRegistryT $ do\n>     -- Labels can be defined as lists or added to an empty label set\n>     connectSuccessGauge <- registerGauge \"example_connections\" (fromList [(\"login\", \"success\")])\n>     connectFailureGauge <- registerGauge \"example_connections\" (addLabel \"login\" \"failure\" mempty)\n>     connectCounter <- registerCounter \"example_connection_total\" mempty\n>     latencyHistogram <- registerHistogram \"example_round_trip_latency_ms\" mempty [10, 20..100]\n>\n>     liftIO $ inc connectCounter -- increment a counter\n>\n>     -- [...] pass metric handles to the rest of the app\n>\n>     serveMetricsT 8080 [\"metrics\"] -- http://localhost:8080/metric server\n>\n\n[Advanced Usage]\n\nA `Registry` and `StateT`-based `RegistryT` are available for unit testing or generating lists\nof `[IO a]` actions that can be `sequenced` and returned from pure code to be applied.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."atomic-primops" or (errorHandler.buildDepError "atomic-primops"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/prometheus-2.2.3.tar.gz";
      sha256 = "ff92277630d3bb510ee09eecf6bf09ab9aa7bd783d1795c6db795e67c4d8eabd";
      });
    }) // {
    package-description-override = "name:                prometheus\nversion:             2.2.3\nsynopsis:            Prometheus Haskell Client\nhomepage:            http://github.com/bitnomial/prometheus\nbug-reports:         http://github.com/bitnomial/prometheus/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Luke Hoersten\nmaintainer:          luke@bitnomial.com, opensource@bitnomial.com\ncopyright:           Bitnomial, Inc. (c) 2016-2019\ncategory:            Metrics, Monitoring, Web, System\nbuild-type:          Simple\ncabal-version:       >=1.10\n\ndescription:\n  [Prometheus Haskell Client]\n  .\n  A simple and modern, type safe, performance focused, idiomatic Haskell client\n  for <http://prometheus.io Prometheus> monitoring. Specifically there is no\n  use of unsafe IO or manual ByteString construction from lists of\n  bytes. Batteries-included web server.\n  .\n  A key design element of this library is that the RegistryT monad transformer\n  is only required for registering new time series. Once the time series is\n  registered, new data samples may just be added in the IO monad.\n  .\n  Note: Version 0.* supports Prometheus v1.0 and version 2.* supports Prometheus v2.0.\n  .\n  [Usage Example]\n  .\n  > module Example where\n  >\n  > import           Control.Monad.IO.Class                         (liftIO)\n  > import           System.Metrics.Prometheus.Http.Scrape          (serveMetricsT)\n  > import           System.Metrics.Prometheus.Concurrent.RegistryT\n  > import           System.Metrics.Prometheus.Metric.Counter       (inc)\n  > import           System.Metrics.Prometheus.MetricId\n  >\n  > main :: IO ()\n  > main = runRegistryT $ do\n  >     -- Labels can be defined as lists or added to an empty label set\n  >     connectSuccessGauge <- registerGauge \"example_connections\" (fromList [(\"login\", \"success\")])\n  >     connectFailureGauge <- registerGauge \"example_connections\" (addLabel \"login\" \"failure\" mempty)\n  >     connectCounter <- registerCounter \"example_connection_total\" mempty\n  >     latencyHistogram <- registerHistogram \"example_round_trip_latency_ms\" mempty [10, 20..100]\n  >\n  >     liftIO $ inc connectCounter -- increment a counter\n  >\n  >     -- [...] pass metric handles to the rest of the app\n  >\n  >     serveMetricsT 8080 [\"metrics\"] -- http://localhost:8080/metric server\n  >\n  .\n  [Advanced Usage]\n  .\n  A `Registry` and `StateT`-based `RegistryT` are available for unit testing or generating lists\n  of `[IO a]` actions that can be `sequenced` and returned from pure code to be applied.\n\n\nextra-source-files: Example.hs\n                  , README.md\n\nlibrary\n  hs-source-dirs: src\n  default-language: Haskell2010\n\n  ghc-options: -Wall -fwarn-tabs -fno-warn-unused-do-bind\n\n  exposed-modules: System.Metrics.Prometheus.Concurrent.Registry\n                 , System.Metrics.Prometheus.Concurrent.RegistryT\n                 , System.Metrics.Prometheus.Encode.Text\n                 , System.Metrics.Prometheus.Encode.Text.Histogram\n                 , System.Metrics.Prometheus.Encode.Text.MetricId\n                 , System.Metrics.Prometheus.Http.Push\n                 , System.Metrics.Prometheus.Http.Scrape\n                 , System.Metrics.Prometheus.Metric\n                 , System.Metrics.Prometheus.Metric.Counter\n                 , System.Metrics.Prometheus.Metric.Gauge\n                 , System.Metrics.Prometheus.Metric.Histogram\n                 , System.Metrics.Prometheus.Metric.Summary\n                 , System.Metrics.Prometheus.MetricId\n                 , System.Metrics.Prometheus.Registry\n                 , System.Metrics.Prometheus.RegistryT\n\n  build-depends: base            >= 4.9  && < 5\n               , atomic-primops  >= 0.8  && < 0.9\n               , bytestring      >= 0.10 && < 0.12\n               , containers      >= 0.5  && < 0.7\n               , http-client     >= 0.4  && < 0.8\n               , http-client-tls >= 0.3  && < 0.4\n               , http-types      >= 0.8  && < 0.13\n               , network-uri     >= 2.5  && < 2.7\n               , text            >= 1.2  && < 1.3\n               , transformers    >= 0.4  && < 0.6\n               , wai             >= 3.2  && < 3.3\n               , warp            >= 3.2  && < 3.5\n\nsource-repository head\n  type:     git\n  location: https://github.com/bitnomial/prometheus\n";
    }