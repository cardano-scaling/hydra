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
    flags = { disable-observables = false; performance-test-queue = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "iohk-monitoring"; version = "0.1.11.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "logging, benchmarking and monitoring framework";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async-timer" or (errorHandler.buildDepError "async-timer"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
          (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/iohk-monitoring-0.1.11.0.tar.gz";
      sha256 = "bdecce2d31bfdde6b5c83f3e157a9d280892542efe0296cb7b3ae7cb00ba12cf";
      });
    }) // {
    package-description-override = "name:                 iohk-monitoring\nversion:              0.1.11.0\nsynopsis:             logging, benchmarking and monitoring framework\n-- description:\nlicense:              Apache-2.0\nlicense-files:        LICENSE, NOTICE\nauthor:               Alexander Diemand, Andreas Triantafyllos\nmaintainer:           operations@iohk.io\ncopyright:            2018 IOHK\ncategory:             Benchmarking\nbuild-type:           Simple\nextra-source-files:   README.md\n                      src/Cardano/BM/Counters/os-support-darwin.h\n                      src/Cardano/BM/Counters/os-support-win.h\n\ncabal-version:        >=1.10\n\nflag disable-observables\n  description:         Turn off observables, observers.\n  default:             False\n  manual:              True\n\nflag performance-test-queue\n  description:         Set the huge size for backends' queues.\n  default:             False\n  manual:              True\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Paths_iohk_monitoring\n\n                       Cardano.BM.Configuration\n                       Cardano.BM.Configuration.Model\n                       Cardano.BM.Configuration.Static\n\n                       Cardano.BM.Counters\n                       Cardano.BM.Counters.Common\n                       Cardano.BM.Counters.Dummy\n\n                       Cardano.BM.Stats\n                       Cardano.BM.Stats.Resources\n\n                       Cardano.BM.Data.Aggregated\n                       Cardano.BM.Data.AggregatedKind\n                       Cardano.BM.Data.Backend\n                       Cardano.BM.Data.BackendKind\n                       Cardano.BM.Data.Configuration\n                       Cardano.BM.Data.Counter\n                       Cardano.BM.Data.LogItem\n                       Cardano.BM.Data.MonitoringEval\n                       Cardano.BM.Data.Observable\n                       Cardano.BM.Data.Output\n                       Cardano.BM.Data.Rotation\n                       Cardano.BM.Data.Severity\n                       Cardano.BM.Data.SubTrace\n                       Cardano.BM.Data.Trace\n                       Cardano.BM.Data.Tracer\n                       Cardano.BM.Data.Transformers\n                       Cardano.BM.Internal.ElidingTracer\n                       Cardano.BM.Tracing\n\n                       Cardano.BM.Backend.Log\n                       Cardano.BM.Backend.LogBuffer\n                       Cardano.BM.Backend.ProcessQueue\n                       Cardano.BM.Backend.Switchboard\n                       Cardano.BM.Plugin\n                       Cardano.BM.Rotator\n                       Cardano.BM.Setup\n                       Cardano.BM.Trace\n                       Cardano.BM.Tracer\n\n                       -- shamelessly stolen from ouroboros-network-framework\n                       Cardano.BM.IOManager\n                       Cardano.BM.Snocket\n\n  if !flag(disable-observables)\n    exposed-modules:   Cardano.BM.Observer.Monadic\n                       Cardano.BM.Observer.STM\n\n  if os(linux)\n    exposed-modules:   Cardano.BM.Counters.Linux\n  if os(windows)\n    exposed-modules:   Cardano.BM.Counters.Windows\n    c-sources:         src/Cardano/BM/Counters/os-support-win.c\n    include-dirs:      src/Cardano/BM/Counters/\n    cc-options:        -DPSAPI_VERSION=2\n  if os(darwin)\n    exposed-modules:   Cardano.BM.Counters.Darwin\n    c-sources:         src/Cardano/BM/Counters/os-support-darwin.c\n    include-dirs:      src/Cardano/BM/Counters/\n\n  other-modules:\n\n  default-language:    Haskell2010\n  default-extensions:  OverloadedStrings\n  other-extensions:    OverloadedStrings\n  build-depends:       base >= 4.11,\n                       aeson >= 1.4.2,\n                       array,\n                       async-timer,\n                       async,\n                       attoparsec,\n                       auto-update,\n                       base64-bytestring,\n                       bytestring,\n                       clock,\n                       containers,\n                       contra-tracer,\n                       contravariant,\n                       directory,\n                       ekg,\n                       filepath,\n                       katip,\n                       libyaml,\n                       mtl,\n                       network,\n                       safe-exceptions,\n                       safe,\n                       scientific,\n                       stm,\n                       template-haskell,\n                       text,\n                       time-units,\n                       time,\n                       tracer-transformers,\n                       transformers,\n                       unordered-containers,\n                       vector,\n                       Win32-network,\n                       yaml\n\n\n  if os(windows)\n     build-depends:    Win32\n  else\n     build-depends:    unix\n\n  if !flag(disable-observables)\n    cpp-options:       -DENABLE_OBSERVABLES\n\n  if flag(performance-test-queue)\n    cpp-options:       -DPERFORMANCE_TEST_QUEUE\n\n  ghc-options:         -Wall -Werror\n                       -fno-ignore-asserts\n\ntest-suite tests\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Test.lhs\n  other-modules:       Cardano.BM.Test.Trace\n                       Cardano.BM.Test.STM\n                       Cardano.BM.Test.Configuration\n                       Cardano.BM.Test.LogItem\n                       Cardano.BM.Test.Mock\n                       Cardano.BM.Test.Rotator\n                       Cardano.BM.Test.Routing\n                       Cardano.BM.Test.Structured\n                       Cardano.BM.Test.Tracer\n                       Cardano.BM.Test.Aggregated\n                       Cardano.BM.Arbitrary\n                       Cardano.BM.Arbitrary.Aggregated\n\n  default-language:    Haskell2010\n  default-extensions:  OverloadedStrings\n  build-depends:       base,\n                       contra-tracer,\n                       iohk-monitoring,\n                       aeson >= 1.4.2,\n                       array,\n                       async,\n                       bytestring,\n                       clock,\n                       containers,\n                       directory,\n                       filepath,\n                       mtl,\n                       process,\n                       QuickCheck,\n                       random,\n                       semigroups,\n                       split,\n                       stm,\n                       tasty,\n                       tasty-hunit,\n                       tasty-quickcheck,\n                       temporary,\n                       text,\n                       time,\n                       time-units,\n                       tracer-transformers,\n                       transformers,\n                       unordered-containers,\n                       vector,\n                       void,\n                       yaml, libyaml\n  ghc-options:         -Wall -threaded -rtsopts \"-with-rtsopts=-T\"\n\n  if !flag(disable-observables)\n    cpp-options:       -DENABLE_OBSERVABLES\n";
    }