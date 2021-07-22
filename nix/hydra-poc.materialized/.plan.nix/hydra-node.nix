{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "hydra-node"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2021 IOHK";
      maintainer = "";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Hydra node executable (server";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [ "api.yaml" ];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-node" or (errorHandler.buildDepError "cardano-node"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
          (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
          (hsPkgs."hydra-plutus" or (errorHandler.buildDepError "hydra-plutus"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-pab" or (errorHandler.buildDepError "plutus-pab"))
          (hsPkgs."prometheus" or (errorHandler.buildDepError "prometheus"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."req" or (errorHandler.buildDepError "req"))
          (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
          (hsPkgs."shelley-spec-ledger-test" or (errorHandler.buildDepError "shelley-spec-ledger-test"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          (hsPkgs."zeromq4-haskell" or (errorHandler.buildDepError "zeromq4-haskell"))
          ];
        buildable = true;
        modules = [
          "Paths_hydra_node"
          "Hydra/API/Server"
          "Hydra/Chain"
          "Hydra/Chain/ExternalPAB"
          "Hydra/Chain/ZeroMQ"
          "Hydra/HeadLogic"
          "Hydra/Ledger"
          "Hydra/Ledger/MaryTest"
          "Hydra/Ledger/Simple"
          "Hydra/Logging"
          "Hydra/Logging/Messages"
          "Hydra/Logging/Monitoring"
          "Hydra/Network"
          "Hydra/Network/BroadcastToSelf"
          "Hydra/Network/Heartbeat"
          "Hydra/Network/Ouroboros"
          "Hydra/Network/Ouroboros/Type"
          "Hydra/Network/Ouroboros/Client"
          "Hydra/Network/Ouroboros/Server"
          "Hydra/Network/ZeroMQ"
          "Hydra/Node"
          "Hydra/Node/Version"
          "Hydra/Options"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hydra-node" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."hydra-node" or (errorHandler.buildDepError "hydra-node"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe/hydra-node" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (!flags.development) "";
          };
        "mock-chain" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."hydra-node" or (errorHandler.buildDepError "hydra-node"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe/mock-chain" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (!flags.development) "";
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-junit-formatter" or (errorHandler.buildDepError "hspec-junit-formatter"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hydra-node" or (errorHandler.buildDepError "hydra-node"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."req" or (errorHandler.buildDepError "req"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.buildPackages.hydra-plutus.components.exes.hydra-pab or (pkgs.buildPackages.hydra-pab or (errorHandler.buildToolDepError "hydra-plutus:hydra-pab")))
            ];
          buildable = true;
          modules = [
            "Paths_hydra_node"
            "Hydra/APISpec"
            "Hydra/API/ServerSpec"
            "Hydra/Chain/ExternalPABSpec"
            "Hydra/Chain/ZeroMQSpec"
            "Hydra/FireForgetSpec"
            "Hydra/BehaviorSpec"
            "Hydra/HeadLogicSpec"
            "Hydra/LedgerSpec"
            "Hydra/Ledger/SimpleSpec"
            "Hydra/Logging/MonitoringSpec"
            "Hydra/NetworkSpec"
            "Hydra/Network/HeartbeatSpec"
            "Hydra/Network/BroadcastToSelfSpec"
            "Hydra/OptionsSpec"
            "Test/Util"
            "Spec"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../hydra-node; }