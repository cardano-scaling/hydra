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
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "local-cluster"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2021 IOHK";
      maintainer = "";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Integration test suite using a local cluster of cardano and hydra nodes";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
          (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
          (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
          (hsPkgs."io-sim-classes" or (errorHandler.buildDepError "io-sim-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."say" or (errorHandler.buildDepError "say"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          ];
        buildable = true;
        modules = [ "HydraNode" "Lib" "Logging" "Ports" "Node" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "local-cluster" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."local-cluster" or (errorHandler.buildDepError "local-cluster"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [ "local-cluster.hs" ];
          };
        };
      tests = {
        "integration" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."local-cluster" or (errorHandler.buildDepError "local-cluster"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."say" or (errorHandler.buildDepError "say"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.buildPackages.hydra-node.components.exes.hydra-node or (pkgs.buildPackages.hydra-node or (errorHandler.buildToolDepError "hydra-node:hydra-node")))
            (hsPkgs.buildPackages.hydra-node.components.exes.mock-chain or (pkgs.buildPackages.mock-chain or (errorHandler.buildToolDepError "hydra-node:mock-chain")))
            ];
          buildable = true;
          modules = [ "Test/EndToEndSpec" "Test/LocalClusterSpec" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../local-cluster; }