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
    flags = { hydra-development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "hydra-tui"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2021 IOHK";
      maintainer = "";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "TUI for managing a Hydra node";
      description = "TUI for managing a Hydra node";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hydra-node" or (errorHandler.buildDepError "hydra-node"))
          (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."local-cluster" or (errorHandler.buildDepError "local-cluster"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          ];
        buildable = true;
        modules = [
          "Paths_hydra_tui"
          "Hydra/Client"
          "Hydra/TUI"
          "Hydra/TUI/Options"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hydra-tui" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."hydra-tui" or (errorHandler.buildDepError "hydra-tui"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (!flags.hydra-development) "";
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hydra-node" or (errorHandler.buildDepError "hydra-node"))
            (hsPkgs."hydra-prelude" or (errorHandler.buildDepError "hydra-prelude"))
            (hsPkgs."hydra-test-utils" or (errorHandler.buildDepError "hydra-test-utils"))
            (hsPkgs."hydra-tui" or (errorHandler.buildDepError "hydra-tui"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."local-cluster" or (errorHandler.buildDepError "local-cluster"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.buildPackages.hydra-node.components.exes.hydra-node or (pkgs.buildPackages.hydra-node or (errorHandler.buildToolDepError "hydra-node:hydra-node")))
            (hsPkgs.buildPackages.cardano-node.components.exes.cardano-node or (pkgs.buildPackages.cardano-node or (errorHandler.buildToolDepError "cardano-node:cardano-node")))
            ];
          buildable = true;
          modules = [ "Hydra/TUI/OptionsSpec" "Hydra/TUISpec" "Spec" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../hydra-tui; }