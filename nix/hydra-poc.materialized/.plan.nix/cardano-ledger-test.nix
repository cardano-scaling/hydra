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
      specVersion = "3.0";
      identifier = { name = "cardano-ledger-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "nicholas.clarke@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Testing harness, tests and benchmarks for Shelley style cardano ledgers";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGELOG.md"
        "bench/resources/0_ledgerstate.cbor"
        "bench/resources/0_tx.cbor"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
          (hsPkgs."genvalidity-scientific" or (errorHandler.buildDepError "genvalidity-scientific"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        modules = [
          "Test/Cardano/Ledger/BaseTypes"
          "Test/Cardano/Ledger/Examples/TwoPhaseValidation"
          "Test/Cardano/Ledger/Generic/Proof"
          "Test/Cardano/Ledger/Generic/Indexed"
          "Test/Cardano/Ledger/Generic/Updaters"
          "Test/Cardano/Ledger/Properties"
          "Test/Cardano/Ledger/TestableEra"
          "Test/Cardano/Ledger/Alonzo/Tools"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-test" or (errorHandler.buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            ];
          buildable = true;
          modules = [
            "Bench/Cardano/Ledger/ApplyTx"
            "Bench/Cardano/Ledger/EpochBoundary"
            "Bench/Cardano/Ledger/Serialisation/Generators"
            ];
          hsSourceDirs = [ "bench" ];
          };
        "benchProperty" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "benchProperty" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "11";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "11";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/libs/cardano-ledger-test; echo source root reset to \$sourceRoot";
    }