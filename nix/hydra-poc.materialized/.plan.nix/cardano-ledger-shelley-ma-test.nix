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
      identifier = {
        name = "cardano-ledger-shelley-ma-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Shelley ledger with multiasset and time lock support.";
      description = "This package extends the Shelley ledger with support for\nnative tokens and timelocks.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cddl-files/shelley-ma.cddl"
        "cddl-files/mock/crypto.cddl"
        "cddl-files/mock/extras.cddl"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."shelley-spec-ledger-test" or (errorHandler.buildDepError "shelley-spec-ledger-test"))
          (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Test/Cardano/Ledger/TranslationTools"
          "Test/Cardano/Ledger/EraBuffet"
          "Test/Cardano/Ledger/MaryEraGen"
          "Test/Cardano/Ledger/Mary/Golden"
          "Test/Cardano/Ledger/Mary/Examples/Consensus"
          "Test/Cardano/Ledger/AllegraEraGen"
          "Test/Cardano/Ledger/Allegra/Examples/Consensus"
          "Test/Cardano/Ledger/ShelleyMA/TxBody"
          "Test/Cardano/Ledger/ShelleyMA/Serialisation/Coders"
          "Test/Cardano/Ledger/ShelleyMA/Serialisation/Generators"
          "Test/Cardano/Ledger/ShelleyMA/Serialisation/Roundtrip"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-shelley-ma-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."shelley-spec-ledger-test" or (errorHandler.buildDepError "shelley-spec-ledger-test"))
            (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          modules = [
            "Test/Cardano/Ledger/Mary/Examples"
            "Test/Cardano/Ledger/Mary/Examples/Cast"
            "Test/Cardano/Ledger/Mary/Examples/MultiAssets"
            "Test/Cardano/Ledger/Mary/Translation"
            "Test/Cardano/Ledger/Mary/Value"
            "Test/Cardano/Ledger/Allegra/Translation"
            "Test/Cardano/Ledger/Allegra/ScriptTranslation"
            "Test/Cardano/Ledger/ShelleyMA/Serialisation"
            "Test/Cardano/Ledger/ShelleyMA/Serialisation/CDDL"
            "Test/Cardano/Ledger/ShelleyMA/Serialisation/Golden/Encoding"
            "Test/Cardano/Ledger/ShelleyMA/Serialisation/Timelocks"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/59; }