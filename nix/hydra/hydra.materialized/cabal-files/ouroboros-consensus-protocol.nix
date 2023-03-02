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
    flags = { asserts = false; };
    package = {
      specVersion = "3.0";
      identifier = {
        name = "ouroboros-consensus-protocol";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK Formal methods team";
      homepage = "";
      url = "";
      synopsis = "Cardano consensus protocols.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      sublibs = {
        "ouroboros-consensus-protocol-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-protocol-0.1.0.1.tar.gz";
      sha256 = "9441c722c4cf2368e84122423ea502e0adfcb4e6d6c63f50f197a00c68da5ed8";
      });
    }) // {
    package-description-override = "cabal-version:      3.0\nname:               ouroboros-consensus-protocol\nversion:            0.1.0.1\nsynopsis:           Cardano consensus protocols.\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:             IOHK Formal methods team\nmaintainer:         operations@iohk.io\n\nsource-repository-head\n    type: git\n    location: https://github.com/input-output-hk/ouroboros-network\n    subdir: ouroboros-consensus-protocol\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nlibrary\n  hs-source-dirs: src\n  default-language:    Haskell2010\n  exposed-modules:\n    Ouroboros.Consensus.Protocol.Ledger.HotKey\n    Ouroboros.Consensus.Protocol.Ledger.Util\n    Ouroboros.Consensus.Protocol.Praos\n    Ouroboros.Consensus.Protocol.Praos.Common\n    Ouroboros.Consensus.Protocol.Praos.Header\n    Ouroboros.Consensus.Protocol.Praos.Translate\n    Ouroboros.Consensus.Protocol.Praos.Views\n    Ouroboros.Consensus.Protocol.Praos.VRF\n    Ouroboros.Consensus.Protocol.TPraos\n    Ouroboros.Consensus.Protocol.Translate\n  build-depends:\n    base,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-core,\n    cardano-ledger-shelley,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    containers,\n    mtl,\n    nothunks,\n    ouroboros-consensus,\n    serialise,\n    small-steps,\n    text\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n  if flag(asserts)\n    ghc-options:\n      -fno-ignore-asserts\n\nlibrary ouroboros-consensus-protocol-test\n  visibility: public\n  hs-source-dirs: test-src\n  default-language: Haskell2010\n  exposed-modules:\n    Test.Consensus.Protocol.Serialisation.Generators\n  build-depends:\n    base,\n    bytestring,\n    cardano-crypto-class,\n    cardano-crypto-tests,\n    cardano-ledger-core,\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    ouroboros-consensus-protocol,\n    QuickCheck\n";
    }