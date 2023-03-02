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
      specVersion = "3.0";
      identifier = {
        name = "ouroboros-consensus-cardano-test";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Test of the instantation of the Ouroboros consensus layer used by Cardano";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-byron-test" or (errorHandler.buildDepError "ouroboros-consensus-byron-test"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-consensus-shelley-test" or (errorHandler.buildDepError "ouroboros-consensus-shelley-test"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-consensus-protocol".components.sublibs.ouroboros-consensus-protocol-test or (errorHandler.buildDepError "ouroboros-consensus-protocol:ouroboros-consensus-protocol-test"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
            (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
            (hsPkgs."ouroboros-consensus-byron-test" or (errorHandler.buildDepError "ouroboros-consensus-byron-test"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."ouroboros-consensus-shelley-test" or (errorHandler.buildDepError "ouroboros-consensus-shelley-test"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-cardano-test" or (errorHandler.buildDepError "ouroboros-consensus-cardano-test"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-cardano-test-0.1.0.1.tar.gz";
      sha256 = "d298b529b3b21b88e755338603672d86e4562f955e3ec7e4ae2d127d5905dff0";
      });
    }) // {
    package-description-override = "cabal-version:         3.0\nname:                  ouroboros-consensus-cardano-test\nversion:               0.1.0.1\nsynopsis:              Test of the instantation of the Ouroboros consensus layer used by Cardano\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2020 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:\n                       Test.Consensus.Cardano.Examples\n                       Test.Consensus.Cardano.Generators\n                       Test.Consensus.Cardano.MockCrypto\n\n                       Test.ThreadNet.Infra.ShelleyBasedHardFork\n                       Test.ThreadNet.Infra.TwoEras\n\n                       Test.ThreadNet.TxGen.Allegra\n                       Test.ThreadNet.TxGen.Alonzo\n                       Test.ThreadNet.TxGen.Babbage\n                       Test.ThreadNet.TxGen.Cardano\n                       Test.ThreadNet.TxGen.Mary\n\n  build-depends:       base\n                     , cardano-crypto-class\n                     , cardano-crypto-wrapper\n                     , cardano-slotting\n                     , containers\n                     , mtl\n                     , QuickCheck\n                     , sop-core\n                     , strict-containers\n\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-alonzo-test\n                     , cardano-ledger-babbage\n                     , cardano-ledger-babbage-test\n                     , cardano-ledger-byron\n                     , cardano-ledger-core\n                     , cardano-ledger-shelley\n                     , cardano-ledger-shelley-test\n                     , cardano-protocol-tpraos\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-test\n                     , ouroboros-consensus-byron\n                     , ouroboros-consensus-byron-test\n                     , ouroboros-consensus-shelley\n                     , ouroboros-consensus-shelley-test\n                     , ouroboros-consensus-cardano\n                     , ouroboros-consensus-protocol\n                     , ouroboros-consensus-protocol:ouroboros-consensus-protocol-test\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:\n                       Test.Consensus.Cardano.ByronCompatibility\n                       Test.Consensus.Cardano.Golden\n                       Test.Consensus.Cardano.Serialisation\n                       Test.ThreadNet.AllegraMary\n                       Test.ThreadNet.Cardano\n                       Test.ThreadNet.MaryAlonzo\n                       Test.ThreadNet.ShelleyAllegra\n\n  build-depends:       base\n                     , bytestring\n                     , cardano-crypto-class\n                     , cardano-slotting\n                     , cborg\n                     , containers\n                     , filepath\n                     , QuickCheck\n                     , sop-core\n                     , tasty\n                     , tasty-quickcheck\n\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-byron\n                     , cardano-ledger-core\n                     , cardano-ledger-shelley\n                     , cardano-protocol-tpraos\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-test\n                     , ouroboros-consensus-byron\n                     , ouroboros-consensus-byron-test\n                     , ouroboros-consensus-protocol\n                     , ouroboros-consensus-shelley\n                     , ouroboros-consensus-shelley-test\n                     , ouroboros-consensus-cardano\n                     , ouroboros-consensus-cardano-test\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n                       -threaded\n                       -rtsopts\n";
    }