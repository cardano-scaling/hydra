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
        name = "ouroboros-consensus-shelley-test";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Test infrastructure for Shelley";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-consensus-protocol".components.sublibs.ouroboros-consensus-protocol-test or (errorHandler.buildDepError "ouroboros-consensus-protocol:ouroboros-consensus-protocol-test"))
          (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
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
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
            (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."ouroboros-consensus-shelley-test" or (errorHandler.buildDepError "ouroboros-consensus-shelley-test"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-shelley-test-0.1.0.1.tar.gz";
      sha256 = "52f32216f10c7fbd8469b1ad35af297800c9c136b9b6a2834932cd870fb1c596";
      });
    }) // {
    package-description-override = "cabal-version:         3.0\nname:                  ouroboros-consensus-shelley-test\nversion:               0.1.0.1\nsynopsis:              Test infrastructure for Shelley\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2020 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:\n                       Test.Consensus.Shelley.Examples\n                       Test.Consensus.Shelley.Generators\n                       Test.Consensus.Shelley.MockCrypto\n                       Test.ThreadNet.Infra.Alonzo\n                       Test.ThreadNet.Infra.Shelley\n                       Test.ThreadNet.TxGen.Shelley\n\n  build-depends:       base              >=4.9   && <4.15\n                     , bytestring        >=0.10  && <0.11\n                     , cardano-crypto-class\n                     , cardano-ledger-core\n                     , cardano-protocol-tpraos\n                     , cardano-slotting\n                     , containers        >=0.5   && <0.7\n                     , generic-random\n                     , quiet             >=0.2   && <0.3\n                     , mtl               >=2.2   && <2.3\n                     , QuickCheck\n                     , strict-containers\n                     , transformers\n\n                       -- cardano-ledger-specs\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-alonzo-test\n                     , cardano-ledger-babbage\n                     , cardano-ledger-babbage-test\n                     , cardano-ledger-shelley\n                     , cardano-ledger-shelley-ma\n                     , cardano-ledger-shelley-ma-test\n                     , cardano-ledger-shelley-test\n                     , cardano-protocol-tpraos\n                     , small-steps\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-protocol\n                     , ouroboros-consensus-protocol:ouroboros-consensus-protocol-test\n                     , ouroboros-consensus-test\n                     , ouroboros-consensus-shelley\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:\n                       Test.Consensus.Shelley.Coherence\n                       Test.Consensus.Shelley.Golden\n                       Test.Consensus.Shelley.Serialisation\n                       Test.ThreadNet.Shelley\n\n  build-depends:       base\n                     , bytestring\n                     , cardano-crypto-class\n                     , cardano-slotting\n                     , cborg\n                     , containers\n                     , filepath\n                     , QuickCheck\n                     , tasty\n                     , tasty-quickcheck\n\n                       -- cardano-ledger-specs\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-alonzo-test\n                     , cardano-ledger-core\n                     , cardano-ledger-shelley\n                     , cardano-protocol-tpraos\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-protocol\n                     , ouroboros-consensus-test\n                     , ouroboros-consensus-shelley\n                     , ouroboros-consensus-shelley-test\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n                       -threaded\n                       -rtsopts\n";
    }