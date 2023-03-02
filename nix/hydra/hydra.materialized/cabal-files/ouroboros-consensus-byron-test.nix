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
      identifier = {
        name = "ouroboros-consensus-byron-test";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Test infrastructure for Byron";
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
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-byronspec" or (errorHandler.buildDepError "ouroboros-consensus-byronspec"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary-search" or (errorHandler.buildDepError "binary-search"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
            (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
            (hsPkgs."ouroboros-consensus-byron-test" or (errorHandler.buildDepError "ouroboros-consensus-byron-test"))
            (hsPkgs."ouroboros-consensus-byronspec" or (errorHandler.buildDepError "ouroboros-consensus-byronspec"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-byron-test-0.1.0.1.tar.gz";
      sha256 = "3536c6884fe6516de28bfe3f341e3c06617f5f7fac6c39c3072f0a3bb9333314";
      });
    }) // {
    package-description-override = "name:                  ouroboros-consensus-byron-test\nversion:               0.1.0.1\nsynopsis:              Test infrastructure for Byron\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2020 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\ncabal-version:         >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:\n                       Ouroboros.Consensus.ByronDual.Ledger\n                       Ouroboros.Consensus.ByronDual.Node\n                       Ouroboros.Consensus.ByronDual.Node.Serialisation\n\n                       Test.Consensus.Byron.Examples\n                       Test.Consensus.Byron.Generators\n                       Test.ThreadNet.Infra.Byron\n                       Test.ThreadNet.Infra.Byron.Genesis\n                       Test.ThreadNet.Infra.Byron.ProtocolInfo\n                       Test.ThreadNet.Infra.Byron.TrackUpdates\n                       Test.ThreadNet.TxGen.Byron\n\n  build-depends:       base              >=4.9   && <4.15\n                     , bytestring        >=0.10  && <0.11\n                     , cardano-binary\n                     , cardano-crypto-class\n                     , cardano-crypto-test\n                     , cardano-crypto-wrapper\n                     , cardano-ledger-byron\n                     , cardano-ledger-byron-test\n                     , containers        >=0.5   && <0.7\n                     , hedgehog-quickcheck\n                     , mtl               >=2.2   && <2.3\n                     , QuickCheck\n                     , serialise         >=0.2   && <0.3\n\n                     , byron-spec-ledger\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-test\n                     , ouroboros-consensus-byron\n                     , ouroboros-consensus-byronspec\n\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:\n                       Test.Consensus.Byron.Golden\n                       Test.Consensus.Byron.Serialisation\n                       Test.ThreadNet.Byron\n                       Test.ThreadNet.DualByron\n\n  build-depends:       base\n                     , binary-search\n                     , bytestring\n                     , cardano-binary\n                     , cardano-crypto-class\n                     , cardano-crypto-wrapper\n                     , cardano-ledger-byron\n                     , cardano-ledger-byron-test\n                     , cardano-slotting\n                     , cborg\n                     , containers\n                     , filepath\n                     , hedgehog-quickcheck\n                     , mtl\n                     , QuickCheck\n                     , tasty\n                     , tasty-quickcheck\n\n                     , byron-spec-chain\n                     , byron-spec-ledger\n                     , small-steps\n                     , small-steps-test\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-test\n                     , ouroboros-consensus-byron\n                     , ouroboros-consensus-byron-test\n                     , ouroboros-consensus-byronspec\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n                       -threaded\n                       -rtsopts\n";
    }