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
      specVersion = "1.10";
      identifier = {
        name = "ouroboros-consensus-cardano";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "The instantation of the Ouroboros consensus layer used by Cardano";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          ];
        buildable = true;
        };
      exes = {
        "db-analyser" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-cardano-0.1.0.1.tar.gz";
      sha256 = "5dcb353fe084d03f6d303cb1fbf72758150c30479669bf6d8edcc045544617e3";
      });
    }) // {
    package-description-override = "name:                  ouroboros-consensus-cardano\nversion:               0.1.0.1\nsynopsis:              The instantation of the Ouroboros consensus layer used by Cardano\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2019 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\ncabal-version:         >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:\n                       Ouroboros.Consensus.Cardano\n                       Ouroboros.Consensus.Cardano.Block\n                       Ouroboros.Consensus.Cardano.ByronHFC\n                       Ouroboros.Consensus.Cardano.Condense\n                       Ouroboros.Consensus.Cardano.CanHardFork\n                       Ouroboros.Consensus.Cardano.Node\n                       Ouroboros.Consensus.Cardano.ShelleyBased\n\n  build-depends:       base              >=4.9   && <4.15\n                     , bytestring        >=0.10  && <0.11\n                     , cborg             >=0.2.2 && <0.3\n                     , containers        >=0.5   && <0.7\n                     , mtl               >=2.2   && <2.3\n                     , nothunks\n                     , these             >=1.1   && <1.2\n\n                     , cardano-binary\n                     , cardano-crypto-class\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-babbage\n                     , cardano-ledger-byron\n                     , cardano-ledger-core\n                     , cardano-ledger-shelley\n                     , cardano-ledger-shelley-ma\n                     , cardano-prelude\n                     , cardano-protocol-tpraos\n                     , cardano-slotting\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-byron\n                     , ouroboros-consensus-protocol\n                     , ouroboros-consensus-shelley\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n  if flag(asserts)\n    ghc-options:       -fno-ignore-asserts\n\nexecutable db-analyser\n  hs-source-dirs:      tools/db-analyser\n  main-is:             Main.hs\n  build-depends:       aeson\n                     , base\n                     , bytestring\n                     , cardano-binary\n                     , cardano-crypto-class\n                     , cardano-crypto-wrapper\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-byron\n                     , cardano-ledger-core\n                     , cardano-ledger-shelley\n                     , cborg\n                     , containers\n                     , contra-tracer\n                     , mtl\n                     , nothunks\n                     , optparse-applicative\n                     , serialise\n                     , strict-containers\n                     , text\n\n                     , ouroboros-consensus\n                     , ouroboros-consensus-byron\n                     , ouroboros-consensus-cardano\n                     , ouroboros-consensus-protocol\n                     , ouroboros-consensus-shelley\n                     , ouroboros-network\n                     , plutus-ledger-api\n  other-modules:\n                       Analysis\n                     , Block.Byron\n                     , Block.Cardano\n                     , Block.Shelley\n                     , HasAnalysis\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -threaded\n                       -rtsopts\n                       \"-with-rtsopts=-T -I0 -N2 -A16m\"\n";
    }