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
        name = "ouroboros-consensus-byronspec";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "ByronSpec ledger integration in the Ouroboros consensus layer";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-byronspec-0.1.0.1.tar.gz";
      sha256 = "805438745c9d3a1e489b7826814dd946eaeb10799190663b4aecfa006db03cf4";
      });
    }) // {
    package-description-override = "name:                  ouroboros-consensus-byronspec\nversion:               0.1.0.1\nsynopsis:              ByronSpec ledger integration in the Ouroboros consensus layer\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2019 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\ncabal-version:         >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:\n                       Ouroboros.Consensus.ByronSpec.Ledger\n                       Ouroboros.Consensus.ByronSpec.Ledger.Accessors\n                       Ouroboros.Consensus.ByronSpec.Ledger.Block\n                       Ouroboros.Consensus.ByronSpec.Ledger.Conversions\n                       Ouroboros.Consensus.ByronSpec.Ledger.Forge\n                       Ouroboros.Consensus.ByronSpec.Ledger.Genesis\n                       Ouroboros.Consensus.ByronSpec.Ledger.GenTx\n                       Ouroboros.Consensus.ByronSpec.Ledger.Ledger\n                       Ouroboros.Consensus.ByronSpec.Ledger.Mempool\n                       Ouroboros.Consensus.ByronSpec.Ledger.Orphans\n                       Ouroboros.Consensus.ByronSpec.Ledger.Rules\n\n  build-depends:       base              >=4.9   && <4.15\n                     , bimap             >=0.3   && <0.5\n                     , cardano-binary\n                     , cardano-ledger-byron-test\n                     , cardano-slotting\n                     , cborg             >=0.2.2 && <0.3\n                     , containers        >=0.5   && <0.7\n                     , byron-spec-chain\n                     , byron-spec-ledger\n                     , mtl               >=2.2   && <2.3\n                     , nothunks\n                     , serialise         >=0.2   && <0.3\n                     , small-steps\n                     , transformers\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n";
    }