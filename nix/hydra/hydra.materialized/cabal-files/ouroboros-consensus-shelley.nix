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
        name = "ouroboros-consensus-shelley";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Shelley ledger integration in the Ouroboros consensus layer";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."orphans-deriving-via" or (errorHandler.buildDepError "orphans-deriving-via"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-shelley-0.1.0.1.tar.gz";
      sha256 = "aa1657a02b374edadfff99d235d286e88b7610085c446fefd5e71ebecc152b11";
      });
    }) // {
    package-description-override = "name:                  ouroboros-consensus-shelley\nversion:               0.1.0.1\nsynopsis:              Shelley ledger integration in the Ouroboros consensus layer\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2019 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\ncabal-version:         >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:\n                       Ouroboros.Consensus.Shelley.Crypto\n                       Ouroboros.Consensus.Shelley.Eras\n                       Ouroboros.Consensus.Shelley.HFEras\n                       Ouroboros.Consensus.Shelley.Ledger\n                       Ouroboros.Consensus.Shelley.Ledger.Block\n                       Ouroboros.Consensus.Shelley.Ledger.Config\n                       Ouroboros.Consensus.Shelley.Ledger.Forge\n                       Ouroboros.Consensus.Shelley.Ledger.Inspect\n                       Ouroboros.Consensus.Shelley.Ledger.Integrity\n                       Ouroboros.Consensus.Shelley.Ledger.Ledger\n                       Ouroboros.Consensus.Shelley.Ledger.Mempool\n                       Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion\n                       Ouroboros.Consensus.Shelley.Ledger.Query\n                       Ouroboros.Consensus.Shelley.Ledger.PeerSelection\n                       Ouroboros.Consensus.Shelley.Ledger.Protocol\n                       Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol\n                       Ouroboros.Consensus.Shelley.Node\n                       Ouroboros.Consensus.Shelley.Node.Common\n                       Ouroboros.Consensus.Shelley.Node.Praos\n                       Ouroboros.Consensus.Shelley.Node.TPraos\n                       Ouroboros.Consensus.Shelley.Node.Serialisation\n                       Ouroboros.Consensus.Shelley.Protocol.Abstract\n                       Ouroboros.Consensus.Shelley.Protocol.Praos\n                       Ouroboros.Consensus.Shelley.Protocol.TPraos\n                       Ouroboros.Consensus.Shelley.ShelleyHFC\n\n  build-depends:       base              >=4.9   && <4.15\n                     , base-deriving-via\n                     , bytestring        >=0.10  && <0.11\n                     , cardano-binary\n                     , cardano-crypto-class\n                     , cardano-crypto-praos\n                     , cardano-data\n                     , cardano-ledger-core\n                     , cardano-protocol-tpraos\n                     , cardano-prelude\n                     , cardano-slotting\n                     , cborg             >=0.2.2 && <0.3\n                     , containers        >=0.5   && <0.7\n                     , data-default-class\n                     , deepseq\n                     , measures\n                     , mtl               >=2.2   && <2.3\n                     , nothunks\n                     , orphans-deriving-via\n                     , serialise         >=0.2   && <0.3\n                     , strict-containers\n                     , text              >=1.2   && <1.3\n                     , transformers\n\n                       -- cardano-ledger-specs\n                     , cardano-ledger-alonzo\n                     , cardano-ledger-babbage\n                     , cardano-ledger-shelley\n                     , cardano-ledger-shelley-ma\n                     , small-steps\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-protocol\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n  if flag(asserts)\n    ghc-options:       -fno-ignore-asserts\n";
    }