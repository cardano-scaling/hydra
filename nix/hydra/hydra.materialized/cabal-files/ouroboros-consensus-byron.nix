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
      identifier = { name = "ouroboros-consensus-byron"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Byron ledger integration in the Ouroboros consensus layer";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          ];
        buildable = true;
        };
      exes = {
        "db-converter" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-generic" or (errorHandler.buildDepError "optparse-generic"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-byron-0.1.0.1.tar.gz";
      sha256 = "65b9093d6615a09f81e5cb449d7127ddbea15941484563f9c0c0ce9c113aef91";
      });
    }) // {
    package-description-override = "name:                  ouroboros-consensus-byron\nversion:               0.1.0.1\nsynopsis:              Byron ledger integration in the Ouroboros consensus layer\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2019 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\ncabal-version:         >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:\n                       Ouroboros.Consensus.Byron.Crypto.DSIGN\n                       Ouroboros.Consensus.Byron.EBBs\n                       Ouroboros.Consensus.Byron.Ledger\n                       Ouroboros.Consensus.Byron.Ledger.Block\n                       Ouroboros.Consensus.Byron.Ledger.Config\n                       Ouroboros.Consensus.Byron.Ledger.Conversions\n                       Ouroboros.Consensus.Byron.Ledger.Forge\n                       Ouroboros.Consensus.Byron.Ledger.HeaderValidation\n                       Ouroboros.Consensus.Byron.Ledger.Inspect\n                       Ouroboros.Consensus.Byron.Ledger.Integrity\n                       Ouroboros.Consensus.Byron.Ledger.Ledger\n                       Ouroboros.Consensus.Byron.Ledger.Mempool\n                       Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion\n                       Ouroboros.Consensus.Byron.Ledger.Orphans\n                       Ouroboros.Consensus.Byron.Ledger.PBFT\n                       Ouroboros.Consensus.Byron.Ledger.Serialisation\n                       Ouroboros.Consensus.Byron.Node\n                       Ouroboros.Consensus.Byron.Node.Serialisation\n                       Ouroboros.Consensus.Byron.Protocol\n\n  build-depends:       base              >=4.9   && <4.15\n                     , bytestring        >=0.10  && <0.11\n                     , cardano-binary\n                     , cardano-crypto\n                     , cardano-crypto-class\n                     , cardano-crypto-wrapper\n                     , cardano-ledger-byron\n                     , cardano-prelude\n                     , cardano-slotting\n                     , cborg             >=0.2.2 && <0.3\n                     , containers        >=0.5   && <0.7\n                     , cryptonite        >=0.25  && <0.28\n                     , formatting        >=6.3   && <6.4\n                     , mtl               >=2.2   && <2.3\n                     , serialise         >=0.2   && <0.3\n                     , nothunks\n                     , text              >=1.2   && <1.3\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n  if flag(asserts)\n    ghc-options:       -fno-ignore-asserts\n\nexecutable db-converter\n  hs-source-dirs:      tools/db-converter\n  main-is:             Main.hs\n  build-depends:       base\n                     , bytestring\n                     , cardano-binary\n                     , cardano-ledger-byron\n                     , directory\n                     , filepath\n                     , mtl\n                     , optparse-generic\n                     , resourcet\n                     , streaming\n                     , text\n\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-byron\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n";
    }