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
      specVersion = "2.2";
      identifier = { name = "cardano-ledger-pretty"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Pretty Printers for the Cardano Ledger";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-pretty-0.1.0.0.tar.gz";
      sha256 = "71fd82760051a857d8e4b91be144a10f2c5c3fa9fafd9f6a880fd83c26c33cc2";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-pretty\nversion:             0.1.0.0\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ndescription:         Pretty Printers for the Cardano Ledger\nbuild-type:          Simple\n\nextra-source-files:\n  README.md\n  ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/cardano-ledger.git\n  subdir:   libs/cardano-ledger-pretty\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n  hs-source-dirs:     src\n  exposed-modules:\n    Cardano.Ledger.Pretty\n    Cardano.Ledger.Pretty.Alonzo\n    Cardano.Ledger.Pretty.Mary\n    Cardano.Ledger.Pretty.Babbage\n  build-depends:\n    bech32,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-alonzo,\n    cardano-ledger-babbage,\n    cardano-ledger-byron,\n    cardano-ledger-core,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-ma,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    vector-map,\n    containers,\n    hashable,\n    iproute,\n    mtl,\n    plutus-ledger-api,\n    prettyprinter,\n    small-steps,\n    strict-containers,\n    text,\n";
    }