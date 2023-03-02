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
      identifier = { name = "cardano-ledger-babbage"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "TODO";
      description = "TODO";
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
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-babbage-0.1.0.0.tar.gz";
      sha256 = "df05565d97d2f328ca837ae971fcecf2bfcdd47780bc4e95136f6f3912911b67";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                cardano-ledger-babbage\nversion:             0.1.0.0\nsynopsis:            TODO\ndescription:         TODO\nbug-reports:         https://github.com/input-output-hk/cardano-ledger/issues\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncopyright:           2020 Input Output (Hong Kong) Ltd.\ncategory:            Network\nbuild-type:          Simple\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   eras/babbage/impl\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wpartial-fields\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n  exposed-modules:\n    Cardano.Ledger.Babbage.Genesis\n    Cardano.Ledger.Babbage.PParams\n    Cardano.Ledger.Babbage.Tx\n    Cardano.Ledger.Babbage.TxBody\n    Cardano.Ledger.Babbage.TxInfo\n    Cardano.Ledger.Babbage.Translation\n    Cardano.Ledger.Babbage.Scripts\n    Cardano.Ledger.Babbage.Collateral\n    Cardano.Ledger.Babbage.Rules.Utxow\n    Cardano.Ledger.Babbage.Rules.Utxo\n    Cardano.Ledger.Babbage.Rules.Utxos\n    Cardano.Ledger.Babbage.Rules.Ledger\n    Cardano.Ledger.Babbage\n  build-depends:\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-alonzo,\n    cardano-ledger-core,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-ma,\n    cardano-slotting,\n    containers,\n    data-default,\n    deepseq,\n    mtl,\n    nothunks,\n    plutus-ledger-api ^>= 1.0,\n    plutus-tx ^>= 1.0,\n    set-algebra,\n    small-steps,\n    strict-containers,\n    text,\n    transformers,\n    validation-selective,\n  hs-source-dirs:\n    src\n";
    }