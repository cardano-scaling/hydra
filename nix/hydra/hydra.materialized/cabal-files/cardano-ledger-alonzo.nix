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
      identifier = { name = "cardano-ledger-alonzo"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Cardano ledger introducing Plutus Core";
      description = "This package builds upon the Mary ledger with support for extended UTxO\nvia Plutus Core.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-alonzo-0.1.0.0.tar.gz";
      sha256 = "e3251ee117bd927251ce0eba28f9faf092d801fc6fab0e77fcfb0701faffae31";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                cardano-ledger-alonzo\nversion:             0.1.0.0\nsynopsis:            Cardano ledger introducing Plutus Core\ndescription:\n  This package builds upon the Mary ledger with support for extended UTxO\n  via Plutus Core.\nbug-reports:         https://github.com/input-output-hk/cardano-ledger/issues\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncopyright:           2020 Input Output (Hong Kong) Ltd.\ncategory:            Network\nbuild-type:          Simple\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   eras/alonzo/impl\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wpartial-fields\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n  exposed-modules:\n    Cardano.Ledger.Alonzo\n    Cardano.Ledger.Alonzo.Data\n    Cardano.Ledger.Alonzo.Genesis\n    Cardano.Ledger.Alonzo.Language\n    Cardano.Ledger.Alonzo.PlutusScriptApi\n    Cardano.Ledger.Alonzo.PParams\n    Cardano.Ledger.Alonzo.Rules.Bbody\n    Cardano.Ledger.Alonzo.Rules.Ledger\n    Cardano.Ledger.Alonzo.Rules.Utxo\n    Cardano.Ledger.Alonzo.Rules.Utxos\n    Cardano.Ledger.Alonzo.Rules.Utxow\n    Cardano.Ledger.Alonzo.Scripts\n    Cardano.Ledger.Alonzo.Tools\n    Cardano.Ledger.Alonzo.Translation\n    Cardano.Ledger.Alonzo.Tx\n    Cardano.Ledger.Alonzo.TxBody\n    Cardano.Ledger.Alonzo.TxInfo\n    Cardano.Ledger.Alonzo.TxSeq\n    Cardano.Ledger.Alonzo.TxWitness\n    Cardano.Ledger.DescribeEras\n  build-depends:\n    aeson >= 2,\n    array,\n    base-deriving-via,\n    base64-bytestring,\n    bytestring,\n    cardano-binary,\n    cardano-data,\n    cardano-crypto-class,\n    cardano-ledger-core,\n    cardano-ledger-shelley-ma,\n    cardano-prelude,\n    cardano-slotting,\n    containers,\n    data-default,\n    deepseq,\n    measures,\n    mtl,\n    nothunks,\n    plutus-ledger-api ^>= 1.0,\n    plutus-tx ^>= 1.0,\n    plutus-core ^>= 1.0,\n    prettyprinter,\n    serialise,\n    set-algebra,\n    cardano-ledger-shelley,\n    scientific,\n    small-steps,\n    strict-containers,\n    text,\n    time,\n    transformers,\n    utf8-string,\n    validation-selective,\n  hs-source-dirs:\n    src\n";
    }