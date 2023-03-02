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
      identifier = { name = "cardano-ledger-shelley-ma"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Shelley ledger with multiasset and time lock support.";
      description = "This package extends the Shelley ledger with support for\nnative tokens and timelocks.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
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
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-shelley-ma-0.1.0.0.tar.gz";
      sha256 = "18f881c00afbd56ca5b534e82403612c0f9701f7189dd26ca7ce062db5ba8d05";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-shelley-ma\nversion:             0.1.0.0\nsynopsis:            Shelley ledger with multiasset and time lock support.\ndescription:\n  This package extends the Shelley ledger with support for\n  native tokens and timelocks.\nbug-reports:         https://github.com/input-output-hk/cardano-ledger/issues\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncopyright:           2020 Input Output (Hong Kong) Ltd.\ncategory:            Network\nbuild-type:          Simple\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   eras/shelley-ma/impl\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wpartial-fields\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:\n    Cardano.Ledger.Allegra\n    Cardano.Ledger.Allegra.Translation\n    Cardano.Ledger.Mary\n    Cardano.Ledger.Mary.Translation\n    Cardano.Ledger.Mary.Value\n    Cardano.Ledger.ShelleyMA\n    Cardano.Ledger.ShelleyMA.AuxiliaryData\n    Cardano.Ledger.ShelleyMA.Rules.EraMapping\n    Cardano.Ledger.ShelleyMA.Rules.Utxo\n    Cardano.Ledger.ShelleyMA.Rules.Utxow\n    Cardano.Ledger.ShelleyMA.Timelocks\n    Cardano.Ledger.ShelleyMA.TxBody\n\n  build-depends:\n    bytestring,\n    base16-bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-core,\n    cardano-slotting,\n    cborg,\n    containers,\n    data-default-class,\n    deepseq,\n    groups,\n    mtl,\n    nothunks,\n    primitive,\n    cardano-ledger-shelley,\n    small-steps,\n    strict-containers,\n    text,\n    transformers,\n    validation-selective,\n  hs-source-dirs: src\n";
    }