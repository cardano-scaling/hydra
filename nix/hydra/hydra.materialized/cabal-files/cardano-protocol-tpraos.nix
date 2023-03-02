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
      specVersion = "2.4";
      identifier = { name = "cardano-protocol-tpraos"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Cardano Protocol: Transitional Praos";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-protocol-tpraos-0.1.0.0.tar.gz";
      sha256 = "667b633418ab89ba1e09bb5088cf137ca5cd2de32c97d93aca6c669e3a6a7c6b";
      });
    }) // {
    package-description-override = "cabal-version:      2.4\nname:               cardano-protocol-tpraos\nversion:            0.1.0.0\ndescription:        Cardano Protocol: Transitional Praos\nbug-reports:        https://github.com/input-output-hk/cardano-ledger/issues\n\nlicense:            Apache-2.0\nauthor:             IOHK Formal Methods Team\nmaintainer:         formal.methods@iohk.io\ncopyright:          2021 Input Output (Hong Kong) Ltd.\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/cardano-protocol-tpraos\n\ncommon base\n  build-depends:\n    base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language: Haskell2010\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\nlibrary\n  import:\n    base, project-config\n\n  hs-source-dirs: src\n\n  exposed-modules:\n    Cardano.Protocol.TPraos.API\n    Cardano.Protocol.TPraos.BHeader\n    Cardano.Protocol.TPraos.OCert\n    Cardano.Protocol.TPraos.Rules.OCert\n    Cardano.Protocol.TPraos.Rules.Overlay\n    Cardano.Protocol.TPraos.Rules.Prtcl\n    Cardano.Protocol.TPraos.Rules.Tickn\n    Cardano.Protocol.TPraos.Rules.Updn\n\n  build-depends:\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-ledger-alonzo,\n    cardano-ledger-babbage,\n    cardano-ledger-core,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-ma,\n    cardano-slotting,\n    containers,\n    deepseq,\n    nothunks,\n    mtl,\n    quiet,\n    non-integral,\n    set-algebra,\n    small-steps,\n    transformers,\n";
    }