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
      identifier = { name = "cardano-ledger-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Core components of Cardano ledgers from the Shelley release on.";
      description = "Cardano ledgers from the Shelley release onwards share a core basis rooted in\nthe Shelley ledger specification. This package abstracts a number of components\nwhich we expect to be shared amongst all future ledgers implemented around this base.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."partial-order" or (errorHandler.buildDepError "partial-order"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-core-0.1.0.0.tar.gz";
      sha256 = "c0aea7faccf8c6a11647526fe41d92b9003be747c570a1074183bc2a89cf4afc";
      });
    }) // {
    package-description-override = "cabal-version:      2.4\nname:               cardano-ledger-core\nversion:            0.1.0.0\nsynopsis:           Core components of Cardano ledgers from the Shelley release on.\ndescription:\n  Cardano ledgers from the Shelley release onwards share a core basis rooted in\n  the Shelley ledger specification. This package abstracts a number of components\n  which we expect to be shared amongst all future ledgers implemented around this base.\nbug-reports:        https://github.com/input-output-hk/cardano-ledger/issues\n\nlicense:            Apache-2.0\nauthor:             IOHK Formal Methods Team\nmaintainer:         formal.methods@iohk.io\ncopyright:          2021 Input Output (Hong Kong) Ltd.\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/cardano-ledger-core\n\ncommon base\n  build-depends:\n    base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language: Haskell2010\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\nlibrary\n  import:\n    base, project-config\n\n  hs-source-dirs: src\n\n  exposed-modules:\n    Cardano.Ledger.Address\n    Cardano.Ledger.CompactAddress\n    Cardano.Ledger.AuxiliaryData\n    Cardano.Ledger.BaseTypes\n    Cardano.Ledger.BHeaderView\n    Cardano.Ledger.Block\n    Cardano.Ledger.Coin\n    Cardano.Ledger.Compactible\n    Cardano.Ledger.Core\n    Cardano.Ledger.Credential\n    Cardano.Ledger.Crypto\n    Cardano.Ledger.Era\n    Cardano.Ledger.Hashes\n    Cardano.Ledger.Keys\n    Cardano.Ledger.PoolDistr\n    Cardano.Ledger.Rules.ValidationMode\n    Cardano.Ledger.SafeHash\n    Cardano.Ledger.Serialization\n    Cardano.Ledger.Slot\n    Cardano.Ledger.TxIn\n    Cardano.Ledger.UnifiedMap\n    Cardano.Ledger.Val\n\n  build-depends:\n    aeson >= 2,\n    base16-bytestring,\n    binary,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-crypto-praos,\n    cardano-crypto-wrapper,\n    cardano-data,\n    cardano-ledger-byron,\n    cardano-prelude,\n    cardano-slotting,\n    containers,\n    data-default-class,\n    deepseq,\n    groups,\n    iproute,\n    mtl,\n    network,\n    nothunks,\n    partial-order,\n    quiet,\n    scientific,\n    set-algebra,\n    non-integral,\n    primitive,\n    small-steps,\n    strict-containers,\n    text,\n    time,\n    transformers,\n    validation-selective,\n";
    }