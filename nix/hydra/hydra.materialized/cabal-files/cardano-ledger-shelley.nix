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
      identifier = { name = "cardano-ledger-shelley"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Shelley Ledger Executable Model";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
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
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-shelley-0.1.0.0.tar.gz";
      sha256 = "f849e607f0df37493db5a0ca4aa4795edec44d1efc00f136b58daa66fb864568";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-shelley\nversion:             0.1.0.0\ndescription:         Shelley Ledger Executable Model\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\nbuild-type:          Simple\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/cardano-ledger.git\n  subdir:   eras/shelley/impl\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wpartial-fields\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n  exposed-modules:\n    Cardano.Ledger.Chain\n    Cardano.Ledger.Shelley\n    Cardano.Ledger.Shelley.Constraints\n    Cardano.Ledger.Shelley.Address.Bootstrap\n    Cardano.Ledger.Shelley.API\n    Cardano.Ledger.Shelley.API.ByronTranslation\n    Cardano.Ledger.Shelley.API.Genesis\n    Cardano.Ledger.Shelley.API.Validation\n    Cardano.Ledger.Shelley.API.Wallet\n    Cardano.Ledger.Shelley.API.Mempool\n    Cardano.Ledger.Shelley.API.Types\n    Cardano.Ledger.Shelley.AdaPots\n    Cardano.Ledger.Shelley.BlockChain\n    Cardano.Ledger.Shelley.CompactAddr\n    Cardano.Ledger.Shelley.Delegation.Certificates\n    Cardano.Ledger.Shelley.Delegation.PoolParams\n    Cardano.Ledger.Shelley.EpochBoundary\n    Cardano.Ledger.Shelley.Genesis\n    Cardano.Ledger.Shelley.HardForks\n    Cardano.Ledger.Shelley.LedgerState\n    Cardano.Ledger.Shelley.Metadata\n    Cardano.Ledger.Shelley.Orphans\n    Cardano.Ledger.Shelley.PoolRank\n    Cardano.Ledger.Shelley.PParams\n    Cardano.Ledger.Shelley.Rewards\n    Cardano.Ledger.Shelley.RewardProvenance\n    Cardano.Ledger.Shelley.RewardUpdate\n    Cardano.Ledger.Shelley.Scripts\n    Cardano.Ledger.Shelley.SoftForks\n    Cardano.Ledger.Shelley.StabilityWindow\n    Cardano.Ledger.Shelley.Rules.Bbody\n    Cardano.Ledger.Shelley.Rules.Deleg\n    Cardano.Ledger.Shelley.Rules.Delegs\n    Cardano.Ledger.Shelley.Rules.Delpl\n    Cardano.Ledger.Shelley.Rules.Epoch\n    Cardano.Ledger.Shelley.Rules.EraMapping\n    Cardano.Ledger.Shelley.Rules.Ledger\n    Cardano.Ledger.Shelley.Rules.Ledgers\n    Cardano.Ledger.Shelley.Rules.Mir\n    Cardano.Ledger.Shelley.Rules.NewEpoch\n    Cardano.Ledger.Shelley.Rules.Newpp\n    Cardano.Ledger.Shelley.Rules.Pool\n    Cardano.Ledger.Shelley.Rules.PoolReap\n    Cardano.Ledger.Shelley.Rules.Ppup\n    Cardano.Ledger.Shelley.Rules.Rupd\n    Cardano.Ledger.Shelley.Rules.Snap\n    Cardano.Ledger.Shelley.Rules.Tick\n    Cardano.Ledger.Shelley.Rules.Upec\n    Cardano.Ledger.Shelley.Rules.Utxo\n    Cardano.Ledger.Shelley.Rules.Utxow\n    Cardano.Ledger.Shelley.Tx\n    Cardano.Ledger.Shelley.TxBody\n    Cardano.Ledger.Shelley.UTxO\n  hs-source-dirs: src\n  build-depends:\n    aeson >= 2,\n    base16-bytestring >= 1,\n    bytestring,\n    cardano-binary,\n    cardano-crypto,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-data,\n    cardano-ledger-byron,\n    cardano-ledger-core,\n    cardano-prelude,\n    cardano-slotting,\n    cborg,\n    vector-map,\n    constraints,\n    containers,\n    data-default-class,\n    deepseq,\n    groups,\n    iproute,\n    mtl,\n    microlens,\n    nothunks,\n    quiet,\n    set-algebra,\n    small-steps,\n    strict-containers,\n    text,\n    time,\n    transformers,\n    validation-selective,\n";
    }