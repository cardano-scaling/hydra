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
      identifier = { name = "cardano-ledger-byron-test"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-ledger exposed to other packages";
      description = "Test helpers from cardano-ledger exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-binary-test" or (errorHandler.buildDepError "cardano-binary-test"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."generic-monoid" or (errorHandler.buildDepError "generic-monoid"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-byron-test-1.3.0.tar.gz";
      sha256 = "9dad6c73eb56bab473dfb9007236c388f9a4ed1724e4885800219fbcf4cf2a20";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-byron-test\nversion:             1.3.0\nsynopsis:            Test helpers from cardano-ledger exposed to other packages\ndescription:         Test helpers from cardano-ledger exposed to other packages\nlicense:             Apache-2.0\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2018 IOHK\ncategory:            Currency\nbuild-type:          Simple\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n  default-extensions: NoImplicitPrelude\n\n  ghc-options:        -Weverything\n                      -Wno-all-missed-specialisations\n                      -Wno-missing-deriving-strategies\n                      -Wno-missing-import-lists\n                      -Wno-missing-safe-haskell-mode\n                      -Wno-prepositive-qualified-module\n                      -Wno-safe\n                      -Wno-unsafe\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:\n                       Test.Cardano.Chain.Block.CBOR\n                       Test.Cardano.Chain.Block.Gen\n                       Test.Cardano.Chain.Block.Model\n                       Test.Cardano.Chain.Block.Model.Examples\n                       Test.Cardano.Chain.Block.Validation\n                       Test.Cardano.Chain.Block.ValidationMode\n\n                       Test.Cardano.Chain.Byron.API\n\n                       Test.Cardano.Chain.Buildable\n\n                       Test.Cardano.Chain.Common.Address\n                       Test.Cardano.Chain.Common.CBOR\n                       Test.Cardano.Chain.Common.Compact\n                       Test.Cardano.Chain.Common.Example\n                       Test.Cardano.Chain.Common.Gen\n                       Test.Cardano.Chain.Common.Lovelace\n                       Test.Cardano.Chain.Config\n\n                       Test.Cardano.Chain.Delegation.CBOR\n                       Test.Cardano.Chain.Delegation.Certificate\n                       Test.Cardano.Chain.Delegation.Example\n                       Test.Cardano.Chain.Delegation.Gen\n                       Test.Cardano.Chain.Delegation.Model\n\n                       Test.Cardano.Chain.Elaboration.Block\n                       Test.Cardano.Chain.Elaboration.Delegation\n                       Test.Cardano.Chain.Elaboration.Keys\n                       Test.Cardano.Chain.Elaboration.Update\n                       Test.Cardano.Chain.Elaboration.UTxO\n\n                       Test.Cardano.Chain.Epoch.File\n\n                       Test.Cardano.Chain.Genesis.CBOR\n                       Test.Cardano.Chain.Genesis.Dummy\n                       Test.Cardano.Chain.Genesis.Example\n                       Test.Cardano.Chain.Genesis.Gen\n                       Test.Cardano.Chain.Genesis.Json\n\n                       Test.Cardano.Chain.MempoolPayload.CBOR\n                       Test.Cardano.Chain.MempoolPayload.Example\n                       Test.Cardano.Chain.MempoolPayload.Gen\n\n                       Test.Cardano.Chain.Ssc.CBOR\n\n                       Test.Cardano.Chain.Slotting.CBOR\n                       Test.Cardano.Chain.Slotting.Example\n                       Test.Cardano.Chain.Slotting.Gen\n                       Test.Cardano.Chain.Slotting.Properties\n\n                       Test.Cardano.Chain.UTxO.CBOR\n                       Test.Cardano.Chain.UTxO.Compact\n                       Test.Cardano.Chain.UTxO.Example\n                       Test.Cardano.Chain.UTxO.Gen\n                       Test.Cardano.Chain.UTxO.Model\n                       Test.Cardano.Chain.UTxO.ValidationMode\n\n                       Test.Cardano.Chain.Update.CBOR\n                       Test.Cardano.Chain.Update.Example\n                       Test.Cardano.Chain.Update.Gen\n                       Test.Cardano.Chain.Update.Properties\n\n                       Test.Cardano.Mirror\n\n                       Test.Options\n\n  build-depends:       base16-bytestring >= 1\n                     , bimap\n                     , bytestring\n                     , cardano-binary\n                     , cardano-binary-test\n                     , cardano-ledger-byron\n                     , cardano-crypto\n                     , cardano-crypto-test\n                     , cardano-crypto-wrapper\n                     , cardano-prelude\n                     , cardano-prelude-test\n                     , containers\n                     , byron-spec-chain\n                     , byron-spec-ledger\n                     , directory\n                     , filepath\n                     , formatting\n                     , generic-monoid\n                     , hedgehog >= 1.0.4\n                     , microlens\n                     , resourcet\n                     , small-steps\n                     , small-steps-test\n                     , streaming\n                     , tasty\n                     , tasty-hedgehog\n                     , text\n                     , time\n                     , vector\n";
    }