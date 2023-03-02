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
    flags = { test-normal-form = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-ledger-byron"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Cardano during the Byron era";
      description = "The blockchain layer of Cardano during the Byron era";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."streaming-binary" or (errorHandler.buildDepError "streaming-binary"))
          (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "cardano-ledger-byron-test" = {
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
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
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
        "epoch-validation-normal-form-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-ledger" or (errorHandler.buildDepError "cardano-ledger"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = if !flags.test-normal-form then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-byron-0.1.0.0.tar.gz";
      sha256 = "a22d5740406122c2ba1f6a98fddda7afdd40bf30fa3e1f03243a10af96a4785e";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-byron\nversion:             0.1.0.0\nsynopsis:            The blockchain layer of Cardano during the Byron era\ndescription:         The blockchain layer of Cardano during the Byron era\nlicense:             Apache-2.0\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2018 IOHK\ncategory:            Currency\nbuild-type:          Simple\nextra-source-files:  README.md\n\nflag test-normal-form\n    description: Test ledger state normal form during epoch validation\n    default: False\n    manual: True\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n  default-extensions: NoImplicitPrelude\n\n  ghc-options:        -Weverything\n                      -Wno-all-missed-specialisations\n                      -Wno-missing-deriving-strategies\n                      -Wno-missing-import-lists\n                      -Wno-missing-safe-haskell-mode\n                      -Wno-prepositive-qualified-module\n                      -Wno-safe\n                      -Wno-unsafe\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  hs-source-dirs:      src\n  exposed-modules:\n                       Cardano.Chain.Block\n                       Cardano.Chain.Byron.API\n                       Cardano.Chain.Common\n                       Cardano.Chain.Constants\n                       Cardano.Chain.Delegation\n                       Cardano.Chain.Delegation.Validation.Activation\n                       Cardano.Chain.Delegation.Validation.Interface\n                       Cardano.Chain.Delegation.Validation.Scheduling\n                       Cardano.Chain.Epoch.File\n                       Cardano.Chain.Epoch.Validation\n                       Cardano.Chain.Genesis\n                       Cardano.Chain.MempoolPayload\n                       Cardano.Chain.ProtocolConstants\n                       Cardano.Chain.Slotting\n                       Cardano.Chain.Ssc\n                       Cardano.Chain.UTxO\n                       Cardano.Chain.UTxO.UTxO\n                       Cardano.Chain.UTxO.Validation\n                       Cardano.Chain.Update\n                       Cardano.Chain.Update.Proposal\n                       Cardano.Chain.Update.Validation.Endorsement\n                       Cardano.Chain.Update.Validation.Interface\n                       Cardano.Chain.Update.Validation.Registration\n                       Cardano.Chain.Update.Validation.Voting\n                       Cardano.Chain.Update.Vote\n                       Cardano.Chain.ValidationMode\n\n  other-modules:\n                       Cardano.Chain.Block.Block\n                       Cardano.Chain.Block.Body\n                       Cardano.Chain.Block.Boundary\n                       Cardano.Chain.Block.Header\n                       Cardano.Chain.Block.Proof\n                       Cardano.Chain.Block.Validation\n                       Cardano.Chain.Block.ValidationMode\n\n                       Cardano.Chain.Byron.API.Common\n                       Cardano.Chain.Byron.API.Mempool\n                       Cardano.Chain.Byron.API.Protocol\n                       Cardano.Chain.Byron.API.Validation\n\n                       Cardano.Chain.Common.AddrAttributes\n                       Cardano.Chain.Common.AddrSpendingData\n                       Cardano.Chain.Common.Address\n                       Cardano.Chain.Common.AddressHash\n                       Cardano.Chain.Common.Attributes\n                       Cardano.Chain.Common.BlockCount\n                       Cardano.Chain.Common.CBOR\n                       Cardano.Chain.Common.ChainDifficulty\n                       Cardano.Chain.Common.Compact\n                       Cardano.Chain.Common.KeyHash\n                       Cardano.Chain.Common.Lovelace\n                       Cardano.Chain.Common.LovelacePortion\n                       Cardano.Chain.Common.Merkle\n                       Cardano.Chain.Common.NetworkMagic\n                       Cardano.Chain.Common.TxFeePolicy\n                       Cardano.Chain.Common.TxSizeLinear\n\n                       Cardano.Chain.Delegation.Certificate\n                       Cardano.Chain.Delegation.Map\n                       Cardano.Chain.Delegation.Payload\n\n                       Cardano.Chain.Genesis.AvvmBalances\n                       Cardano.Chain.Genesis.Config\n                       Cardano.Chain.Genesis.Data\n                       Cardano.Chain.Genesis.Delegation\n                       Cardano.Chain.Genesis.Generate\n                       Cardano.Chain.Genesis.Hash\n                       Cardano.Chain.Genesis.Initializer\n                       Cardano.Chain.Genesis.KeyHashes\n                       Cardano.Chain.Genesis.NonAvvmBalances\n                       Cardano.Chain.Genesis.Spec\n\n                       Cardano.Chain.Slotting.EpochAndSlotCount\n                       Cardano.Chain.Slotting.EpochNumber\n                       Cardano.Chain.Slotting.EpochSlots\n                       Cardano.Chain.Slotting.SlotCount\n                       Cardano.Chain.Slotting.SlotNumber\n\n                       Cardano.Chain.UTxO.Compact\n                       Cardano.Chain.UTxO.GenesisUTxO\n                       Cardano.Chain.UTxO.Tx\n                       Cardano.Chain.UTxO.TxAux\n                       Cardano.Chain.UTxO.TxPayload\n                       Cardano.Chain.UTxO.UTxOConfiguration\n                       Cardano.Chain.UTxO.TxProof\n                       Cardano.Chain.UTxO.TxWitness\n                       Cardano.Chain.UTxO.ValidationMode\n\n                       Cardano.Chain.Update.ApplicationName\n                       Cardano.Chain.Update.InstallerHash\n                       Cardano.Chain.Update.Payload\n                       Cardano.Chain.Update.Proof\n                       Cardano.Chain.Update.ProtocolParameters\n                       Cardano.Chain.Update.ProtocolParametersUpdate\n                       Cardano.Chain.Update.ProtocolVersion\n                       Cardano.Chain.Update.SoftforkRule\n                       Cardano.Chain.Update.SoftwareVersion\n                       Cardano.Chain.Update.SystemTag\n                       Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump\n\n  build-depends:       aeson\n                     , base58-bytestring\n                     , bimap >=0.4 && <0.5\n                     , binary\n                     , bytestring\n                     , canonical-json\n                     , cardano-binary\n                     , cardano-crypto\n                     , cardano-crypto-wrapper\n                     , cardano-prelude\n                     , cborg\n                     , containers\n                     , contra-tracer\n                     , cryptonite\n                     , Cabal\n                     , deepseq\n                     , digest\n                     , directory\n                     , filepath\n                     , formatting\n                     , mtl\n                     , nothunks\n                     , quiet\n                     , resourcet\n                     , streaming\n                     , streaming-binary >=0.2 && <0.3\n                     , streaming-bytestring\n                     , text\n                     , time\n                     , vector\n\ntest-suite cardano-ledger-byron-test\n  import:             base, project-config\n\n  hs-source-dirs:      test\n  main-is:             test.hs\n  type:                exitcode-stdio-1.0\n\n  other-modules:\n\n                       Test.Cardano.Chain.Block.CBOR\n                       Test.Cardano.Chain.Block.Gen\n                       Test.Cardano.Chain.Block.Model\n                       Test.Cardano.Chain.Block.Model.Examples\n                       Test.Cardano.Chain.Block.Size\n                       Test.Cardano.Chain.Block.Validation\n                       Test.Cardano.Chain.Block.ValidationMode\n                       Test.Cardano.Chain.Byron.API\n\n                       Test.Cardano.Chain.Buildable\n\n                       Test.Cardano.Chain.Common.Address\n                       Test.Cardano.Chain.Common.Attributes\n                       Test.Cardano.Chain.Common.CBOR\n                       Test.Cardano.Chain.Common.Compact\n                       Test.Cardano.Chain.Common.Example\n                       Test.Cardano.Chain.Common.Gen\n                       Test.Cardano.Chain.Common.Lovelace\n                       Test.Cardano.Chain.Config\n\n                       Test.Cardano.Chain.Delegation.CBOR\n                       Test.Cardano.Chain.Delegation.Certificate\n                       Test.Cardano.Chain.Delegation.Example\n                       Test.Cardano.Chain.Delegation.Gen\n                       Test.Cardano.Chain.Delegation.Model\n\n                       Test.Cardano.Chain.Elaboration.Block\n                       Test.Cardano.Chain.Elaboration.Delegation\n                       Test.Cardano.Chain.Elaboration.Keys\n                       Test.Cardano.Chain.Elaboration.Update\n                       Test.Cardano.Chain.Elaboration.UTxO\n\n                       Test.Cardano.Chain.Epoch.File\n\n                       Test.Cardano.Chain.Genesis.CBOR\n                       Test.Cardano.Chain.Genesis.Dummy\n                       Test.Cardano.Chain.Genesis.Example\n                       Test.Cardano.Chain.Genesis.Gen\n                       Test.Cardano.Chain.Genesis.Json\n\n                       Test.Cardano.Chain.MempoolPayload.CBOR\n                       Test.Cardano.Chain.MempoolPayload.Example\n                       Test.Cardano.Chain.MempoolPayload.Gen\n\n                       Test.Cardano.Chain.Ssc.CBOR\n\n                       Test.Cardano.Chain.Slotting.CBOR\n                       Test.Cardano.Chain.Slotting.Example\n                       Test.Cardano.Chain.Slotting.Gen\n                       Test.Cardano.Chain.Slotting.Properties\n\n                       Test.Cardano.Chain.UTxO.CBOR\n                       Test.Cardano.Chain.UTxO.Compact\n                       Test.Cardano.Chain.UTxO.Example\n                       Test.Cardano.Chain.UTxO.Gen\n                       Test.Cardano.Chain.UTxO.Model\n                       Test.Cardano.Chain.UTxO.ValidationMode\n\n                       Test.Cardano.Chain.Update.CBOR\n                       Test.Cardano.Chain.Update.Example\n                       Test.Cardano.Chain.Update.Gen\n                       Test.Cardano.Chain.Update.Properties\n\n                       Test.Cardano.Mirror\n\n                       Test.Options\n\n  build-depends:       base16-bytestring >= 1\n                     , bimap >=0.4 && <0.5\n                     , bytestring\n                     , cardano-binary\n                     , cardano-binary-test\n                     , cardano-ledger-byron\n                     , cardano-crypto\n                     , cardano-crypto-test\n                     , cardano-crypto-wrapper\n                     , cardano-prelude\n                     , cardano-prelude-test\n                     , cborg\n                     , containers\n                     , byron-spec-chain\n                     , byron-spec-ledger\n                     , directory\n                     , filepath\n                     , formatting\n                     , generic-monoid\n                     , hedgehog >= 1.0.4\n                     , microlens\n                     , resourcet\n                     , small-steps\n                     , small-steps-test\n                     , streaming\n                     , tasty\n                     , tasty-hedgehog\n                     , text\n                     , time\n                     , vector\n\n  ghc-options:         \"-with-rtsopts=-K450K -M500M\"\n\ntest-suite epoch-validation-normal-form-test\n  import:             base, project-config\n\n  if (!flag(test-normal-form))\n   buildable: False\n\n  hs-source-dirs:      test\n  main-is:             NormalFormTest.hs\n  type:                exitcode-stdio-1.0\n\n  other-modules:\n                       Test.Cardano.Chain.Block.Validation\n                       Test.Cardano.Chain.Config\n                       Test.Cardano.Mirror\n                       Test.Options\n\n  build-depends:       bytestring\n                     , cardano-binary\n                     , cardano-ledger\n                     , cardano-crypto-wrapper\n                     , cardano-prelude\n                     , cardano-prelude-test\n                     , directory\n                     , filepath\n                     , hedgehog >= 1.0.4\n                     , resourcet\n                     , silently\n                     , streaming\n                     , tasty\n                     , tasty-hedgehog\n\n  ghc-options:         \"-with-rtsopts=-K450K -M500M\"\n";
    }