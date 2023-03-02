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
      identifier = { name = "cardano-api"; version = "1.35.4"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The cardano api";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      sublibs = {
        "gen" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "cardano-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-api".components.sublibs.gen or (errorHandler.buildDepError "cardano-api:gen"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-api-1.35.4.tar.gz";
      sha256 = "8c5c6029fbcd6d0317b27ba81c1d20dce54dc68897dc039eac9d55631f0394e3";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                   cardano-api\nversion:                1.35.4\ndescription:            The cardano api\nauthor:                 IOHK\nmaintainer:             operations@iohk.io\nlicense:                Apache-2.0\nlicense-files:          LICENSE\n                        NOTICE\nbuild-type:             Simple\nextra-source-files:     README.md, ChangeLog.md\n\ncommon base                         { build-depends: base                             >= 4.14       && < 4.15     }\n\ncommon project-config\n  default-language:     Haskell2010\n  default-extensions:   NoImplicitPrelude\n                        OverloadedStrings\n\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wpartial-fields\n                        -Wredundant-constraints\n                        -Wunused-packages\n\ncommon maybe-unix\n  if !os(windows)\n     build-depends:    unix\n\nlibrary\n  import:               base, project-config, maybe-unix\n\n  hs-source-dirs:       src\n\n  exposed-modules:      Cardano.Api\n                        Cardano.Api.Byron\n                        Cardano.Api.ChainSync.Client\n                        Cardano.Api.ChainSync.ClientPipelined\n                        Cardano.Api.Crypto.Ed25519Bip32\n                        Cardano.Api.EraCast\n                        Cardano.Api.Protocol.Types\n                        Cardano.Api.Shelley\n                        -- TODO: Eliminate in the future when\n                        -- we create wrapper types for the ledger types\n                        -- in this module\n                        Cardano.Api.Orphans\n                        Cardano.Api.SerialiseTextEnvelope\n\n  other-modules:\n                        -- Splitting up the big Typed module:\n                        Cardano.Api.Address\n                        Cardano.Api.Block\n                        Cardano.Api.Certificate\n                        Cardano.Api.Convenience.Constraints\n                        Cardano.Api.Convenience.Construction\n                        Cardano.Api.Convenience.Query\n                        Cardano.Api.Environment\n                        Cardano.Api.Eras\n                        Cardano.Api.Error\n                        Cardano.Api.Fees\n                        Cardano.Api.GenesisParameters\n                        Cardano.Api.Hash\n                        Cardano.Api.HasTypeProxy\n                        Cardano.Api.IPC\n                        Cardano.Api.IPC.Monad\n                        Cardano.Api.InMode\n                        Cardano.Api.Json\n                        Cardano.Api.Key\n                        Cardano.Api.KeysByron\n                        Cardano.Api.KeysPraos\n                        Cardano.Api.KeysShelley\n                        Cardano.Api.LedgerEvent\n                        Cardano.Api.LedgerState\n                        Cardano.Api.Modes\n                        Cardano.Api.NetworkId\n                        Cardano.Api.OperationalCertificate\n                        Cardano.Api.ProtocolParameters\n                        Cardano.Api.Query\n                        Cardano.Api.Script\n                        Cardano.Api.ScriptData\n                        Cardano.Api.SerialiseBech32\n                        Cardano.Api.SerialiseCBOR\n                        Cardano.Api.SerialiseJSON\n                        Cardano.Api.SerialiseLedgerCddl\n                        Cardano.Api.SerialiseRaw\n                        Cardano.Api.SerialiseUsing\n                        Cardano.Api.Shelley.Genesis\n                        Cardano.Api.SpecialByron\n                        Cardano.Api.StakePoolMetadata\n                        Cardano.Api.Tx\n                        Cardano.Api.TxBody\n                        Cardano.Api.TxIn\n                        Cardano.Api.TxMetadata\n                        Cardano.Api.TxSubmit.ErrorRender\n                        Cardano.Api.TxSubmit.Types\n                        Cardano.Api.Utils\n                        Cardano.Api.Value\n                        Cardano.Api.ValueParser\n\n  build-depends:        aeson             >= 1.5.6.0\n                      , aeson-pretty      >= 0.8.5\n                      , attoparsec\n                      , array\n                      , base16-bytestring >= 1.0\n                      , base58-bytestring\n                      , bech32 >= 1.1.0\n                      , bytestring\n                      , cardano-binary\n                      , cardano-crypto\n                      , cardano-crypto-class\n                      , cardano-crypto-wrapper\n                      , cardano-data\n                      , cardano-ledger-alonzo\n                      , cardano-ledger-babbage\n                      , cardano-ledger-byron\n                      , cardano-ledger-core\n                      , cardano-ledger-shelley-ma\n                      , cardano-prelude\n                      , cardano-protocol-tpraos\n                      , cardano-slotting\n                      , cborg\n                      , vector-map\n                      , contra-tracer\n                      , containers\n                      , cryptonite\n                      , deepseq\n                      , directory\n                      , filepath\n                      , formatting\n                      , iproute\n                      , memory\n                      , network\n                      , nothunks\n                      , optparse-applicative-fork\n                      , ouroboros-consensus\n                      , ouroboros-consensus-byron\n                      , ouroboros-consensus-cardano\n                      , ouroboros-consensus-protocol\n                      , ouroboros-consensus-shelley\n                      , ouroboros-network\n                      , ouroboros-network-framework\n                      , parsec\n                      , plutus-ledger-api\n                      , prettyprinter\n                      , prettyprinter-configurable\n                      , scientific\n                      , serialise\n                      , small-steps\n                      , cardano-ledger-shelley\n                      , small-steps\n                      , stm\n                      , strict-containers\n                      , text\n                      , time\n                      , transformers\n                      , transformers-except\n                      , typed-protocols\n                      , unordered-containers >= 0.2.11\n                      , vector\n                      , yaml\n\nlibrary gen\n  import:               base, project-config\n\n  visibility:           public\n\n  hs-source-dirs:       gen\n\n  exposed-modules:      Gen.Cardano.Api\n                        Gen.Cardano.Api.Metadata\n                        Gen.Cardano.Api.Typed\n                        Gen.Cardano.Crypto.Seed\n                        Gen.Hedgehog.Roundtrip.Bech32\n                        Gen.Hedgehog.Roundtrip.CBOR\n\n  build-depends:        aeson             >= 1.5.6.0\n                      , base16-bytestring\n                      , bytestring\n                      , cardano-api\n                      , cardano-binary\n                      , cardano-crypto-class\n                      , cardano-crypto-test\n                      , cardano-ledger-alonzo\n                      , cardano-ledger-byron-test\n                      , cardano-ledger-core\n                      , cardano-prelude\n                      , containers\n                      , hedgehog\n                      , plutus-core\n                      , cardano-ledger-shelley\n                      , text\n\ntest-suite cardano-api-test\n  import:               base, project-config\n  hs-source-dirs:       test\n  main-is:              cardano-api-test.hs\n  type:                 exitcode-stdio-1.0\n\n  build-depends:        aeson             >= 1.5.6.0\n                      , bytestring\n                      , cardano-api\n                      , cardano-api:gen\n                      , cardano-binary\n                      , cardano-crypto\n                      , cardano-crypto-class\n                      , cardano-crypto-test\n                      , cardano-crypto-tests\n                      , cardano-ledger-core\n                      , cardano-prelude\n                      , cardano-slotting\n                      , containers\n                      , hedgehog\n                      , hedgehog-extras\n                      , ouroboros-consensus\n                      , ouroboros-consensus-shelley\n                      , QuickCheck\n                      , cardano-ledger-shelley\n                      , cardano-ledger-shelley-test\n                      , tasty\n                      , tasty-hedgehog\n                      , tasty-quickcheck\n                      , tasty-th\n                      , time\n\n  other-modules:        Test.Cardano.Api.Crypto\n                        Test.Cardano.Api.Genesis\n                        Test.Cardano.Api.Json\n                        Test.Cardano.Api.KeysByron\n                        Test.Cardano.Api.Ledger\n                        Test.Cardano.Api.Metadata\n                        Test.Cardano.Api.Typed.Address\n                        Test.Cardano.Api.Typed.Bech32\n                        Test.Cardano.Api.Typed.CBOR\n                        Test.Cardano.Api.Typed.Envelope\n                        Test.Cardano.Api.Typed.JSON\n                        Test.Cardano.Api.Typed.Ord\n                        Test.Cardano.Api.Typed.Orphans\n                        Test.Cardano.Api.Typed.RawBytes\n                        Test.Cardano.Api.Typed.Script\n                        Test.Cardano.Api.Typed.TxBody\n                        Test.Cardano.Api.Typed.Value\n\n  ghc-options:          -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T\n";
    }