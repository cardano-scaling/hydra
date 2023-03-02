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
      identifier = { name = "cardano-crypto-wrapper"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cryptographic primitives used in the Cardano project";
      description = "Cryptographic primitives used in the Cardano project";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."base64-bytestring-type" or (errorHandler.buildDepError "base64-bytestring-type"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-binary-test" or (errorHandler.buildDepError "cardano-binary-test"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-crypto-wrapper-1.3.0.tar.gz";
      sha256 = "a618a1a90fa85dac8dade50a5f3cc29906534f45f7f5100b57b001f157f1db44";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-crypto-wrapper\nversion:             1.3.0\nsynopsis:            Cryptographic primitives used in the Cardano project\ndescription:         Cryptographic primitives used in the Cardano project\nlicense:             Apache-2.0\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2019 IOHK\ncategory:            Currency\nbuild-type:          Simple\nextra-source-files:  README.md\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n  default-extensions: NoImplicitPrelude\n\n  ghc-options:        -Weverything\n                      -Wno-all-missed-specialisations\n                      -Wno-missing-deriving-strategies\n                      -Wno-missing-import-lists\n                      -Wno-missing-safe-haskell-mode\n                      -Wno-prepositive-qualified-module\n                      -Wno-safe\n                      -Wno-unsafe\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  hs-source-dirs:      src\n  exposed-modules:\n                       Cardano.Crypto\n\n                       Cardano.Crypto.Hashing\n                       Cardano.Crypto.Orphans\n                       Cardano.Crypto.ProtocolMagic\n                       Cardano.Crypto.Random\n                       Cardano.Crypto.Signing\n                       Cardano.Crypto.Signing.Redeem\n                       Cardano.Crypto.Signing.Safe\n\n  other-modules:\n                       Cardano.Crypto.Signing.Tag\n\n                       Cardano.Crypto.Signing.KeyGen\n                       Cardano.Crypto.Signing.VerificationKey\n                       Cardano.Crypto.Signing.SigningKey\n                       Cardano.Crypto.Signing.Signature\n\n                       Cardano.Crypto.Signing.Redeem.Compact\n                       Cardano.Crypto.Signing.Redeem.KeyGen\n                       Cardano.Crypto.Signing.Redeem.SigningKey\n                       Cardano.Crypto.Signing.Redeem.Signature\n                       Cardano.Crypto.Signing.Redeem.VerificationKey\n\n                       Cardano.Crypto.Signing.Safe.KeyGen\n                       Cardano.Crypto.Signing.Safe.PassPhrase\n                       Cardano.Crypto.Signing.Safe.SafeSigner\n\n  build-depends:       aeson\n                     , base16-bytestring >= 1\n                     , base64-bytestring\n                     , base64-bytestring-type\n                     , binary\n                     , bytestring\n                     , canonical-json\n                     , cardano-binary\n                     , cardano-crypto\n                     , cardano-prelude\n                     , cryptonite\n                     , data-default\n                     , formatting\n                     , memory\n                     , mtl\n                     , nothunks\n                     , text\n\ntest-suite test\n  import:             base, project-config\n\n  hs-source-dirs:      test\n  main-is:             test.hs\n  type:                exitcode-stdio-1.0\n\n  other-modules:\n                       Test.Cardano.Crypto.CBOR\n                       Test.Cardano.Crypto.Dummy\n                       Test.Cardano.Crypto.Example\n                       Test.Cardano.Crypto.Gen\n                       Test.Cardano.Crypto.Hashing\n                       Test.Cardano.Crypto.Json\n                       Test.Cardano.Crypto.Keys\n                       Test.Cardano.Crypto.Limits\n                       Test.Cardano.Crypto.Orphans\n                       Test.Cardano.Crypto.Random\n                       Test.Cardano.Crypto.Signing.Redeem\n                       Test.Cardano.Crypto.Signing.Redeem.Compact\n                       Test.Cardano.Crypto.Signing.Safe\n                       Test.Cardano.Crypto.Signing.Signing\n\n  build-depends:       bytestring\n                     , cardano-binary\n                     , cardano-binary-test\n                     , cardano-crypto\n                     , cardano-crypto-wrapper\n                     , cardano-prelude\n                     , cardano-prelude-test\n                     , cryptonite\n                     , formatting\n                     , hedgehog >= 1.0.4\n                     , memory\n                     , text\n\n  ghc-options:         -threaded\n                       -rtsopts\n";
    }