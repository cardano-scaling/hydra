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
      identifier = { name = "cardano-crypto-test"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-crypto exposed to other packages";
      description = "Test helpers from cardano-crypto exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
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
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-crypto-test-1.3.0.tar.gz";
      sha256 = "e70f9b3a5f1b5ea298a3f88c269890462472fc5ff13e244af04ac2e8fe0c18d6";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-crypto-test\nversion:             1.3.0\nsynopsis:            Test helpers from cardano-crypto exposed to other packages\ndescription:         Test helpers from cardano-crypto exposed to other packages\nlicense:             Apache-2.0\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2018 IOHK\ncategory:            Currency\nbuild-type:          Simple\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  default-extensions: NoImplicitPrelude\n\n  ghc-options:        -Weverything\n                      -Wno-all-missed-specialisations\n                      -Wno-missing-deriving-strategies\n                      -Wno-missing-import-lists\n                      -Wno-missing-safe-haskell-mode\n                      -Wno-prepositive-qualified-module\n                      -Wno-safe\n                      -Wno-unsafe\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:\n                       Test.Cardano.Crypto.CBOR\n                       Test.Cardano.Crypto.Dummy\n                       Test.Cardano.Crypto.Example\n                       Test.Cardano.Crypto.Gen\n                       Test.Cardano.Crypto.Json\n                       Test.Cardano.Crypto.Orphans\n\n  build-depends:       bytestring\n                     , cardano-binary\n                     , cardano-binary-test\n                     , cardano-crypto\n                     , cardano-crypto-wrapper\n                     , cardano-prelude\n                     , cardano-prelude-test\n                     , cryptonite\n                     , hedgehog >= 1.0.4\n                     , memory\n";
    }