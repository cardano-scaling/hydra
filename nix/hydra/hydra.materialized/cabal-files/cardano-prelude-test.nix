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
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-prelude-test"; version = "0.1.0.1"; };
      license = "MIT";
      copyright = "2018-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Utility types and functions for testing Cardano";
      description = "Utility types and functions for testing Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      tests = {
        "prelude-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-prelude-test-0.1.0.1.tar.gz";
      sha256 = "c599f3fc68c52f9ab63d1dc6c09db961521a2b996d720baf211c560268252c93";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                 cardano-prelude-test\nversion:              0.1.0.1\nsynopsis:             Utility types and functions for testing Cardano\ndescription:          Utility types and functions for testing Cardano\nlicense:              MIT\nlicense-file:         LICENSE\nauthor:               IOHK\nmaintainer:           operations@iohk.io\ncopyright:            2018-2021 IOHK\ncategory:             Currency\nbuild-type:           Simple\n\nflag development\n  description: Disable `-Werror`\n  default: False\n  manual: True\n\nlibrary\n  hs-source-dirs:     src\n  exposed-modules:    Test.Cardano.Prelude\n  other-modules:      Test.Cardano.Prelude.Base16\n                      Test.Cardano.Prelude.Gen\n                      Test.Cardano.Prelude.Golden\n                      Test.Cardano.Prelude.Helpers\n                      Test.Cardano.Prelude.Orphans\n                      Test.Cardano.Prelude.QuickCheck.Arbitrary\n                      Test.Cardano.Prelude.QuickCheck.Property\n                      Test.Cardano.Prelude.Tripping\n\n  build-depends:      base\n                    , aeson\n                    , aeson-pretty >= 0.8.5\n                    , attoparsec\n                    , base16-bytestring >= 1\n                    , bytestring\n                    , canonical-json\n                    , cardano-prelude\n                    , containers\n                    , cryptonite\n                    , formatting\n                    , hedgehog\n                    , hspec\n                    , pretty-show\n                    , QuickCheck\n                    , quickcheck-instances\n                    , template-haskell\n                    , th-compat\n                    , text\n                    , time\n  default-language:   Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:        -Wall\n\n  if (!flag(development))\n    ghc-options:      -Werror\n \n-- NOTE: We need to keep the test-suite name short.\n--       If it's too long, we may exceed the path length\n--       on windows... somehow.\n-- => Keep the test-suite name short (and module names as well)\n--    to make this build on windows.\ntest-suite prelude-tests\n  hs-source-dirs:     test\n  main-is:            test.hs\n  type:               exitcode-stdio-1.0\n  other-modules:      Test.Cardano.Prelude.GHC.Heap.NormalFormSpec\n                      Test.Cardano.Prelude.GHC.Heap.SizeSpec\n                      Test.Cardano.Prelude.GHC.Heap.TreeSpec\n\n  build-depends:      base\n                    , bytestring\n                    , cardano-prelude\n                    , cardano-prelude-test\n                    , ghc-heap\n                    , ghc-prim\n                    , hedgehog\n                    , text\n  default-language:   Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:        -threaded\n                      -rtsopts\n                      -Wall\n\n  if (!flag(development))\n    ghc-options:      -Werror\n";
    }