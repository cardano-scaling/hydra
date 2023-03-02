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
      identifier = { name = "cardano-binary-test"; version = "1.3.0"; };
      license = "MIT";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-binary exposed to other packages";
      description = "Test helpers from cardano-binary exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-binary-test-1.3.0.tar.gz";
      sha256 = "88441c7c6111de6b54697129da39b4e2d187c404b3105e6c9690ca05e5495557";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-binary-test\nversion:             1.3.0\nsynopsis:            Test helpers from cardano-binary exposed to other packages\ndescription:         Test helpers from cardano-binary exposed to other packages\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2019-2021 IOHK\ncategory:            Currency\nbuild-type:          Simple\n\nflag development\n    description: Disable `-Werror`\n    default: False\n    manual: True\n\ncommon base                         { build-depends: base                             >= 4.14       && < 4.15     }\n\ncommon project-config\n  default-language:     Haskell2010\n  default-extensions:   NoImplicitPrelude\n\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wpartial-fields\n                        -Wredundant-constraints\n                        -Wunused-packages\n\n  if (!flag(development))\n    ghc-options:         -Werror\n\nlibrary\n  import:               base, project-config\n  exposed-modules:      Test.Cardano.Binary.Helpers\n                        Test.Cardano.Binary.Helpers.GoldenRoundTrip\n                        Test.Cardano.Binary.Serialization\n                        Test.Cardano.Binary.Drop\n                        Test.Cardano.Binary.Failure\n\n  build-depends:        base\n                      , bytestring\n                      , cardano-binary\n                      , cardano-prelude\n                      , cardano-prelude-test\n                      , cborg\n                      , containers\n                      , formatting\n                      , hedgehog\n                      , hspec\n                      , pretty-show\n                      , QuickCheck\n                      , quickcheck-instances\n                      , text\n                      , time\n                      , vector\n";
    }