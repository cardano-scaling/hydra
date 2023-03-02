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
      specVersion = "1.8";
      identifier = { name = "hedgehog-quickcheck"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Jacob Stanley";
      homepage = "https://hedgehog.qa";
      url = "";
      synopsis = "Use QuickCheck generators in Hedgehog and vice versa.";
      description = "Use QuickCheck generators in Hedgehog and vice versa.\n\nHedgehog is a modern property-based testing system, in the spirit of\nQuickCheck. Hedgehog uses integrated shrinking, so shrinks obey the\ninvariants of generated values by construction.\n\nTo get started quickly, see the examples:\n<https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hedgehog-quickcheck-0.1.1.tar.gz";
      sha256 = "97b65db815fdfaacc7c4d06a7b9b74680b50264afa03f839c4037dcc875152fc";
      });
    }) // {
    package-description-override = "version: 0.1.1\r\nx-revision: 4\r\n\r\nname:\r\n  hedgehog-quickcheck\r\nauthor:\r\n  Jacob Stanley\r\nmaintainer:\r\n  Jacob Stanley <jacob@stanley.io>\r\nhomepage:\r\n  https://hedgehog.qa\r\nbug-reports:\r\n  https://github.com/hedgehogqa/haskell-hedgehog/issues\r\nsynopsis:\r\n  Use QuickCheck generators in Hedgehog and vice versa.\r\ndescription:\r\n  Use QuickCheck generators in Hedgehog and vice versa.\r\n  .\r\n  Hedgehog is a modern property-based testing system, in the spirit of\r\n  QuickCheck. Hedgehog uses integrated shrinking, so shrinks obey the\r\n  invariants of generated values by construction.\r\n  .\r\n  To get started quickly, see the examples:\r\n  <https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-\r\n\r\ncategory:\r\n  Testing\r\nlicense:\r\n  BSD3\r\nlicense-file:\r\n  LICENSE\r\ncabal-version:\r\n  >= 1.8\r\nbuild-type:\r\n  Simple\r\ntested-with:\r\n    GHC == 8.0.2\r\n  , GHC == 8.2.2\r\n  , GHC == 8.4.4\r\n  , GHC == 8.6.5\r\n  , GHC == 8.8.3\r\n  , GHC == 8.10.1\r\nextra-source-files:\r\n  README.md\r\n  CHANGELOG.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/hedgehogqa/haskell-hedgehog.git\r\n\r\nlibrary\r\n  build-depends:\r\n      base                            >= 3          && < 5\r\n    , hedgehog                        >= 0.5        && < 1.3\r\n    , QuickCheck                      >= 2.7        && < 2.15\r\n    , transformers                    >= 0.4        && < 0.7\r\n\r\n  ghc-options:\r\n    -Wall\r\n\r\n  hs-source-dirs:\r\n    src\r\n\r\n  exposed-modules:\r\n    Hedgehog.Gen.QuickCheck\r\n\r\n    Test.QuickCheck.Hedgehog\r\n";
    }