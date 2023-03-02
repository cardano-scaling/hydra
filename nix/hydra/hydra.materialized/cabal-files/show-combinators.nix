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
      specVersion = "1.10";
      identifier = { name = "show-combinators"; version = "0.2.0.0"; };
      license = "MIT";
      copyright = "2018 Li-yao Xia";
      maintainer = "lysxia@gmail.com";
      author = "Li-yao Xia";
      homepage = "https://github.com/Lysxia/show-combinators#readme";
      url = "";
      synopsis = "Combinators to write Show instances";
      description = "A minimal pretty-printing library for Show instances in Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."show-combinators" or (errorHandler.buildDepError "show-combinators"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/show-combinators-0.2.0.0.tar.gz";
      sha256 = "c902dbaf0e9cf7056d786d44fbdea2781bc65524089639242c2624dae841ba1d";
      });
    }) // {
    package-description-override = "name:                show-combinators\r\nversion:             0.2.0.0\r\nx-revision: 2\r\nsynopsis:            Combinators to write Show instances\r\ndescription:\r\n  A minimal pretty-printing library for Show instances in Haskell.\r\nhomepage:            https://github.com/Lysxia/show-combinators#readme\r\nlicense:             MIT\r\nlicense-file:        LICENSE\r\nauthor:              Li-yao Xia\r\nmaintainer:          lysxia@gmail.com\r\ncopyright:           2018 Li-yao Xia\r\ncategory:            Text\r\nbuild-type:          Simple\r\nextra-source-files:  README.md, CHANGELOG.md\r\ncabal-version:       >=1.10\r\ntested-with:         GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.1\r\n\r\nlibrary\r\n  hs-source-dirs:      src\r\n  exposed-modules:\r\n    Text.Show.Combinators\r\n  build-depends:\r\n    -- This upper bound is not conservative\r\n    base >= 4.8 && < 5\r\n  ghc-options:         -Wall\r\n  default-language:    Haskell2010\r\n\r\ntest-suite test\r\n  hs-source-dirs:      test\r\n  main-is:             test.hs\r\n  build-depends:\r\n    show-combinators,\r\n    base\r\n  ghc-options:         -Wall\r\n  default-language:    Haskell2010\r\n  type:                exitcode-stdio-1.0\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/Lysxia/show-combinators\r\n";
    }