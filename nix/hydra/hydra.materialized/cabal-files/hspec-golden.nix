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
      specVersion = "1.12";
      identifier = { name = "hspec-golden"; version = "0.2.0.0"; };
      license = "MIT";
      copyright = "2019-2020 Stack Builders Inc";
      maintainer = "cmotoche@stackbuilders.com";
      author = "Stack Builders";
      homepage = "https://github.com/stackbuilders/hspec-golden#readme";
      url = "";
      synopsis = "Golden tests for hspec";
      description = "Please see the README on GitHub at <https://github.com/stackbuilders/hspec-golden#README>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
          ];
        buildable = true;
        };
      exes = {
        "hgold" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec-golden" or (errorHandler.buildDepError "hspec-golden"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          };
        };
      tests = {
        "hspec-golden-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-golden" or (errorHandler.buildDepError "hspec-golden"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-golden-0.2.0.0.tar.gz";
      sha256 = "d322dd7d625dd3f1c5fd30e7965be127744bfa0f6b337983352f44d1c771e969";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.33.1.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 34c8c9ac8723c74e1b8dfdf2b6153ad8f1d4c677cbdf667841dc84bda2db22d4\n\nname:           hspec-golden\nversion:        0.2.0.0\nsynopsis:       Golden tests for hspec\ndescription:    Please see the README on GitHub at <https://github.com/stackbuilders/hspec-golden#README>\ncategory:       Testing\nhomepage:       https://github.com/stackbuilders/hspec-golden#readme\nbug-reports:    https://github.com/stackbuilders/hspec-golden/issues\nauthor:         Stack Builders\nmaintainer:     cmotoche@stackbuilders.com\ncopyright:      2019-2020 Stack Builders Inc\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/stackbuilders/hspec-golden\n\nlibrary\n  exposed-modules:\n      Test.Hspec.Golden\n  other-modules:\n      Paths_hspec_golden\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.6 && <5\n    , directory\n    , filepath >=1.0 && <2.0\n    , hspec-core >=2.5 && <3.0\n  default-language: Haskell2010\n\nexecutable hgold\n  main-is: Main.hs\n  other-modules:\n      Paths_hspec_golden\n  hs-source-dirs:\n      app\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.6 && <5\n    , directory >=1.2.5.0\n    , hspec-golden\n    , optparse-applicative\n  default-language: Haskell2010\n\ntest-suite hspec-golden-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Test.Hspec.GoldenSpec\n      Paths_hspec_golden\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.6 && <5\n    , directory\n    , hspec\n    , hspec-core\n    , hspec-golden\n    , silently\n  default-language: Haskell2010\n";
    }