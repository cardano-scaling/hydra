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
      identifier = { name = "tasty-hedgehog"; version = "1.3.1.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2017, Commonwealth Scientific and Industrial Research Organisation (CSIRO) ABN 41 687 119 230.";
      maintainer = "dave.laing.80@gmail.com";
      author = "Dave Laing";
      homepage = "https://github.com/qfpl/tasty-hedgehog";
      url = "";
      synopsis = "Integration for tasty and hedgehog.";
      description = "Integrates the <https://hackage.haskell.org/package/hedgehog hedgehog testing library> with the <https://hackage.haskell.org/package/tasty tasty testing framework>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          ];
        buildable = true;
        };
      tests = {
        "tasty-hedgehog-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tasty-hedgehog-1.3.1.0.tar.gz";
      sha256 = "75e0dd95079aed2032adc22917a6de6c5c2e1141b56a59dd4d8ca7b6ab2804c7";
      });
    }) // {
    package-description-override = "name:                tasty-hedgehog\nversion:             1.3.1.0\nlicense:             BSD3\nlicense-file:        LICENCE\nauthor:              Dave Laing\nmaintainer:          dave.laing.80@gmail.com\ncopyright:           Copyright (c) 2017, Commonwealth Scientific and Industrial Research Organisation (CSIRO) ABN 41 687 119 230.\ndescription:         Integrates the <https://hackage.haskell.org/package/hedgehog hedgehog testing library> with the <https://hackage.haskell.org/package/tasty tasty testing framework>.\ncategory:            Testing\nsynopsis:            Integration for tasty and hedgehog.\nhomepage:            https://github.com/qfpl/tasty-hedgehog\nbug-reports:         https://github.com/qfpl/tasty-hedgehog/issues\nbuild-type:          Simple\nextra-source-files:  changelog.md\ncabal-version:       >=1.10\ntested-with:         GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.3, GHC == 8.10.1\n\nsource-repository   head\n  type:             git\n  location:         git@github.com:qfpl/tasty-hedgehog.git\n\nlibrary\n  exposed-modules:     Test.Tasty.Hedgehog\n  build-depends:       base >= 4.8 && <4.18\n                     , tagged >= 0.8 && < 0.9\n                     , tasty >= 0.11 && < 1.5\n                     , hedgehog >= 1.0.2 && < 1.1.3\n  hs-source-dirs:      src\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n\ntest-suite tasty-hedgehog-tests\n  type:                exitcode-stdio-1.0\n  main-is:             Main.hs\n  hs-source-dirs:      test\n  build-depends:       base >= 4.8 && <4.18\n                     , tasty >= 0.11 && < 1.5\n                     , tasty-expected-failure >= 0.11 && < 0.13\n                     , hedgehog >= 1.0.2 && < 1.1.3\n                     , tasty-hedgehog\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n";
    }