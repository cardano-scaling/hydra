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
      identifier = { name = "genvalidity-scientific"; version = "1.0.0.0"; };
      license = "MIT";
      copyright = "Copyright: (c) 2016-2021 Tom Sydney Kerckhove";
      maintainer = "syd@cs-syd.eu";
      author = "Tom Sydney Kerckhove";
      homepage = "https://github.com/NorfairKing/validity#readme";
      url = "";
      synopsis = "GenValidity support for Scientific";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
          (hsPkgs."validity-scientific" or (errorHandler.buildDepError "validity-scientific"))
          ];
        buildable = true;
        };
      tests = {
        "genvalidity-scientific-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
            (hsPkgs."genvalidity-hspec" or (errorHandler.buildDepError "genvalidity-hspec"))
            (hsPkgs."genvalidity-scientific" or (errorHandler.buildDepError "genvalidity-scientific"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/genvalidity-scientific-1.0.0.0.tar.gz";
      sha256 = "b85e13c3f54ed955f6d568503d9cda49162f3a4af7560acac62df8a565502176";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.4.\n--\n-- see: https://github.com/sol/hpack\n\nname:           genvalidity-scientific\nversion:        1.0.0.0\nsynopsis:       GenValidity support for Scientific\ncategory:       Testing\nhomepage:       https://github.com/NorfairKing/validity#readme\nbug-reports:    https://github.com/NorfairKing/validity/issues\nauthor:         Tom Sydney Kerckhove\nmaintainer:     syd@cs-syd.eu\ncopyright:      Copyright: (c) 2016-2021 Tom Sydney Kerckhove\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    LICENSE\n    CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/NorfairKing/validity\n\nlibrary\n  exposed-modules:\n      Data.GenValidity.Scientific\n  other-modules:\n      Paths_genvalidity_scientific\n  hs-source-dirs:\n      src\n  build-depends:\n      QuickCheck\n    , base >=4.7 && <5\n    , genvalidity >=1.0\n    , scientific\n    , validity >=0.5\n    , validity-scientific >=0.2\n  default-language: Haskell2010\n\ntest-suite genvalidity-scientific-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Data.GenValidity.ScientificSpec\n      Paths_genvalidity_scientific\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall\n  build-depends:\n      QuickCheck\n    , base >=4.7 && <5\n    , genvalidity\n    , genvalidity-hspec\n    , genvalidity-scientific\n    , hspec\n    , scientific\n  default-language: Haskell2010\n";
    }