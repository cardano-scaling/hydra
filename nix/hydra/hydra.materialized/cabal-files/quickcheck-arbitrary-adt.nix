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
      identifier = { name = "quickcheck-arbitrary-adt"; version = "0.3.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2016-2017 Plow Technologies";
      maintainer = "mchaver@gmail.com";
      author = "James M.C. Haver II";
      homepage = "https://github.com/plow-technologies/quickcheck-arbitrary-adt#readme";
      url = "";
      synopsis = "Generic typeclasses for generating arbitrary ADTs";
      description = "Improve arbitrary value generation for ADTs";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-arbitrary-adt" or (errorHandler.buildDepError "quickcheck-arbitrary-adt"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-arbitrary-adt-0.3.1.0.tar.gz";
      sha256 = "5c4a2e20366def76ba851211ac554e9a0f60535efcd0940606e4d410c27a45b9";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.17.1.\n--\n-- see: https://github.com/sol/hpack\n\nname:           quickcheck-arbitrary-adt\nversion:        0.3.1.0\nsynopsis:       Generic typeclasses for generating arbitrary ADTs\ndescription:    Improve arbitrary value generation for ADTs\ncategory:       Testing\nstability:      Beta\nhomepage:       https://github.com/plow-technologies/quickcheck-arbitrary-adt#readme\nbug-reports:    https://github.com/plow-technologies/quickcheck-arbitrary-adt/issues\nauthor:         James M.C. Haver II\nmaintainer:     mchaver@gmail.com\ncopyright:      2016-2017 Plow Technologies\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\nsource-repository head\n  type: git\n  location: https://github.com/plow-technologies/quickcheck-arbitrary-adt\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base >= 4.7 && < 5\n    , QuickCheck\n  exposed-modules:\n      Test.QuickCheck.Arbitrary.ADT\n  other-modules:\n      Paths_quickcheck_arbitrary_adt\n  default-language: Haskell2010\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      tests\n  build-depends:\n      base\n    , hspec\n    , lens\n    , template-haskell\n    , QuickCheck\n    , quickcheck-arbitrary-adt\n    , transformers\n  other-modules:\n      Test.QuickCheck.Arbitrary.ADTSpec\n  default-language: Haskell2010\n";
    }