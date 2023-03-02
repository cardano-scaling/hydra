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
      identifier = { name = "dense-linear-algebra"; version = "0.1.0.0"; };
      license = "BSD-2-Clause";
      copyright = "2018 Author name here";
      maintainer = "Alexey Khudaykov <alexey.skladnoy@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "";
      url = "";
      synopsis = "Simple and incomplete pure haskell implementation of linear algebra";
      description = "This library is simply collection of linear-algebra related modules\nsplit from statistics library.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          (hsPkgs."vector-th-unbox" or (errorHandler.buildDepError "vector-th-unbox"))
          (hsPkgs."vector-binary-instances" or (errorHandler.buildDepError "vector-binary-instances"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."dense-linear-algebra" or (errorHandler.buildDepError "dense-linear-algebra"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dense-linear-algebra-0.1.0.0.tar.gz";
      sha256 = "f7777a7931b40332ebbc716f64abb63697cbab0128e5c1228d47760c5597f2d4";
      });
    }) // {
    package-description-override = "name:                dense-linear-algebra\nversion:             0.1.0.0\nsynopsis:            Simple and incomplete pure haskell implementation of linear algebra\ndescription:\n  This library is simply collection of linear-algebra related modules\n  split from statistics library.\n\nlicense:             BSD2\nlicense-file:        LICENSE\nauthor:              Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:          Alexey Khudaykov <alexey.skladnoy@gmail.com>\ncopyright:           2018 Author name here\ncategory:            Math, Statistics, Numeric\nbuild-type:          Simple\nextra-source-files:  README.md\ncabal-version:       >=1.10\ntested-with: GHC==7.6.3, GHC==7.8.3, GHC==7.10.3, GHC==8.0.1\n\nlibrary\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  hs-source-dirs:      src\n  exposed-modules:     Statistics.Matrix\n                       Statistics.Matrix.Algorithms\n                       Statistics.Matrix.Function\n                       Statistics.Matrix.Mutable\n                       Statistics.Matrix.Types\n  build-depends:       base                    >= 4.5 && < 5\n                     , deepseq                 >= 1.1.0.2\n                     , math-functions          >= 0.1.7                     \n                     , primitive               >= 0.3\n                     , vector                  >= 0.10\n                     , vector-algorithms       >= 0.4\n                     , vector-th-unbox\n                     , vector-binary-instances >= 0.2.1\n\ntest-suite spec\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             LibSpec.hs\n  build-depends:       base\n                     , dense-linear-algebra\n                     , hspec\n                     , QuickCheck\n\nsource-repository head\n  type:     git\n  location: https://github.com/bos/statistics\n";
    }