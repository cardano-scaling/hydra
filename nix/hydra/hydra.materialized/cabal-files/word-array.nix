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
      specVersion = "2.4";
      identifier = { name = "word-array"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Zachary Churchill, Michael Peyton Jones";
      homepage = "https://github.com/plutus";
      url = "";
      synopsis = "";
      description = "Treat integral types as arrays of smaller integral types";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/word-array-0.1.0.0.tar.gz";
      sha256 = "d49b9222b6b5f0f419b69f717bb65e036b20897c7c36f95397b97fa233335c14";
      });
    }) // {
    package-description-override = "cabal-version: 2.4\nname: word-array\nversion: 0.1.0.0\nsynopsis:\ndescription: Treat integral types as arrays of smaller integral types\nhomepage: https://github.com/plutus\nlicense: Apache-2.0\nlicense-file: LICENSE\nauthor: Zachary Churchill, Michael Peyton Jones\nmaintainer: michael.peyton-jones@iohk.io\n\ncategory: Data\n\nsource-repository head\n    type: git\n    location: https://github.com/iohk/plutus\n\ncommon lang\n    default-language: Haskell2010\n    default-extensions: ExplicitForAll FlexibleContexts ScopedTypeVariables\n                        DeriveGeneric StandaloneDeriving DeriveLift\n                        GeneralizedNewtypeDeriving DeriveFunctor DeriveFoldable\n                        DeriveTraversable DerivingStrategies DerivingVia\n                        ImportQualifiedPost\n    ghc-options: -Wall -Wnoncanonical-monad-instances\n                 -Wincomplete-uni-patterns -Wincomplete-record-updates\n                 -Wredundant-constraints -Widentities -Wunused-packages\n                 -Wmissing-deriving-strategies\n\nlibrary\n  import: lang\n  exposed-modules:\n      Data.Word64Array.Word8\n\n  build-depends:\n    , base >=4.13 && <5.0\n    , mono-traversable\n    , primitive\n    , deepseq\n  hs-source-dirs: src\n  ghc-options: -O2\n\ntest-suite test\n  import: lang\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Spec.hs\n  build-depends:\n      base\n    , tasty\n    , tasty-quickcheck\n    , QuickCheck\n    , vector\n    , word-array\n    , mono-traversable\n\nbenchmark bench\n  import: lang\n  type: exitcode-stdio-1.0\n  build-depends:\n      base\n    , tasty-bench\n    , word-array\n    , primitive\n  ghc-options: -O2\n  hs-source-dirs: bench\n  main-is: Main.hs\n";
    }