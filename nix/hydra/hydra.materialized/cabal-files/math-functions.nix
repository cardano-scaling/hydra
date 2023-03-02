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
    flags = { system-expm1 = true; system-erf = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "math-functions"; version = "0.3.4.2"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Alexey Khudyakov <alexey.skladnoy@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>,\nAlexey Khudyakov <alexey.skladnoy@gmail.com>";
      homepage = "https://github.com/bos/math-functions";
      url = "";
      synopsis = "Collection of tools for numeric computations";
      description = "This library contain collection of various utilities for numerical\ncomputing. So far there're special mathematical functions,\ncompensated summation algorithm, summation of series, root finding\nfor real functions, polynomial summation and Chebyshev\npolynomials.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          ];
        buildable = true;
        };
      tests = {
        "math-function-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-th-unbox" or (errorHandler.buildDepError "vector-th-unbox"))
            (hsPkgs."erf" or (errorHandler.buildDepError "erf"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "math-functions-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            ];
          buildable = if compiler.isGhc && (compiler.version).le "7.10" || compiler.isGhcjs && true
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/math-functions-0.3.4.2.tar.gz";
      sha256 = "c1e50ac0d23492b684cce33a9a979e1315ac144175b54f82eade9b8b1885c1a3";
      });
    }) // {
    package-description-override = "name:           math-functions\nversion:        0.3.4.2\ncabal-version:  >= 1.10\nlicense:        BSD2\nlicense-file:   LICENSE\nauthor:         Bryan O'Sullivan <bos@serpentine.com>,\n                Alexey Khudyakov <alexey.skladnoy@gmail.com>\nmaintainer:     Alexey Khudyakov <alexey.skladnoy@gmail.com>\nhomepage:       https://github.com/bos/math-functions\nbug-reports:    https://github.com/bos/math-functions/issues\ncategory:       Math, Numeric\nbuild-type:     Simple\nsynopsis:       Collection of tools for numeric computations\ndescription:\n\n  This library contain collection of various utilities for numerical\n  computing. So far there're special mathematical functions,\n  compensated summation algorithm, summation of series, root finding\n  for real functions, polynomial summation and Chebyshev\n  polynomials. \n\ntested-with:\n    GHC ==7.4.2\n     || ==7.6.3\n     || ==7.8.4\n     || ==7.10.3\n     || ==8.0.2\n     || ==8.2.2\n     || ==8.4.4\n     || ==8.6.5\n     || ==8.8.4\n     || ==8.10.2\n     || ==9.0.1\n  , GHCJS ==8.4\n\nextra-source-files:\n                   changelog.md\n                   README.markdown\n                   tests/Tests/SpecFunctions/gen.py\n                   tests/tables/generate.py\n                   tests/tables/shell.nix\n                   tests/tables/*.dat\n                   tests/tables/inputs/*.dat\n                   doc/sinc.hs\n\nflag system-expm1\n     description: Use expm1 provided by system. For GHC newer that\n                  8.0, GHCJS, and on Windows has no effect. GHC>=8.0\n                  provides expm1 so it's used. On GHCJS and on Windows\n                  we don't have C implementation so bundled one is\n                  used instead.\n     default:     True\n     manual:      True\n\nflag system-erf\n     description: Use erf and erfc provided by system. On GHCJS\n                  version provided by library is used regardless of\n                  flag for that lack of libc.\n     default:     True\n     manual:      True\n\nlibrary\n  default-language: Haskell2010\n  other-extensions:\n    BangPatterns\n    CPP\n    DeriveDataTypeable\n    FlexibleContexts\n    MultiParamTypeClasses\n    ScopedTypeVariables\n    TypeFamilies\n    DeriveGeneric\n\n  ghc-options:          -Wall -O2\n  build-depends:        base                >= 4.5 && < 5\n                      , deepseq\n                      , data-default-class  >= 0.1.2.0\n                      , vector              >= 0.11\n                      , primitive\n  if flag(system-expm1) && !os(windows)\n    cpp-options: -DUSE_SYSTEM_EXPM1\n  if flag(system-erf)   && !impl(ghcjs)\n    cpp-options: -DUSE_SYSTEM_ERF\n  exposed-modules:\n    Numeric.MathFunctions.Constants\n    Numeric.MathFunctions.Comparison\n    Numeric.Polynomial\n    Numeric.Polynomial.Chebyshev\n    Numeric.RootFinding\n    Numeric.SpecFunctions\n    Numeric.SpecFunctions.Extra\n    Numeric.SpecFunctions.Internal\n    Numeric.Series\n    Numeric.Sum\n  other-modules:\n    Numeric.SpecFunctions.Compat\n\ntest-suite math-function-tests\n  default-language: Haskell2010\n  other-extensions: ViewPatterns\n\n  type:           exitcode-stdio-1.0\n  ghc-options:    -Wall -threaded\n  if arch(i386)\n    -- The Sum tests require SSE2 on i686 to pass (because of excess precision)\n    ghc-options:  -msse2\n  hs-source-dirs: tests\n  main-is:        tests.hs\n  other-modules:\n    Tests.Helpers\n    Tests.Chebyshev\n    Tests.Comparison\n    Tests.RootFinding\n    Tests.SpecFunctions\n    Tests.SpecFunctions.Tables\n    Tests.Sum\n  if flag(system-erf)   && !impl(ghcjs)\n    cpp-options: -DUSE_SYSTEM_ERF\n  build-depends:    base >=4.5 && <5\n                  , math-functions\n                  , data-default-class  >= 0.1.2.0\n                  , deepseq\n                  , primitive\n                  , vector >= 0.7\n                  , vector-th-unbox\n                  , erf\n                  , QuickCheck       >= 2.7\n                  , tasty            >= 1.2\n                  , tasty-hunit      >= 0.10\n                  , tasty-quickcheck >= 0.10\n\nbenchmark math-functions-bench\n  type:             exitcode-stdio-1.0\n  if impl(ghc <= 7.10 ) || impl(ghcjs)\n     buildable: False\n  default-language: Haskell2010\n  other-extensions:\n    BangPatterns\n    CPP\n    DeriveDataTypeable\n    FlexibleContexts\n    MultiParamTypeClasses\n    ScopedTypeVariables\n    TemplateHaskell\n    TypeFamilies\n    DeriveGeneric\n  ghc-options:          -Wall -O2\n  hs-source-dirs:       bench\n  Main-is:              bench.hs\n  build-depends:        base                >= 4.5 && < 5\n                      , math-functions\n                      , data-default-class\n                      , vector\n                      , random\n                      , gauge               >=0.2.5\n\nsource-repository head\n  type:     git\n  location: https://github.com/bos/math-functions\n";
    }