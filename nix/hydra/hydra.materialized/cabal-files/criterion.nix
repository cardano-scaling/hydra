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
    flags = { fast = false; embed-data-files = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "criterion"; version = "1.6.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2009-2016 Bryan O'Sullivan and others";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://www.serpentine.com/criterion";
      url = "";
      synopsis = "Robust, reliable performance measurement and analysis";
      description = "This library provides a powerful but simple way to measure software\nperformance.  It provides both a framework for executing and\nanalysing benchmarks and a set of driver functions that makes it\neasy to build and run benchmarks, and to analyse their results.\n\nThe fastest way to get started is to read the\n<http://www.serpentine.com/criterion/tutorial.html online tutorial>,\nfollowed by the documentation and examples in the \"Criterion.Main\"\nmodule.\n\nFor examples of the kinds of reports that criterion generates, see\n<http://www.serpentine.com/criterion the home page>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."binary-orphans" or (errorHandler.buildDepError "binary-orphans"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
          (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
          (hsPkgs."js-chart" or (errorHandler.buildDepError "js-chart"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ]) ++ (pkgs.lib).optionals (flags.embed-data-files) [
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      exes = {
        "criterion-report" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      tests = {
        "sanity" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            ];
          buildable = true;
          };
        "cleanup" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/criterion-1.6.0.0.tar.gz";
      sha256 = "4029ef9ccd158e995a97827bb50d189a98a0875ea4b73300276fac1510a252b0";
      });
    }) // {
    package-description-override = "name:           criterion\nversion:        1.6.0.0\nsynopsis:       Robust, reliable performance measurement and analysis\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\ncopyright:      2009-2016 Bryan O'Sullivan and others\ncategory:       Development, Performance, Testing, Benchmarking\nhomepage:       http://www.serpentine.com/criterion\nbug-reports:    https://github.com/haskell/criterion/issues\nbuild-type:     Simple\ncabal-version:  >= 1.10\nextra-source-files:\n  README.markdown\n  changelog.md\n  examples/LICENSE\n  examples/*.cabal\n  examples/*.hs\ntested-with:\n  GHC==7.4.2,\n  GHC==7.6.3,\n  GHC==7.8.4,\n  GHC==7.10.3,\n  GHC==8.0.2,\n  GHC==8.2.2,\n  GHC==8.4.4,\n  GHC==8.6.5,\n  GHC==8.8.4,\n  GHC==8.10.7,\n  GHC==9.0.2,\n  GHC==9.2.2\n\ndata-files:\n  templates/*.css\n  templates/*.tpl\n  templates/*.js\n\ndescription:\n  This library provides a powerful but simple way to measure software\n  performance.  It provides both a framework for executing and\n  analysing benchmarks and a set of driver functions that makes it\n  easy to build and run benchmarks, and to analyse their results.\n  .\n  The fastest way to get started is to read the\n  <http://www.serpentine.com/criterion/tutorial.html online tutorial>,\n  followed by the documentation and examples in the \"Criterion.Main\"\n  module.\n  .\n  For examples of the kinds of reports that criterion generates, see\n  <http://www.serpentine.com/criterion the home page>.\n\nflag fast\n  description: compile without optimizations\n  default: False\n  manual: True\n\nflag embed-data-files\n  description: Embed the data files in the binary for a relocatable executable.\n               (Warning: This will increase the executable size significantly.)\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Criterion\n    Criterion.Analysis\n    Criterion.IO\n    Criterion.IO.Printf\n    Criterion.Internal\n    Criterion.Main\n    Criterion.Main.Options\n    Criterion.Monad\n    Criterion.Report\n    Criterion.Types\n\n  other-modules:\n    Criterion.Main.Options.Internal\n    Criterion.Monad.Internal\n\n  other-modules:\n    Paths_criterion\n\n  build-depends:\n    -- TODO: Eventually, we should bump the lower version bounds to >=2 so that\n    -- we can remove some CPP in Criterion.Report. See #247.\n    aeson >= 1 && < 2.2,\n    ansi-wl-pprint >= 0.6.7.2,\n    base >= 4.5 && < 5,\n    base-compat-batteries >= 0.10 && < 0.13,\n    binary >= 0.5.1.0,\n    binary-orphans >= 1.0.1 && < 1.1,\n    bytestring >= 0.9 && < 1.0,\n    cassava >= 0.3.0.0,\n    code-page,\n    containers,\n    criterion-measurement >= 0.2 && < 0.3,\n    deepseq >= 1.1.0.0,\n    directory,\n    exceptions >= 0.8.2 && < 0.11,\n    filepath,\n    Glob >= 0.7.2,\n    microstache >= 1.0.1 && < 1.1,\n    js-chart >= 2.9.4 && < 3,\n    mtl >= 2,\n    mwc-random >= 0.8.0.3,\n    -- TODO: Depend on optparse-applicative-0.17 as the minimum (see #258)\n    optparse-applicative >= 0.13 && < 0.18,\n    parsec >= 3.1.0,\n    statistics >= 0.14 && < 0.17,\n    text >= 0.11,\n    time,\n    transformers,\n    transformers-compat >= 0.6.4,\n    vector >= 0.7.1,\n    vector-algorithms >= 0.4\n  if impl(ghc < 7.6)\n    build-depends:\n      ghc-prim\n  if !impl(ghc >= 8.0)\n    build-depends:\n      fail == 4.9.*,\n      semigroups\n\n  default-language: Haskell2010\n  ghc-options: -Wall -funbox-strict-fields\n  if impl(ghc >= 6.8)\n    ghc-options: -fwarn-tabs\n  if flag(fast)\n    ghc-options: -O0\n  else\n    ghc-options: -O2\n\n  if flag(embed-data-files)\n    other-modules: Criterion.EmbeddedData\n    build-depends: file-embed < 0.1,\n                   template-haskell\n    cpp-options: \"-DEMBED\"\n\nExecutable criterion-report\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall -rtsopts\n  Main-Is:              Report.hs\n  Other-Modules:        Options\n                        Paths_criterion\n  Hs-Source-Dirs:       app\n\n  Build-Depends:\n    base,\n    base-compat-batteries,\n    criterion,\n    optparse-applicative >= 0.13\n\n  if impl(ghc < 7.6)\n    build-depends:\n      ghc-prim\n\n  if !impl(ghc >= 8.0)\n    build-depends:\n      semigroups\n\ntest-suite sanity\n  type:                 exitcode-stdio-1.0\n  hs-source-dirs:       tests\n  main-is:              Sanity.hs\n  default-language:     Haskell2010\n  ghc-options:          -Wall -rtsopts\n  if flag(fast)\n    ghc-options:        -O0\n  else\n    ghc-options:        -O2\n\n  build-depends:\n    HUnit,\n    base,\n    bytestring,\n    criterion,\n    deepseq,\n    tasty,\n    tasty-hunit\n\ntest-suite tests\n  type:                 exitcode-stdio-1.0\n  hs-source-dirs:       tests\n  main-is:              Tests.hs\n  default-language:     Haskell2010\n  other-modules:        Properties\n\n  ghc-options:\n    -Wall -threaded     -O0 -rtsopts\n\n  build-depends:\n    QuickCheck >= 2.4,\n    base,\n    base-compat-batteries,\n    criterion,\n    statistics,\n    HUnit,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    vector,\n    aeson\n\ntest-suite cleanup\n  type:                 exitcode-stdio-1.0\n  hs-source-dirs:       tests\n  default-language:     Haskell2010\n  main-is:              Cleanup.hs\n\n  ghc-options:\n    -Wall -threaded     -O0 -rtsopts\n\n  build-depends:\n    HUnit,\n    base,\n    base-compat,\n    bytestring,\n    criterion,\n    deepseq,\n    directory,\n    tasty,\n    tasty-hunit\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/criterion.git\n";
    }