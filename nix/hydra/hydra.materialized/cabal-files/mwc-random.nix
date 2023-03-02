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
      identifier = { name = "mwc-random"; version = "0.15.0.2"; };
      license = "BSD-3-Clause";
      copyright = "2009, 2010, 2011 Bryan O'Sullivan";
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/bos/mwc-random";
      url = "";
      synopsis = "Fast, high quality pseudo random number generation";
      description = "This package contains code for generating high quality random\nnumbers that follow either a uniform or normal distribution.  The\ngenerated numbers are suitable for use in statistical applications.\n\nThe uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)\nmultiply-with-carry generator, which has a period of 2^8222 and\nfares well in tests of randomness.  It is also extremely fast,\nbetween 2 and 3 times faster than the Mersenne Twister.\n\nCompared to the mersenne-random package, this package has a more\nconvenient API, is faster, and supports more statistical\ndistributions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
          ];
        buildable = true;
        };
      tests = {
        "mwc-prop-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        "mwc-doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = if compiler.isGhcjs && true || compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          };
        };
      benchmarks = {
        "mwc-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mersenne-random" or (errorHandler.buildDepError "mersenne-random"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mwc-random-0.15.0.2.tar.gz";
      sha256 = "35d64d5d6f8e93321d36952cf9ab64e21a9cad642ba39aa7378d88aa08afc25b";
      });
    }) // {
    package-description-override = "name:           mwc-random\r\nversion:        0.15.0.2\r\nx-revision: 1\r\nsynopsis:       Fast, high quality pseudo random number generation\r\ndescription:\r\n  This package contains code for generating high quality random\r\n  numbers that follow either a uniform or normal distribution.  The\r\n  generated numbers are suitable for use in statistical applications.\r\n  .\r\n  The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)\r\n  multiply-with-carry generator, which has a period of 2^8222 and\r\n  fares well in tests of randomness.  It is also extremely fast,\r\n  between 2 and 3 times faster than the Mersenne Twister.\r\n  .\r\n  Compared to the mersenne-random package, this package has a more\r\n  convenient API, is faster, and supports more statistical\r\n  distributions.\r\n\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nhomepage:       https://github.com/bos/mwc-random\r\nbug-reports:    https://github.com/bos/mwc-random/issues\r\nauthor:         Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:     Bryan O'Sullivan <bos@serpentine.com>\r\ncopyright:      2009, 2010, 2011 Bryan O'Sullivan\r\ncategory:       Math, Statistics\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\nextra-source-files:\r\n  changelog.md\r\n  README.markdown\r\n\r\ntested-with:\r\n    GHC ==7.10.3\r\n     || ==8.0.2\r\n     || ==8.2.2\r\n     || ==8.4.4\r\n     || ==8.6.5\r\n     || ==8.8.3\r\n     || ==8.10.1\r\n  , GHCJS ==8.4\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  exposed-modules: System.Random.MWC\r\n                   System.Random.MWC.Distributions\r\n                   System.Random.MWC.CondensedTable\r\n                   System.Random.MWC.SeedSource\r\n  build-depends: base           >= 4.8 && < 5\r\n               , primitive      >= 0.6.2\r\n               , random         >= 1.2\r\n               , time\r\n               , vector         >= 0.7\r\n               , math-functions >= 0.2.1.0\r\n\r\n  ghc-options: -Wall -funbox-strict-fields -fwarn-tabs\r\n\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/bos/mwc-random\r\n\r\nsource-repository head\r\n  type:     mercurial\r\n  location: https://bitbucket.org/bos/mwc-random\r\n\r\n\r\nbenchmark mwc-bench\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: bench\r\n  main-is:        Benchmark.hs\r\n  default-language: Haskell2010\r\n  build-depends: base < 5\r\n               , vector          >= 0.11\r\n               , gauge           >= 0.2.5\r\n               , mersenne-random\r\n               , mwc-random\r\n               , random\r\n\r\n\r\ntest-suite mwc-prop-tests\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is:        props.hs\r\n  default-language: Haskell2010\r\n  ghc-options:\r\n    -Wall -threaded -rtsopts\r\n\r\n  build-depends: base\r\n               , mwc-random\r\n               , QuickCheck                 >=2.2\r\n               , vector                     >=0.12.1\r\n               , tasty >=1.3.1\r\n               , tasty-quickcheck\r\n               , tasty-hunit\r\n\r\ntest-suite mwc-doctests\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          doctests.hs\r\n  hs-source-dirs:   tests\r\n  default-language: Haskell2010\r\n  if impl(ghcjs) || impl(ghc < 8.0)\r\n    Buildable: False\r\n  build-depends:\r\n            base       -any\r\n          , mwc-random -any\r\n          , doctest    >=0.15 && <0.20\r\n            --\r\n          , bytestring\r\n          , primitive\r\n          , vector     >=0.11\r\n          , random     >=1.2\r\n";
    }