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
      identifier = { name = "unbounded-delays"; version = "0.1.1.1"; };
      license = "BSD-3-Clause";
      copyright = "2011-2020 Bas van Dijk & Roel van Dijk";
      maintainer = "Bas van Dijk <v.dijk.bas@gmail.com>\nRoel van Dijk <roel@lambdacube.nl>";
      author = "Bas van Dijk <v.dijk.bas@gmail.com>\nRoel van Dijk <roel@lambdacube.nl>";
      homepage = "https://github.com/basvandijk/unbounded-delays";
      url = "";
      synopsis = "Unbounded thread delays and timeouts";
      description = "The @threadDelay@ and @timeout@ functions from the @base@ library\nuse the bounded @Int@ type for specifying the delay or timeout\nperiod. This packages provides alternatives which use the\nunbounded @Integer@ type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unbounded-delays-0.1.1.1.tar.gz";
      sha256 = "59ad7e53bfe32ffbf0e703b31490d41d14c70e4745ed49e8adf592ed68dd6185";
      });
    }) // {
    package-description-override = "name:          unbounded-delays\nversion:       0.1.1.1\ncabal-version: >= 1.10\nbuild-type:    Simple\nauthor:        Bas van Dijk <v.dijk.bas@gmail.com>\n               Roel van Dijk <roel@lambdacube.nl>\nmaintainer:    Bas van Dijk <v.dijk.bas@gmail.com>\n               Roel van Dijk <roel@lambdacube.nl>\ncopyright:     2011-2020 Bas van Dijk & Roel van Dijk\nlicense:       BSD3\nlicense-file:  LICENSE\nhomepage:      https://github.com/basvandijk/unbounded-delays\nbug-reports:   https://github.com/basvandijk/unbounded-delays/issues\ncategory:      Concurrency\nsynopsis:      Unbounded thread delays and timeouts\ndescription:   The @threadDelay@ and @timeout@ functions from the @base@ library\n               use the bounded @Int@ type for specifying the delay or timeout\n               period. This packages provides alternatives which use the\n               unbounded @Integer@ type.\n\nextra-source-files: README.markdown\n\nsource-repository head\n  Type: git\n  Location: git://github.com/basvandijk/unbounded-delays.git\n\nlibrary\n  default-language: Haskell98\n  build-depends: base >= 4.4 && < 5\n  exposed-modules: Control.Concurrent.Thread.Delay\n                 , Control.Concurrent.Timeout\n  ghc-options: -Wall\n";
    }