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
      specVersion = "1.18";
      identifier = { name = "ekg-core"; version = "0.1.1.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Johan Tibell <johan.tibell@gmail.com>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>";
      author = "Johan Tibell";
      homepage = "https://github.com/tibbe/ekg-core";
      url = "";
      synopsis = "Tracking of system metrics";
      description = "This library lets you defined and track system metrics.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      benchmarks = {
        "counter" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            ];
          buildable = true;
          };
        "distribution" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ekg-core-0.1.1.7.tar.gz";
      sha256 = "45813f2b94fde0b92c7979bd37de52f09b8b645560f5789276c3acfc7934db12";
      });
    }) // {
    package-description-override = "cabal-version:       1.18\r\nname:                ekg-core\r\nversion:             0.1.1.7\r\nx-revision: 2\r\nsynopsis:            Tracking of system metrics\r\ndescription:\r\n  This library lets you defined and track system metrics.\r\nhomepage:            https://github.com/tibbe/ekg-core\r\nbug-reports:         https://github.com/tibbe/ekg-core/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Johan Tibell\r\nmaintainer:          Johan Tibell <johan.tibell@gmail.com>,\r\n                     Mikhail Glushenkov <mikhail.glushenkov@gmail.com>\r\ncategory:            System\r\nbuild-type:          Simple\r\nextra-source-files:  CHANGES.md\r\ntested-with:         GHC == 8.8.3, GHC == 8.6.5, GHC == 8.4.4,\r\n                     GHC == 8.2.2, GHC == 8.0.2, GHC == 7.10.3,\r\n                     GHC == 7.8.4, GHC == 7.6.3\r\n\r\nlibrary\r\n  exposed-modules:\r\n    System.Metrics\r\n    System.Metrics.Counter\r\n    System.Metrics.Distribution\r\n    System.Metrics.Distribution.Internal\r\n    System.Metrics.Gauge\r\n    System.Metrics.Label\r\n\r\n  other-modules:\r\n    Data.Array\r\n    Data.Atomic\r\n    System.Metrics.ThreadId\r\n\r\n  build-depends:\r\n    ghc-prim < 0.10,\r\n    base >= 4.6 && < 5,\r\n    containers >= 0.5 && < 0.7,\r\n    text < 2.1,\r\n    unordered-containers < 0.3\r\n\r\n  default-language:    Haskell2010\r\n\r\n  ghc-options: -Wall\r\n  if arch(i386)\r\n    cc-options: -march=i686\r\n  includes: distrib.h\r\n  install-includes: distrib.h\r\n  include-dirs: cbits\r\n  c-sources: cbits/atomic.c cbits/distrib.c\r\n\r\nbenchmark counter\r\n  main-is: Counter.hs\r\n  type: exitcode-stdio-1.0\r\n  build-depends:\r\n    base,\r\n    ekg-core\r\n  default-language: Haskell2010\r\n  hs-source-dirs: benchmarks\r\n  ghc-options: -O2 -threaded -Wall\r\n\r\nbenchmark distribution\r\n  main-is: Distribution.hs\r\n  type: exitcode-stdio-1.0\r\n  build-depends:\r\n    base,\r\n    ekg-core\r\n  default-language: Haskell2010\r\n  hs-source-dirs: benchmarks\r\n  ghc-options: -O2 -threaded -Wall\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/tibbe/ekg-core.git\r\n";
    }