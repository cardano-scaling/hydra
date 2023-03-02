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
      identifier = { name = "js-chart"; version = "2.9.4.1"; };
      license = "MIT";
      copyright = "Jonas Carpay 2020, Neil Mitchell 2014-2020, Chart.js contributors 2020";
      maintainer = "Jonas Carpay <jonascarpay@gmail.com>";
      author = "Jonas Carpay <jonascarpay@gmail.com>";
      homepage = "https://github.com/jonascarpay/js-chart#readme";
      url = "";
      synopsis = "Obtain minified chart.js code";
      description = "This package bundles the minified <http://www.chartjs.org/ chart.js> code into a Haskell package, so it can be depended upon by Cabal packages.\nThe first three components of the version number match the upstream chart.js version.\nThe package is designed to meet the redistribution requirements of downstream users (e.g. Debian).\nThis package is a fork of <https://hackage.haskell.org/package/js-flot js-flot> using chart.js instead of flot.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "js-chart-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."js-chart" or (errorHandler.buildDepError "js-chart"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/js-chart-2.9.4.1.tar.gz";
      sha256 = "0a08efdd35bd1b8f293f9163f59305f31835304b74c3e3a1a840fc94bbc9bd0e";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.18\nbuild-type:         Simple\nname:               js-chart\nversion:            2.9.4.1\nlicense:            MIT\nlicense-file:       LICENSE\ncategory:           Javascript\nauthor:             Jonas Carpay <jonascarpay@gmail.com>\nmaintainer:         Jonas Carpay <jonascarpay@gmail.com>\ncopyright:\n  Jonas Carpay 2020, Neil Mitchell 2014-2020, Chart.js contributors 2020\n\nsynopsis:           Obtain minified chart.js code\ndescription:\n  This package bundles the minified <http://www.chartjs.org/ chart.js> code into a Haskell package, so it can be depended upon by Cabal packages.\n  The first three components of the version number match the upstream chart.js version.\n  The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).\n  This package is a fork of <https://hackage.haskell.org/package/js-flot js-flot> using chart.js instead of flot.\n\nhomepage:           https://github.com/jonascarpay/js-chart#readme\nbug-reports:        https://github.com/jonascarpay/js-chart/issues\ntested-with:        GHC ==8.0 || ==8.2 || ==8.4 || ==8.6 || ==8.8 || ==8.10\nextra-source-files: javascript/chart.js-2.9.4.tgz\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ndata-dir:           javascript\ndata-files:\n  Chart.bundle.min.js\n  Chart.min.js\n  Chart.min.css\n\nsource-repository head\n  type:     git\n  location: https://github.com/jonascarpay/js-chart.git\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  build-depends:    base ==4.*\n  exposed-modules:  Language.Javascript.Chart\n  other-modules:    Paths_js_chart\n\ntest-suite js-chart-test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          src/Test.hs\n  other-modules:    Paths_js_chart\n  build-depends:\n      base      >=4 && <5\n    , js-chart\n";
    }