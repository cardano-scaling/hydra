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
      identifier = { name = "word-wrap"; version = "0.5"; };
      license = "BSD-3-Clause";
      copyright = "2017 Jonathan Daugherty";
      maintainer = "cygnus@foobox.com";
      author = "Jonathan Daugherty";
      homepage = "https://github.com/jtdaugherty/word-wrap/";
      url = "";
      synopsis = "A library for word-wrapping";
      description = "A library for wrapping long lines of text.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "word-wrap-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."word-wrap" or (errorHandler.buildDepError "word-wrap"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "word-wrap-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."word-wrap" or (errorHandler.buildDepError "word-wrap"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/word-wrap-0.5.tar.gz";
      sha256 = "f85f29ce003ca3e195ec95d49e350dfb713bf71db726270143375df2c610a744";
      });
    }) // {
    package-description-override = "name:                word-wrap\nversion:             0.5\nsynopsis:            A library for word-wrapping\ndescription:         A library for wrapping long lines of text.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Jonathan Daugherty\nmaintainer:          cygnus@foobox.com\ncopyright:           2017 Jonathan Daugherty\ncategory:            Text\nbuild-type:          Simple\ncabal-version:       1.18\nHomepage:            https://github.com/jtdaugherty/word-wrap/\nBug-reports:         https://github.com/jtdaugherty/word-wrap/issues\n\nextra-doc-files:\n  README.md\n  CHANGELOG.md\n\nSource-Repository head\n  type:     git\n  location: git://github.com/jtdaugherty/word-wrap.git\n\nlibrary\n  exposed-modules:\n    Text.Wrap\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  build-depends:       base >= 4.8 && < 5,\n                       text\n\nbenchmark word-wrap-benchmarks\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  hs-source-dirs:      benchmarks\n  main-is:             Main.hs\n  ghc-options:         -Wall\n  build-depends:       base < 5,\n                       word-wrap,\n                       criterion,\n                       text\n\ntest-suite word-wrap-tests\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  hs-source-dirs:      tests\n  main-is:             Main.hs\n  ghc-options:         -Wall\n  build-depends:       base < 5,\n                       word-wrap,\n                       hspec >= 2.4\n";
    }