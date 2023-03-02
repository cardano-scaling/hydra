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
      identifier = { name = "text-zipper"; version = "0.12"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2015 Jonathan Daugherty";
      maintainer = "cygnus@foobox.com";
      author = "Jonathan Daugherty <cygnus@foobox.com>";
      homepage = "https://github.com/jtdaugherty/text-zipper/";
      url = "";
      synopsis = "A text editor zipper library";
      description = "This library provides a zipper and API for editing text.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "text-zipper-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text-zipper" or (errorHandler.buildDepError "text-zipper"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-zipper-0.12.tar.gz";
      sha256 = "86aba7244c9ed0d8e24e9d1fa64ee317a062e7bd777018053517daefb0696702";
      });
    }) // {
    package-description-override = "name:                text-zipper\nversion:             0.12\nsynopsis:            A text editor zipper library\ndescription:         This library provides a zipper and API for editing text.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Jonathan Daugherty <cygnus@foobox.com>\nmaintainer:          cygnus@foobox.com\ncopyright:           (c) 2015 Jonathan Daugherty\ncategory:            Text\nbuild-type:          Simple\ncabal-version:       >=1.10\ndata-files:          CHANGELOG.md\nhomepage:            https://github.com/jtdaugherty/text-zipper/\nbug-reports:         https://github.com/jtdaugherty/text-zipper/issues\n\nSource-Repository head\n  type:     git\n  location: git://github.com/jtdaugherty/text-zipper.git\n\nlibrary\n  exposed-modules:\n    Data.Text.Zipper\n    Data.Text.Zipper.Generic\n    Data.Text.Zipper.Generic.Words\n\n  other-modules:\n    Data.Text.Zipper.Vector\n\n  build-depends:       base < 5,\n                       text,\n                       vector,\n                       deepseq\n  ghc-options:         -Wall\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n\ntest-suite text-zipper-tests\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      tests\n  main-is:             Main.hs\n  other-modules:       WordsSpec\n  default-language:    Haskell2010\n  build-depends:       base,\n                       text,\n                       hspec,\n                       QuickCheck,\n                       text-zipper\n";
    }