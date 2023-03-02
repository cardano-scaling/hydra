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
      specVersion = "1.0";
      identifier = { name = "Stream"; version = "0.4.7.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Wouter Swierstra <wouter.swierstra@gmail.com>";
      author = "Wouter Swierstra <wouter.swierstra@gmail.com>\nBas van Dijk <v.dijk.bas@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "A library for manipulating infinite lists.";
      description = "This package implements functions, analogous\nto those from Data.List, to create and manipulate\ninfinite lists: @data Stream a = Cons a (Stream a)@.\nIt provides alternative definitions for those\nPrelude functions that make sense for such streams.\nNote that this package has (almost)\nnothing to do with the work on /Stream Fusion/ by\nDuncan Coutts, Roman Leshchinskiy, and Don Stewart.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."lazysmallcheck" or (errorHandler.buildDepError "lazysmallcheck"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Stream-0.4.7.2.tar.gz";
      sha256 = "990be249b3ef1b0075563026d4d2c803b86e3cbf168965ba6f9f2b4227a007d1";
      });
    }) // {
    package-description-override = "Name:                   Stream\nVersion:                0.4.7.2\nLicense:                BSD3\nLicense-file:           LICENSE\nAuthor:                 Wouter Swierstra <wouter.swierstra@gmail.com>\n\t\t\tBas van Dijk <v.dijk.bas@gmail.com>\nMaintainer:             Wouter Swierstra <wouter.swierstra@gmail.com>\nStability:\t\texperimental\nSynopsis:               A library for manipulating infinite lists.\nDescription:            This package implements functions, analogous\n                        to those from Data.List, to create and manipulate\n\t\t\tinfinite lists: @data Stream a = Cons a (Stream a)@.\n\t\t\tIt provides alternative definitions for those\n\t\t\tPrelude functions that make sense for such streams. \n\t\t\tNote that this package has (almost) \n\t\t\tnothing to do with the work on /Stream Fusion/ by \n\t\t\tDuncan Coutts, Roman Leshchinskiy, and Don Stewart.\nCategory:               Data\nBuild-Depends:          base < 5, QuickCheck >= 2.0, lazysmallcheck >= 0.3\nBuild-Type:\t\tSimple\nExposed-modules:        Data.Stream\n\n";
    }