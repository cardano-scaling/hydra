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
      identifier = { name = "erf"; version = "2.0.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Lennart Augustsson";
      author = "Lennart Augustsson";
      homepage = "";
      url = "";
      synopsis = "The error function, erf, and related functions.";
      description = "A type class for the error function, erf, and related functions.\nInstances for Float and Double.";
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
      url = "http://hackage.haskell.org/package/erf-2.0.0.0.tar.gz";
      sha256 = "24f0b79c7e1d25cb2cd44c2258d7a464bf6db8079775b50b60b54a254616b337";
      });
    }) // {
    package-description-override = "Name:           erf\nVersion:        2.0.0.0\nLicense:        BSD3\nAuthor:         Lennart Augustsson\nMaintainer:     Lennart Augustsson\nCategory:       Math\nSynopsis:       The error function, erf, and friends\nStability:      experimental\nBuild-type:     Simple\nLicense:        BSD3\nSynopsis:       The error function, erf, and related functions.\nDescription:    A type class for the error function, erf, and related functions.\n                Instances for Float and Double.\nHs-Source-Dirs: src\nBuild-Depends:          base >= 4 && < 10\nExposed-modules:        Data.Number.Erf\n";
    }