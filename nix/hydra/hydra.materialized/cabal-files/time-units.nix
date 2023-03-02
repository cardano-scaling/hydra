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
      specVersion = "1.6";
      identifier = { name = "time-units"; version = "1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Adam Wick <awick@uhusre.com>";
      author = "Adam Wick <awick@uhsure.com>";
      homepage = "http://github.com/acw/time-units";
      url = "";
      synopsis = "A basic library for defining units of time as types.";
      description = "In many cases, it is useful (either for error checking or documentation\nreasons) to define input and output types as having a particular unit of\ntime. In addition, by creating a type class defining type units, this\nlibrary should make it easier to separate the units of time the developer\nwants to think in versus the units of time the library author wants to\nthink in.";
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
      url = "http://hackage.haskell.org/package/time-units-1.0.0.tar.gz";
      sha256 = "e181997dd05321f09b21c5e0bf38524ccab51ecc588a6017253cc96db289e099";
      });
    }) // {
    package-description-override = "Name: time-units\nVersion: 1.0.0\nBuild-Type: Simple\nCabal-Version: >= 1.6\nLicense: BSD3\nLicense-File: LICENSE\nAuthor: Adam Wick <awick@uhsure.com>\nMaintainer: Adam Wick <awick@uhusre.com>\nHomepage: http://github.com/acw/time-units\nCategory: Data\nSynopsis: A basic library for defining units of time as types.\n\nDescription:\n  In many cases, it is useful (either for error checking or documentation\n  reasons) to define input and output types as having a particular unit of\n  time. In addition, by creating a type class defining type units, this\n  library should make it easier to separate the units of time the developer\n  wants to think in versus the units of time the library author wants to\n  think in.\n\nLibrary\n  Build-Depends: base >= 4.0 && < 5.0\n  Exposed-Modules: Data.Time.Units\n  Extensions: DeriveDataTypeable,GeneralizedNewtypeDeriving\n\nsource-repository head\n  type: git\n  location: http://github.com/acw/time-units\n\n";
    }