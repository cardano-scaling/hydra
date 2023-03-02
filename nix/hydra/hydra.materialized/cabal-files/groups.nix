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
      specVersion = "2.4";
      identifier = { name = "groups"; version = "0.5.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2013 Nathan van Doorn";
      maintainer = "nvd1234@gmail.com";
      author = "Nathan \"Taneb\" van Doorn";
      homepage = "";
      url = "";
      synopsis = "Groups";
      description = "A group is a monoid with invertibility.";
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
      url = "http://hackage.haskell.org/package/groups-0.5.3.tar.gz";
      sha256 = "ce1e52a8be7effbd1f995eadf0ed34fa45c412656d372db8a38f9c955e43ac38";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nname:                groups\nversion:             0.5.3\nsynopsis:            Groups\ndescription:\n  A group is a monoid with invertibility.\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Nathan \"Taneb\" van Doorn\nmaintainer:          nvd1234@gmail.com\ncopyright:           Copyright (C) 2013 Nathan van Doorn\ncategory:            Algebra, Data, Math\nbuild-type:          Simple\n\nsource-repository head\n  type:     git\n  location: https://github.com/Taneb/groups.git\n\nlibrary\n  exposed-modules:     Data.Group\n  -- other-modules:\n  build-depends:       base >= 4.6 && < 5\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n";
    }