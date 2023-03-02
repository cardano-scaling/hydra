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
      identifier = { name = "th-extras"; version = "0.0.0.6"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "James Cook <mokus@deepbondi.net>";
      author = "James Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/mokus0/th-extras";
      url = "";
      synopsis = "A grab bag of functions for use with Template Haskell";
      description = "A grab bag of functions for use with Template Haskell.\n\nThis is basically the place I put all my ugly CPP hacks to support\nthe ever-changing interface of the template haskell system by\nproviding high-level operations and making sure they work on as many\nversions of Template Haskell as I can.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-extras-0.0.0.6.tar.gz";
      sha256 = "02bf23940c0233a6ef6f61868e827ebd4554afe8d71cef2a1eb8e286a7f07c4a";
      });
    }) // {
    package-description-override = "name:                   th-extras\nversion:                0.0.0.6\nstability:              experimental\n\ncabal-version:          >= 1.10\nbuild-type:             Simple\n\nauthor:                 James Cook <mokus@deepbondi.net>\nmaintainer:             James Cook <mokus@deepbondi.net>\nlicense:                PublicDomain\nhomepage:               https://github.com/mokus0/th-extras\n\ncategory:               Template Haskell\nsynopsis:               A grab bag of functions for use with Template Haskell\ndescription:            A grab bag of functions for use with Template Haskell.\n                        .\n                        This is basically the place I put all my ugly CPP hacks to support\n                        the ever-changing interface of the template haskell system by\n                        providing high-level operations and making sure they work on as many\n                        versions of Template Haskell as I can.\n\ntested-with:            GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5,\n                        GHC == 8.8.4, GHC == 8.10.2, GHC == 9.0.1\n\nextra-source-files:     ReadMe.md\n                      , ChangeLog.md\n\nsource-repository head\n  type:                 git\n  location:             https://github.com/mokus0/th-extras.git\n\nLibrary\n  hs-source-dirs:       src\n  exposed-modules:      Language.Haskell.TH.Extras\n  build-depends:        base >= 4.9 && < 5\n                      , containers\n                      , template-haskell < 2.19\n                      , th-abstraction >= 0.4 && < 0.5\n                      , syb\n  default-language:     Haskell2010\n";
    }