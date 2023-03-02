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
      identifier = { name = "dependent-map"; version = "0.4.0.0"; };
      license = "LicenseRef-OtherLicense";
      copyright = "";
      maintainer = "Obsidian Systems, LLC <maintainer@obsidian.systems>";
      author = "James Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/obsidiansystems/dependent-map";
      url = "";
      synopsis = "Dependent finite maps (partial dependent products)";
      description = "Provides a type called @DMap@ which generalizes\n@Data.Map.Map@, allowing keys to specify the type\nof value that can be associated with them.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dependent-map-0.4.0.0.tar.gz";
      sha256 = "53ce0b52d8be1b85fc6489fb27656f16d837bee4fbe0ddf39c844e3ea8871f2c";
      });
    }) // {
    package-description-override = "name:                   dependent-map\nversion:                0.4.0.0\nstability:              provisional\n\ncabal-version:          >= 1.6\nbuild-type:             Simple\n\nauthor:                 James Cook <mokus@deepbondi.net>\nmaintainer:             Obsidian Systems, LLC <maintainer@obsidian.systems>\nlicense:                OtherLicense\nlicense-file:           LICENSE\nhomepage:               https://github.com/obsidiansystems/dependent-map\n\ncategory:               Data, Dependent Types\nsynopsis:               Dependent finite maps (partial dependent products)\ndescription:            Provides a type called @DMap@ which generalizes\n                        @Data.Map.Map@, allowing keys to specify the type\n                        of value that can be associated with them.\n\nextra-source-files: ChangeLog.md\n                    README.md\n\ntested-with:            GHC == 8.0.2,\n                        GHC == 8.2.2,\n                        GHC == 8.4.4,\n                        GHC == 8.6.5,\n                        GHC == 8.8.3\n\nsource-repository head\n  type:     git\n  location: https://github.com/obsidiansystems/dependent-map\n\nLibrary\n  hs-source-dirs:       src\n  ghc-options:          -fwarn-unused-imports -fwarn-unused-binds\n  exposed-modules:      Data.Dependent.Map,\n                        Data.Dependent.Map.Lens,\n                        Data.Dependent.Map.Internal\n  other-modules:        Data.Dependent.Map.PtrEquality\n  build-depends:        base >= 4.9 && < 5,\n                        containers >= 0.5.7.1 && <0.7,\n                        dependent-sum >= 0.6.1 && < 0.8,\n                        constraints-extras >= 0.2.3.0 && < 0.4\n";
    }