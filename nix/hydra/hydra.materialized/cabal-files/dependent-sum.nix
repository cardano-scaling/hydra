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
      specVersion = "1.22";
      identifier = { name = "dependent-sum"; version = "0.7.1.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "Obsidian Systems, LLC <maintainer@obsidian.systems>";
      author = "James Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/obsidiansystems/dependent-sum";
      url = "";
      synopsis = "Dependent sum type";
      description = "A dependent sum is a generalization of a\nparticular way of thinking about the @Either@\ntype.  @Either a b@ can be thought of as a\n2-tuple @(tag, value)@, where the value of the\ntag determines the type of the value.  In\nparticular, either @tag = Left@ and @value :: a@\nor @tag = Right@ and @value :: b@.\n\nThis package allows you to define your own\ndependent sum types by using your own \\\"tag\\\"\ntypes.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dependent-sum-0.7.1.0.tar.gz";
      sha256 = "81cb55907f321f62bea095ae72e9711095c4cb7378fa66fbabc483a9f61b462a";
      });
    }) // {
    package-description-override = "name:                   dependent-sum\r\nversion:                0.7.1.0\r\nx-revision: 2\r\nstability:              provisional\r\n\r\ncabal-version:          1.22\r\nbuild-type:             Simple\r\n\r\nauthor:                 James Cook <mokus@deepbondi.net>\r\nmaintainer:             Obsidian Systems, LLC <maintainer@obsidian.systems>\r\nlicense:                PublicDomain\r\nhomepage:               https://github.com/obsidiansystems/dependent-sum\r\n\r\ncategory:               Data, Dependent Types\r\nsynopsis:               Dependent sum type\r\ndescription:            A dependent sum is a generalization of a\r\n                        particular way of thinking about the @Either@\r\n                        type.  @Either a b@ can be thought of as a\r\n                        2-tuple @(tag, value)@, where the value of the\r\n                        tag determines the type of the value.  In\r\n                        particular, either @tag = Left@ and @value :: a@\r\n                        or @tag = Right@ and @value :: b@.\r\n                        .\r\n                        This package allows you to define your own\r\n                        dependent sum types by using your own \\\"tag\\\"\r\n                        types.\r\n\r\ntested-with:            GHC == 8.0.2,\r\n                        GHC == 8.2.2,\r\n                        GHC == 8.4.4,\r\n                        GHC == 8.6.5,\r\n                        GHC == 8.8.3\r\n\r\nextra-source-files:     ChangeLog.md\r\n                      , examples/*.hs\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/obsidiansystems/dependent-sum\r\n\r\nLibrary\r\n  default-language:     Haskell2010\r\n  hs-source-dirs:       src\r\n  exposed-modules:      Data.Dependent.Sum\r\n  reexported-modules:   Data.GADT.Compare,\r\n                        Data.GADT.Show,\r\n                        Data.Some\r\n  other-extensions:     PatternSynonyms\r\n  build-depends:        base >= 4.9 && <5\r\n                      , constraints-extras >= 0.2 && < 0.4\r\n\r\n  -- tight bounds, so re-exported API is versioned properly.\r\n  build-depends:        some >= 1.0.1 && < 1.0.4\r\n\r\n  if impl(ghc >= 7.2)\r\n    ghc-options:        -trust base\r\n";
    }