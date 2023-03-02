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
      identifier = { name = "dependent-sum-template"; version = "0.1.1.1"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "Obsidian Systems, LLC <maintainer@obsidian.systems>";
      author = "James Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/obsidiansystems/dependent-sum";
      url = "";
      synopsis = "Template Haskell code to generate instances of classes in dependent-sum package";
      description = "Template Haskell code to generate instances of classes in dependent-sum package, such as 'GEq' and 'GCompare'.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-extras" or (errorHandler.buildDepError "th-extras"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          ];
        buildable = if compiler.isGhc && (compiler.version).lt "7.10"
          then false
          else true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dependent-sum-template-0.1.1.1.tar.gz";
      sha256 = "d46fc18d5c2f5d385f901777e982e8a1ae9094ba7e68f7e4997a82036375ae0f";
      });
    }) // {
    package-description-override = "name:                   dependent-sum-template\nversion:                0.1.1.1\nstability:              experimental\n\ncabal-version:          >= 1.10\nbuild-type:             Simple\n\nauthor:                 James Cook <mokus@deepbondi.net>\nmaintainer:             Obsidian Systems, LLC <maintainer@obsidian.systems>\nlicense:                PublicDomain\nhomepage:               https://github.com/obsidiansystems/dependent-sum\n\ncategory:               Unclassified\nsynopsis:               Template Haskell code to generate instances of classes in dependent-sum package\ndescription:            Template Haskell code to generate instances of classes in dependent-sum package, such as 'GEq' and 'GCompare'.\n\ntested-with:            GHC == 8.0.2,\n                        GHC == 8.2.2,\n                        GHC == 8.4.4,\n                        GHC == 8.6.5,\n                        GHC == 8.8.3,\n                        GHC == 9.0.1\n\nextra-source-files:     ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/obsidiansystems/dependent-sum\n\nLibrary\n  if impl(ghc < 7.10)\n    buildable: False\n  hs-source-dirs:       src\n  default-language:     Haskell2010\n  exposed-modules:      Data.GADT.Compare.TH\n                        Data.GADT.Show.TH\n  other-modules:        Data.Dependent.Sum.TH.Internal\n  build-depends:        base >= 3 && <5,\n                        dependent-sum >= 0.4.1 && < 0.8,\n                        template-haskell,\n                        th-extras >= 0.0.0.2,\n                        th-abstraction >= 0.4\n\ntest-suite test\n  if impl(ghc < 8.0)\n    buildable: False\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-language:     Haskell2010\n  main-is: test.hs\n  build-depends: base\n               , constraints-extras\n               , dependent-sum\n               , dependent-sum-template\n";
    }