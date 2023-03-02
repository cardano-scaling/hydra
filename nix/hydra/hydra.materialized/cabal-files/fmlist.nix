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
      identifier = { name = "fmlist"; version = "0.9.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "sjoerd@w3future.com";
      author = "Sjoerd Visscher";
      homepage = "https://github.com/sjoerdvisscher/fmlist";
      url = "";
      synopsis = "FoldMap lists";
      description = "FoldMap lists are lists represented by their foldMap function.\nFoldMap lists have O(1) cons, snoc and append, just like DLists,\nbut other operations might have favorable performance\ncharacteristics as well. These wild claims are still completely\nunverified though.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fmlist-0.9.4.tar.gz";
      sha256 = "2dbdc1850e6768fd5f4e4c45d011ef6796d8b2d639ec200da7f4407ae02d09a6";
      });
    }) // {
    package-description-override = "name:                fmlist\nversion:             0.9.4\nsynopsis:            FoldMap lists\ndescription:\n  FoldMap lists are lists represented by their foldMap function.\n  FoldMap lists have O(1) cons, snoc and append, just like DLists,\n  but other operations might have favorable performance\n  characteristics as well. These wild claims are still completely\n  unverified though.\ncategory:            Data\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Sjoerd Visscher\nmaintainer:          sjoerd@w3future.com\nhomepage:            https://github.com/sjoerdvisscher/fmlist\nbug-reports:         https://github.com/sjoerdvisscher/fmlist/issues\n\nbuild-type:          Simple\ncabal-version:       >= 1.10\n\nLibrary\n  exposed-modules:     Data.FMList\n  build-depends:       base >= 3 && < 5\n  if impl(ghc < 8.0)\n    build-depends: fail\n  default-language:    Haskell98\n\n\nsource-repository head\n  type:     git\n  location: git://github.com/sjoerdvisscher/fmlist.git\n";
    }