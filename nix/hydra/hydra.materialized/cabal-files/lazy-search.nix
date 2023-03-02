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
      identifier = { name = "lazy-search"; version = "0.1.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "byorgey@gmail.com";
      author = "Jonas Duregard";
      homepage = "";
      url = "";
      synopsis = "Finds values satisfying a lazy predicate";
      description = "This library can be used as a property based testing driver, and more\ngenerally to find values satisfying a predicate (@a -> Bool@). This\nis done by a size bounded search, and it uses the laziness of the\npredicate to speed up the search by avoiding isomorphic values.\n\nThis is similar to \"LazySmallCheck\" but uses size instead of depth\nand a faster algorithm.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lazy-search-0.1.3.0.tar.gz";
      sha256 = "56a7ec14260265d3c739f42a519b7cdd9c4ebf8d64ee1b290a05a284ee139aa8";
      });
    }) // {
    package-description-override = "name:                lazy-search\r\nversion:             0.1.3.0\r\nsynopsis:            Finds values satisfying a lazy predicate\r\ndescription:\r\n  This library can be used as a property based testing driver, and more\r\n  generally to find values satisfying a predicate (@a -> Bool@). This\r\n  is done by a size bounded search, and it uses the laziness of the\r\n  predicate to speed up the search by avoiding isomorphic values.\r\n  .\r\n  This is similar to \"LazySmallCheck\" but uses size instead of depth\r\n  and a faster algorithm.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Jonas Duregard\r\nmaintainer:          byorgey@gmail.com\r\n-- copyright:\r\ncategory:            Testing\r\nbuild-type:          Simple\r\nextra-source-files:  CHANGELOG.md\r\ncabal-version:       >=1.10\r\ntested-with:         GHC ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.2 || ==9.2.2\r\n\r\nsource-repository head\r\n  type:      git\r\n  location:  https://github.com/size-based/lazy-search\r\n\r\nlibrary\r\n  exposed-modules:     Data.Coolean, Control.Search\r\n  -- other-modules:\r\n  -- other-extensions:\r\n  build-depends:       base >=4.7 && <5, size-based >=0.1 && <0.2\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  -- ghc-options:   -Wall";
    }