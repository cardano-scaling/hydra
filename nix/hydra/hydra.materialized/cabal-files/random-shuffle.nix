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
      identifier = { name = "random-shuffle"; version = "0.0.4"; };
      license = "BSD-3-Clause";
      copyright = "Oleg Kiselyov 2001\nManlio Perillo 2009";
      maintainer = "Manlio Perillo <manlio.perillo@gmail.com>";
      author = "Oleg Kiselyov, Manlio Perillo, Andras Slemmer";
      homepage = "";
      url = "";
      synopsis = "Random shuffle implementation.";
      description = "Random shuffle implementation, on immutable lists.\nBased on `perfect shuffle' implementation by Oleg Kiselyov,\navailable on http://okmij.org/ftp/Haskell/perfect-shuffle.txt";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/random-shuffle-0.0.4.tar.gz";
      sha256 = "52704411f040fd0bf2361dad162e35dc13caa6535b2e4908d3513c00a95d0615";
      });
    }) // {
    package-description-override = "cabal-version:    >= 1.6\nbuild-type:       Simple\nname:             random-shuffle\nversion:          0.0.4\nlicense:          BSD3\nlicense-file:     LICENSE\ncategory:         System\nauthor:           Oleg Kiselyov, Manlio Perillo, Andras Slemmer\nmaintainer:       Manlio Perillo <manlio.perillo@gmail.com>\ncopyright:        Oleg Kiselyov 2001\n                  Manlio Perillo 2009\nsynopsis:         Random shuffle implementation.\ndescription:\n    Random shuffle implementation, on immutable lists.\n\n    Based on `perfect shuffle' implementation by Oleg Kiselyov,\n    available on http://okmij.org/ftp/Haskell/perfect-shuffle.txt\nstability:        Beta\n\nlibrary\n    build-depends:      base < 5, random, MonadRandom\n    exposed-modules:    System.Random.Shuffle\n    hs-source-dirs:     src\n    ghc-options:        -Wall\n";
    }