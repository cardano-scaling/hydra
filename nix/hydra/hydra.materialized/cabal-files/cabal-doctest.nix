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
      identifier = { name = "cabal-doctest"; version = "1.0.9"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017 Oleg Grenrus";
      maintainer = "Andreas Abel";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/haskellari/cabal-doctest";
      url = "";
      synopsis = "A Setup.hs helper for running doctests";
      description = "As of now (end of 2021), there isn't @cabal doctest@\ncommand. Yet, to properly work, @doctest@ needs plenty of configuration.\nThis library provides the common bits for writing a custom @Setup.hs@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-doctest-1.0.9.tar.gz";
      sha256 = "5556088496111d33810c4ae6c4a065bb37fa3315e9e8891e8000b1ab6707ba73";
      });
    }) // {
    package-description-override = "name:               cabal-doctest\nversion:            1.0.9\nx-revision:         2\nsynopsis:           A Setup.hs helper for running doctests\ndescription:\n  As of now (end of 2021), there isn't @cabal doctest@\n  command. Yet, to properly work, @doctest@ needs plenty of configuration.\n  This library provides the common bits for writing a custom @Setup.hs@.\n\nhomepage:           https://github.com/haskellari/cabal-doctest\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Andreas Abel\ncopyright:          (c) 2017 Oleg Grenrus\ncategory:           Distribution\nbuild-type:         Simple\ncabal-version:      >=1.10\nextra-source-files:\n  ChangeLog.md\n  README.md\n\ntested-with:\n  GHC == 9.4.1\n  GHC == 9.2.4\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n  GHC == 7.2.2\n  GHC == 7.0.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/cabal-doctest\n\nlibrary\n  exposed-modules:  Distribution.Extra.Doctest\n  other-modules:\n  other-extensions:\n  build-depends:\n      base       >=4.3  && <5\n    , Cabal      >=1.10 && <3.10\n    , directory\n    , filepath\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:      -Wall\n\n  if !impl(ghc >=7.2)\n    -- Work around a pattern-match coverage checking bug in GHC 7.0\n    ghc-options: -fno-warn-overlapping-patterns\n";
    }