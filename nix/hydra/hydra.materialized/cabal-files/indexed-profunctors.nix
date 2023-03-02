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
      specVersion = "1.24";
      identifier = { name = "indexed-profunctors"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "optics@well-typed.com";
      author = "Adam Gundry, Andres Löh, Andrzej Rybczak, Oleg Grenrus";
      homepage = "";
      url = "";
      synopsis = "Utilities for indexed profunctors";
      description = "This package contains basic definitions related to indexed profunctors.  These\nare primarily intended as internal utilities to support the @optics@ and\n@generic-lens@ package families.";
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
      url = "http://hackage.haskell.org/package/indexed-profunctors-0.1.1.tar.gz";
      sha256 = "5aba418a92a4f75efc626de7c0e4d88ed57033e0de0f2743ce6d9c9ef7626cb1";
      });
    }) // {
    package-description-override = "name:          indexed-profunctors\nversion:       0.1.1\nlicense:       BSD3\nlicense-file:  LICENSE\nbuild-type:    Simple\ncabal-version: 1.24\nmaintainer:    optics@well-typed.com\nauthor:        Adam Gundry, Andres Löh, Andrzej Rybczak, Oleg Grenrus\ntested-with:   GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.4 || ==9.0.1, GHCJS ==8.4\nsynopsis:      Utilities for indexed profunctors\ncategory:      Data, Optics, Lenses, Profunctors\ndescription:\n  This package contains basic definitions related to indexed profunctors.  These\n  are primarily intended as internal utilities to support the @optics@ and\n  @generic-lens@ package families.\n\nextra-doc-files:\n  CHANGELOG.md\n\nbug-reports:   https://github.com/well-typed/optics/issues\nsource-repository head\n  type:     git\n  location: https://github.com/well-typed/optics.git\n  subdir:   indexed-profunctors\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n\n  build-depends: base                   >= 4.10       && <5\n\n  exposed-modules:    Data.Profunctor.Indexed\n";
    }