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
      specVersion = "1.12";
      identifier = { name = "newtype"; version = "0.2.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Andreas Abel";
      author = "Herbert Valerio Riedel, Darius Jahandarie, Conor McBride";
      homepage = "";
      url = "";
      synopsis = "A typeclass and set of functions for working with newtypes.";
      description = "Per Conor McBride, the 'Newtype' typeclass represents the packing and unpacking of a @newtype@, and allows you to operate under that @newtype@ with functions such as 'ala'. See \"Control.Newtype\" for documentation and examples.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/newtype-0.2.2.0.tar.gz";
      sha256 = "3a00ffd1bb48a81e09f8be6510fa4c642ba1482b2f8d4777af1b5dd06c55ebac";
      });
    }) // {
    package-description-override = "cabal-version:       1.12\nbuild-type:          Simple\nname:                newtype\nversion:             0.2.2.0\nx-revision:          3\n\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Herbert Valerio Riedel, Darius Jahandarie, Conor McBride\nmaintainer:          Andreas Abel\ncategory:            Control\nbug-reports:         https://github.com/hvr/newtype/issues\n\nsynopsis:            A typeclass and set of functions for working with newtypes.\ndescription:         Per Conor McBride, the 'Newtype' typeclass represents the packing and unpacking of a @newtype@, and allows you to operate under that @newtype@ with functions such as 'ala'. See \"Control.Newtype\" for documentation and examples.\n\ntested-with:\n   GHC == 9.4.1\n   GHC == 9.2.2\n   GHC == 9.0.2\n   GHC == 8.10.7\n   GHC == 8.8.4\n   GHC == 8.6.5\n   GHC == 8.4.4\n   GHC == 8.2.2\n   GHC == 8.0.2\n   GHC == 7.10.3\n   GHC == 7.8.4\n   GHC == 7.6.3\n   GHC == 7.4.2\n   GHC == 7.0.4\n\nextra-source-files:    CHANGES.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/hvr/newtype.git\n\nlibrary\n  exposed-modules:     Control.Newtype\n\n  build-depends:       base >= 4.3 && < 5\n  if !impl(ghc >= 8.0)\n    build-depends:     transformers >= 0.2.2.0 && < 0.6\n\n  default-language:    Haskell2010\n  other-extensions:\n    CPP\n    FlexibleInstances\n    FunctionalDependencies\n    MultiParamTypeClasses\n    TypeFamilies\n\n  if impl(ghc >= 7.2)\n    default-extensions: Trustworthy\n    if impl(ghc >= 7.10) { ghc-options: -fno-warn-trustworthy-safe }\n\n  ghc-options: -Wall\n";
    }