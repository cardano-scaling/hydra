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
      identifier = { name = "Only"; version = "0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "";
      url = "";
      synopsis = "The 1-tuple type or single-value \"collection\"";
      description = "This package provides a canonical anonymous 1-tuple type missing\nfrom Haskell for attaching typeclass instances.\n\nNOTE: There is also the </package/OneTuple OneTuple package> which\nby using a boxed @data@-type provides a 1-tuple type which has\nlaziness properties which are more faithful to the ones of Haskell's\nnative tuples; whereas the primary purpose of 'Only' is to\nprovide the traditionally so named type-wrapper for attaching typeclass\ninstances.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.4" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Only-0.1.tar.gz";
      sha256 = "ab7aa193e8c257d3bda6b0b3c1cbcf74cdaa85ab08cb20c2dd62ba248c1ab265";
      });
    }) // {
    package-description-override = "name:                Only\nversion:             0.1\nx-revision: 1\nsynopsis:            The 1-tuple type or single-value \"collection\"\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Herbert Valerio Riedel\nmaintainer:          hvr@gnu.org\nbug-reports:         https://github.com/hvr/Only/issues\ncategory:            Data\nbuild-type:          Simple\ncabal-version:       >=1.10\ndescription:\n  This package provides a canonical anonymous 1-tuple type missing\n  from Haskell for attaching typeclass instances.\n  .\n  NOTE: There is also the </package/OneTuple OneTuple package> which\n  by using a boxed @data@-type provides a 1-tuple type which has\n  laziness properties which are more faithful to the ones of Haskell's\n  native tuples; whereas the primary purpose of 'Only' is to\n  provide the traditionally so named type-wrapper for attaching typeclass\n  instances.\n\nSource-Repository head\n  Type:              git\n  Location:          https://github.com/hvr/Only.git\n\nlibrary\n  hs-source-dirs:    src\n  exposed-modules:   Data.Tuple.Only\n\n  default-language:  Haskell2010\n  other-extensions:  DeriveGeneric\n                   , DeriveDataTypeable\n                   , DeriveFunctor\n                   , Safe\n\n  build-depends:     base    >= 4.5 && <5\n                   , deepseq >= 1.1 && <1.5\n\n  if impl(ghc == 7.4.*)\n     build-depends: ghc-prim\n";
    }