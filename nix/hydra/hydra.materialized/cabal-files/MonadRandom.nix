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
      identifier = { name = "MonadRandom"; version = "0.5.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Brent Yorgey <byorgey@gmail.com>";
      author = "Cale Gibbard and others";
      homepage = "";
      url = "";
      synopsis = "Random-number generation monad.";
      description = "Support for computations which consume random values.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/MonadRandom-0.5.3.tar.gz";
      sha256 = "27184dadda0a49abac0208a1e6576b14217a60dc45b6839cd9e90af25ee00a9f";
      });
    }) // {
    package-description-override = "name:                MonadRandom\r\nversion:             0.5.3\r\nx-revision: 2\r\nsynopsis:            Random-number generation monad.\r\ndescription:         Support for computations which consume random values.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Cale Gibbard and others\r\nmaintainer:          Brent Yorgey <byorgey@gmail.com>\r\nbug-reports:         https://github.com/byorgey/MonadRandom/issues\r\ncategory:            Control\r\nbuild-type:          Simple\r\ncabal-version:       >=1.10\r\nextra-source-files:  CHANGES.markdown\r\ntested-with:         GHC ==7.6.3 || ==7.8.4 || ==7.10.3 || ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.3 || ==8.10.1\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/byorgey/MonadRandom.git\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Control.Monad.Random,\r\n    Control.Monad.Random.Class,\r\n    Control.Monad.Random.Lazy,\r\n    Control.Monad.Random.Strict,\r\n    Control.Monad.Trans.Random,\r\n    Control.Monad.Trans.Random.Lazy,\r\n    Control.Monad.Trans.Random.Strict\r\n  build-depends:\r\n    base                >=4.6 && <5,\r\n    transformers        >=0.3 && <0.6,\r\n    transformers-compat >=0.4 && <0.8,\r\n    mtl                 >=2.1 && <2.3,\r\n    primitive           >=0.6 && <0.8,\r\n    random              >=1.0 && <1.3\r\n  ghc-options:         -Wall\r\n  default-language:    Haskell2010\r\n  other-extensions:    Safe\r\n\r\n  if impl(ghc < 8.0)\r\n    build-depends: fail >= 4.9\r\n";
    }