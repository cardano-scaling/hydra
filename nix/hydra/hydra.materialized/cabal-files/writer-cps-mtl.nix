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
      identifier = { name = "writer-cps-mtl"; version = "0.1.1.6"; };
      license = "BSD-3-Clause";
      copyright = "2016 Daniel Mendler";
      maintainer = "mail@daniel-mendler.de";
      author = "Andy Gill, Edward Kmett, Daniel Mendler";
      homepage = "https://github.com/minad/writer-cps-mtl#readme";
      url = "";
      synopsis = "MonadWriter orphan instances for writer-cps-transformers";
      description = "The WriterT and RWST monad transformers provided by writer-cps-transformers are written in continuation passing style and avoid the space-leak problem of the traditional Control.Monad.Trans.Writer.Strict and Control.Monad.Trans.Writer.Lazy. See also (<http://hackage.haskell.org/package/writer-cps-transformers>).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."writer-cps-transformers" or (errorHandler.buildDepError "writer-cps-transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/writer-cps-mtl-0.1.1.6.tar.gz";
      sha256 = "06f9fb60dc41ad26f3d18089a0b7ff1e1aeb15dc862508c59b6b577c0914dd36";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.31.1.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 522a344e5b14c30ac172bd9ebb6ea7c7e3f1d5ebcd889f7326611c19eaf9c565\n\nname:           writer-cps-mtl\nversion:        0.1.1.6\nx-revision: 1\nlicense:        BSD3\nlicense-file:   LICENSE\ntested-with:    GHC == 7.10.3, GHC == 8.0.1, GHC == 8.2.1, GHC == 8.4.3, GHC == 8.6.1\nauthor:         Andy Gill, Edward Kmett, Daniel Mendler\nmaintainer:     mail@daniel-mendler.de\ncopyright:      2016 Daniel Mendler\ncategory:       Control\nsynopsis:       MonadWriter orphan instances for writer-cps-transformers\nhomepage:       https://github.com/minad/writer-cps-mtl#readme\nbug-reports:    https://github.com/minad/writer-cps-mtl/issues\ndescription:    The WriterT and RWST monad transformers provided by writer-cps-transformers are written in continuation passing style and avoid the space-leak problem of the traditional Control.Monad.Trans.Writer.Strict and Control.Monad.Trans.Writer.Lazy. See also (<http://hackage.haskell.org/package/writer-cps-transformers>).\nbuild-type:     Simple\n\nsource-repository head\n  type: git\n  location: https://github.com/minad/writer-cps-mtl\n\nlibrary\n  build-depends: mtl <2.3\n\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base <6\n    , mtl >=2.2 && <2.4\n    , transformers >=0.4 && <0.6\n    , writer-cps-transformers >=0.1.1.1 && <0.6\n  exposed-modules:\n      Control.Monad.RWS.CPS\n      Control.Monad.Writer.CPS\n  other-modules:\n      Paths_writer_cps_mtl\n  default-language: Haskell2010\n";
    }