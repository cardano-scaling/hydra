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
      identifier = { name = "writer-cps-transformers"; version = "0.5.6.1"; };
      license = "BSD-3-Clause";
      copyright = "2016 Daniel Mendler";
      maintainer = "mail@daniel-mendler.de";
      author = "Andy Gill, Ross Paterson, Daniel Mendler";
      homepage = "https://github.com/minad/writer-cps-transformers#readme";
      url = "";
      synopsis = "WriteT and RWST monad transformers";
      description = "NOTE: From version 0.5.6.0 on the modules provided by this package went upstream to transformers. This package acts as a compatibility package for GHC versions older than 8.6. The WriterT and RWST monad transformers provided by writer-cps-transformers are written in continuation passing style and avoid the space-leak problem of the traditional Control.Monad.Trans.Writer.Strict and Control.Monad.Trans.Writer.Lazy. The corresponding MTL class instances are in the package writer-cps-mtl (<http://hackage.haskell.org/package/writer-cps-mtl>).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/writer-cps-transformers-0.5.6.1.tar.gz";
      sha256 = "76eacf1c3df8f86b6d11507219d7e840d7fb2898f53959aa3dad40791b8f321c";
      });
    }) // {
    package-description-override = "name:           writer-cps-transformers\nversion:        0.5.6.1\nlicense:        BSD3\nlicense-file:   LICENSE\ntested-with:    GHC == 7.10.3, GHC == 8.0.1, GHC == 8.2.1, GHC == 8.4.3, GHC == 8.4.4, GHC == 8.6.1, GHC == 8.6.2, GHC == 8.6.3, GHC == 8.6.4\nauthor:         Andy Gill, Ross Paterson, Daniel Mendler\nmaintainer:     mail@daniel-mendler.de\ncopyright:      2016 Daniel Mendler\nbug-reports:    https://github.com/minad/writer-cps-transformers/issues\ncategory:       Control\nhomepage:       https://github.com/minad/writer-cps-transformers#readme\nsynopsis:       WriteT and RWST monad transformers\ndescription:    NOTE: From version 0.5.6.0 on the modules provided by this package went upstream to transformers. This package acts as a compatibility package for GHC versions older than 8.6. The WriterT and RWST monad transformers provided by writer-cps-transformers are written in continuation passing style and avoid the space-leak problem of the traditional Control.Monad.Trans.Writer.Strict and Control.Monad.Trans.Writer.Lazy. The corresponding MTL class instances are in the package writer-cps-mtl (<http://hackage.haskell.org/package/writer-cps-mtl>).\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\nsource-repository head\n  type: git\n  location: https://github.com/minad/writer-cps-transformers\n\nlibrary\n  build-depends:\n    base <6,\n    transformers >= 0.5.6.0\n  ghc-options: -Wall\n  default-language: Haskell2010\n";
    }