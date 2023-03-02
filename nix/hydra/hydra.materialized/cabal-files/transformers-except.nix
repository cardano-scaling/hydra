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
      specVersion = "3.0";
      identifier = { name = "transformers-except"; version = "0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017 Tim McGilchrist";
      maintainer = "Tim McGilchrist <timmcgil@gmail.com>";
      author = "Tim McGilchrist <timmcgil@gmail.com>";
      homepage = "http://github.com/tmcgilchrist/transformers-either/";
      url = "";
      synopsis = "An Except monad transformer with ";
      description = "Extra pieces for working with Except";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-except-0.1.2.tar.gz";
      sha256 = "9925f82037b9aa9d382e98cff91a6ca12009093b7aaa1f583c91429b6ebcfd03";
      });
    }) // {
    package-description-override = "cabal-version:         3.0\nname:                  transformers-except\nversion:               0.1.2\nlicense:               BSD-3-Clause\nlicense-file:          LICENSE\nauthor:                Tim McGilchrist <timmcgil@gmail.com>\nmaintainer:            Tim McGilchrist <timmcgil@gmail.com>\ncopyright:             (c) 2017 Tim McGilchrist\nsynopsis:              An Except monad transformer with \ncategory:              System\nbuild-type:            Simple\ntested-with:           GHC == 8.10.5\n                     , GHC == 8.8.4\n                     , GHC == 8.6.5\n                     , GHC == 8.4.4\n                     , GHC == 8.2.2\n                     , GHC == 8.0.2\n\ndescription:           Extra pieces for working with Except\nhomepage:      http://github.com/tmcgilchrist/transformers-either/\nbug-reports:   http://github.com/tmcgilchrist/transformers-either/issues\nsource-repository head\n  type: git\n  location: https://github.com/tmcgilchrist/transformers-either.git\n\nlibrary\n  build-depends:\n                       base                            >= 4.8        && < 5\n                     , text                            == 1.2.*\n                     , exceptions                      >= 0.6        && < 0.11\n                     , transformers                    >= 0.4        && < 0.6\n\n  ghc-options:\n                       -Wall\n  default-language:   Haskell98\n  hs-source-dirs:\n                       src\n\n  exposed-modules:\n                       Control.Monad.Trans.Except.Extra\n                       Control.Monad.Trans.Except.Exit\n";
    }