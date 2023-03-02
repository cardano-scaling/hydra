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
      identifier = { name = "unliftio-core"; version = "0.1.2.0"; };
      license = "MIT";
      copyright = "2017 FP Complete";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Francesco Mazzoli";
      homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
      url = "";
      synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO";
      description = "Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>";
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
      url = "http://hackage.haskell.org/package/unliftio-core-0.1.2.0.tar.gz";
      sha256 = "24c38b3d610ca2642ed496d1de3d7b6b398ce0410aa0a15f3c7ce636ba8f7a78";
      });
    }) // {
    package-description-override = "cabal-version: >= 1.10\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.29.6.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n--\r\n-- hash: 9dda29b2ae88c7aba738c44b9efa373de55a416be845a7bf888fc8c108166fed\r\n\r\nname:           unliftio-core\r\nversion:        0.1.2.0\r\nx-revision: 3\r\nsynopsis:       The MonadUnliftIO typeclass for unlifting monads to IO\r\ndescription:    Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>\r\ncategory:       Control\r\nhomepage:       https://github.com/fpco/unliftio/tree/master/unliftio-core#readme\r\nauthor:         Michael Snoyman, Francesco Mazzoli\r\nmaintainer:     michael@snoyman.com\r\ncopyright:      2017 FP Complete\r\nlicense:        MIT\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    ChangeLog.md\r\n    README.md\r\n\r\nlibrary\r\n  hs-source-dirs:\r\n      src\r\n  build-depends:\r\n      base >=4.5 && <4.15\r\n    , transformers >=0.2 && <0.6\r\n  exposed-modules:\r\n      Control.Monad.IO.Unlift\r\n  other-modules:\r\n      Paths_unliftio_core\r\n  default-language: Haskell2010\r\n";
    }