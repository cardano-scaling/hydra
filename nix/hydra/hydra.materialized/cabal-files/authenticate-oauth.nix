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
      identifier = { name = "authenticate-oauth"; version = "1.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Hiromi Ishii, Artem Chirkin";
      author = "Hiromi Ishii";
      homepage = "http://github.com/yesodweb/authenticate";
      url = "";
      synopsis = "Library to authenticate with OAuth for Haskell web applications.";
      description = "API docs and the README are available at <http://www.stackage.org/package/authenticate-oauth>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."crypto-pubkey-types" or (errorHandler.buildDepError "crypto-pubkey-types"))
          (hsPkgs."RSA" or (errorHandler.buildDepError "RSA"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/authenticate-oauth-1.7.tar.gz";
      sha256 = "746ff695fec1bd7c7b90f1952847ce3453fadf0f18a31db206753360b3219b78";
      });
    }) // {
    package-description-override = "cabal-version:   >= 1.10\r\nname:            authenticate-oauth\r\nversion:         1.7\r\nx-revision: 1\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\nauthor:          Hiromi Ishii\r\nmaintainer:      Hiromi Ishii, Artem Chirkin\r\nsynopsis:        Library to authenticate with OAuth for Haskell web applications.\r\ndescription:     API docs and the README are available at <http://www.stackage.org/package/authenticate-oauth>.\r\ncategory:        Web\r\nstability:       Stable\r\nbuild-type:      Simple\r\nhomepage:        http://github.com/yesodweb/authenticate\r\nextra-source-files: README.md ChangeLog.md\r\n\r\nlibrary\r\n    default-language: Haskell2010\r\n    build-depends:   base                          >= 4.10     && < 5\r\n                   , http-client                   >= 0.3\r\n                   , transformers                  >= 0.1      && < 0.7\r\n                   , bytestring                    >= 0.9\r\n                   , crypto-pubkey-types           >= 0.1      && < 0.5\r\n                   , RSA                           >= 2.0      && < 2.5\r\n                   , time\r\n                   , data-default\r\n                   , base64-bytestring             >= 0.1      && < 1.3\r\n                   , SHA                           >= 1.4      && < 1.7\r\n                   , random\r\n                   , http-types                    >= 0.6\r\n                   , blaze-builder\r\n                   , transformers-compat           >= 0.3\r\n    exposed-modules: Web.Authenticate.OAuth, Web.Authenticate.OAuth.IO\r\n    ghc-options:     -Wall\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/yesodweb/authenticate.git\r\n";
    }