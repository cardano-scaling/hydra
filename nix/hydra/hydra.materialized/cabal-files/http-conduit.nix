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
    flags = { aeson = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "http-conduit"; version = "2.3.8"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/http-conduit";
      url = "";
      synopsis = "HTTP client package with conduit interface and HTTPS support.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-conduit>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ] ++ (pkgs.lib).optional (flags.aeson) (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.9")) (hsPkgs."void" or (errorHandler.buildDepError "void"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."warp-tls" or (errorHandler.buildDepError "warp-tls"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."wai-conduit" or (errorHandler.buildDepError "wai-conduit"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ] ++ (pkgs.lib).optional (flags.aeson) (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-conduit-2.3.8.tar.gz";
      sha256 = "cfbef293856fdcce58618726ff911ca28e2ad07c8522b2cd1cfa2cb6e02542ae";
      });
    }) // {
    package-description-override = "cabal-version:   >= 1.10\nname:            http-conduit\nversion:         2.3.8\nlicense:         BSD3\nlicense-file:    LICENSE\nauthor:          Michael Snoyman <michael@snoyman.com>\nmaintainer:      Michael Snoyman <michael@snoyman.com>\nsynopsis:        HTTP client package with conduit interface and HTTPS support.\ndescription:         Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-conduit>.\ncategory:        Web, Conduit\nstability:       Stable\nbuild-type:      Simple\nhomepage:        http://www.yesodweb.com/book/http-conduit\nextra-source-files: test/main.hs\n                  , test/CookieTest.hs\n                  , multipart-example.bin\n                  , nyan.gif\n                  , certificate.pem\n                  , key.pem\n                  , README.md\n                  , ChangeLog.md\n\nflag aeson\n  manual: True\n  description: Enable the dependency on aeson\n  default: True\n\nlibrary\n    default-language: Haskell2010\n    build-depends: base                  >= 4.10    && < 5\n                 , attoparsec\n                 , bytestring            >= 0.9.1.4\n                 , transformers          >= 0.2\n                 , resourcet             >= 1.1\n                 , conduit               >= 1.2\n                 , conduit-extra         >= 1.1\n                 , http-types            >= 0.7\n                 , http-client           >= 0.5.13  && < 0.8\n                 , http-client-tls       >= 0.3     && < 0.4\n                 , mtl\n                 , unliftio-core\n\n    if flag(aeson)\n      build-depends: aeson                 >= 0.8\n\n    if !impl(ghc>=7.9)\n      build-depends:   void >= 0.5.5\n    exposed-modules: Network.HTTP.Conduit\n                     Network.HTTP.Client.Conduit\n                     Network.HTTP.Simple\n    ghc-options:     -Wall\n\ntest-suite test\n    default-language: Haskell2010\n    main-is: main.hs\n    other-modules: CookieTest\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test\n\n    ghc-options:   -Wall\n    cpp-options:   -DDEBUG\n    build-depends: base >= 4 && < 5\n                 , HUnit\n                 , hspec >= 1.3\n                 , data-default-class\n                 , connection >= 0.2\n                 , warp-tls\n                 , tls < 1.5 || >= 1.5.2\n                 , time\n                 , blaze-builder\n                 , bytestring\n                 , text\n                 , transformers\n                 , conduit >= 1.1\n                 , utf8-string\n                 , case-insensitive\n                 , unliftio\n                 , wai >= 3.0 && < 3.3\n                 , warp >= 3.0.0.2 && < 3.4\n                 , wai-conduit\n                 , http-types\n                 , cookie\n                 , http-client\n                 , http-conduit\n                 , conduit-extra\n                 , streaming-commons\n                 , temporary\n                 , resourcet\n                 , network\n\n    if flag(aeson)\n      build-depends: aeson\n\nsource-repository head\n  type:     git\n  location: git://github.com/snoyberg/http-client.git\n";
    }