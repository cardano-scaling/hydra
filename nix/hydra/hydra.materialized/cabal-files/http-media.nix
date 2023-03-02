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
      identifier = { name = "http-media"; version = "0.8.0.0"; };
      license = "MIT";
      copyright = "(c) 2012-2019 Timothy Jones";
      maintainer = "Timothy Jones <tim@zmthy.net>";
      author = "Timothy Jones";
      homepage = "https://github.com/zmthy/http-media";
      url = "";
      synopsis = "Processing HTTP Content-Type and Accept headers";
      description = "This library is intended to be a comprehensive solution to parsing and\nselecting quality-indexed values in HTTP headers.  It is capable of\nparsing both media types and language parameters from the Accept and\nContent header families, and can be extended to match against other\naccept headers as well.  Selecting the appropriate header value is\nachieved by comparing a list of server options against the\nquality-indexed values supplied by the client.\n\nIn the following example, the Accept header is parsed and then matched\nagainst a list of server options to serve the appropriate media using\n'mapAcceptMedia':\n\n> getHeader >>= maybe send406Error sendResourceWith . mapAcceptMedia\n>     [ (\"text/html\",        asHtml)\n>     , (\"application/json\", asJson)\n>     ]\n\nSimilarly, the Content-Type header can be used to produce a parser for\nrequest bodies based on the given content type with 'mapContentMedia':\n\n> getContentType >>= maybe send415Error readRequestBodyWith . mapContentMedia\n>     [ (\"application/json\", parseJson)\n>     , (\"text/plain\",       parseText)\n>     ]\n\nThe API is agnostic to your choice of server.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ];
        buildable = true;
        };
      tests = {
        "test-http-media" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-media-0.8.0.0.tar.gz";
      sha256 = "398279d1dff5b60cd8b8c650caceca248ea1184d694bedf5df5426963b2b9c53";
      });
    }) // {
    package-description-override = "name:          http-media\r\nversion:       0.8.0.0\r\nx-revision: 7\r\nlicense:       MIT\r\nlicense-file:  LICENSE\r\nauthor:        Timothy Jones\r\nmaintainer:    Timothy Jones <tim@zmthy.net>\r\nhomepage:      https://github.com/zmthy/http-media\r\nbug-reports:   https://github.com/zmthy/http-media/issues\r\ncopyright:     (c) 2012-2019 Timothy Jones\r\ncategory:      Web\r\nbuild-type:    Simple\r\ncabal-version: >= 1.10\r\nsynopsis:      Processing HTTP Content-Type and Accept headers\r\ndescription:\r\n  This library is intended to be a comprehensive solution to parsing and\r\n  selecting quality-indexed values in HTTP headers.  It is capable of\r\n  parsing both media types and language parameters from the Accept and\r\n  Content header families, and can be extended to match against other\r\n  accept headers as well.  Selecting the appropriate header value is\r\n  achieved by comparing a list of server options against the\r\n  quality-indexed values supplied by the client.\r\n  .\r\n  In the following example, the Accept header is parsed and then matched\r\n  against a list of server options to serve the appropriate media using\r\n  'mapAcceptMedia':\r\n  .\r\n  > getHeader >>= maybe send406Error sendResourceWith . mapAcceptMedia\r\n  >     [ (\"text/html\",        asHtml)\r\n  >     , (\"application/json\", asJson)\r\n  >     ]\r\n  .\r\n  Similarly, the Content-Type header can be used to produce a parser for\r\n  request bodies based on the given content type with 'mapContentMedia':\r\n  .\r\n  > getContentType >>= maybe send415Error readRequestBodyWith . mapContentMedia\r\n  >     [ (\"application/json\", parseJson)\r\n  >     , (\"text/plain\",       parseText)\r\n  >     ]\r\n  .\r\n  The API is agnostic to your choice of server.\r\n\r\nextra-source-files:\r\n  CHANGES.md\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n\r\n  ghc-options: -Wall\r\n\r\n  hs-source-dirs:\r\n    src\r\n\r\n  default-extensions:\r\n    OverloadedStrings\r\n\r\n  exposed-modules:\r\n    Network.HTTP.Media\r\n    Network.HTTP.Media.Accept\r\n    Network.HTTP.Media.Charset\r\n    Network.HTTP.Media.Encoding\r\n    Network.HTTP.Media.Language\r\n    Network.HTTP.Media.MediaType\r\n    Network.HTTP.Media.RenderHeader\r\n\r\n  other-modules:\r\n    Network.HTTP.Media.Charset.Internal\r\n    Network.HTTP.Media.Encoding.Internal\r\n    Network.HTTP.Media.Language.Internal\r\n    Network.HTTP.Media.MediaType.Internal\r\n    Network.HTTP.Media.Quality\r\n    Network.HTTP.Media.Utils\r\n\r\n  build-depends:\r\n    base             >= 4.8  && < 4.18,\r\n    bytestring       >= 0.10 && < 0.12,\r\n    case-insensitive >= 1.0  && < 1.3,\r\n    containers       >= 0.5  && < 0.7,\r\n    utf8-string      >= 0.3  && < 1.1\r\n\r\ntest-suite test-http-media\r\n  type:    exitcode-stdio-1.0\r\n  main-is: Test.hs\r\n\r\n  default-language: Haskell2010\r\n\r\n  ghc-options: -Wall\r\n\r\n  hs-source-dirs:\r\n    src\r\n    test\r\n\r\n  default-extensions:\r\n    OverloadedStrings\r\n\r\n  other-extensions:\r\n    TupleSections\r\n\r\n  other-modules:\r\n    Network.HTTP.Media\r\n    Network.HTTP.Media.Accept\r\n    Network.HTTP.Media.Accept.Tests\r\n    Network.HTTP.Media.Charset\r\n    Network.HTTP.Media.Charset.Gen\r\n    Network.HTTP.Media.Charset.Internal\r\n    Network.HTTP.Media.Charset.Tests\r\n    Network.HTTP.Media.Gen\r\n    Network.HTTP.Media.Encoding\r\n    Network.HTTP.Media.Encoding.Gen\r\n    Network.HTTP.Media.Encoding.Internal\r\n    Network.HTTP.Media.Encoding.Tests\r\n    Network.HTTP.Media.Language\r\n    Network.HTTP.Media.Language.Gen\r\n    Network.HTTP.Media.Language.Internal\r\n    Network.HTTP.Media.Language.Tests\r\n    Network.HTTP.Media.MediaType\r\n    Network.HTTP.Media.MediaType.Gen\r\n    Network.HTTP.Media.MediaType.Internal\r\n    Network.HTTP.Media.MediaType.Tests\r\n    Network.HTTP.Media.Quality\r\n    Network.HTTP.Media.RenderHeader\r\n    Network.HTTP.Media.Tests\r\n    Network.HTTP.Media.Utils\r\n\r\n  build-depends:\r\n    base                       >= 4.7  && < 4.13,\r\n    bytestring                 >= 0.10 && < 0.12,\r\n    case-insensitive           >= 1.0  && < 1.3,\r\n    containers                 >= 0.5  && < 0.7,\r\n    utf8-string                >= 0.3  && < 1.1,\r\n    QuickCheck                 >= 2.8  && < 2.14,\r\n    test-framework             >= 0.8  && < 0.9,\r\n    test-framework-quickcheck2 >= 0.3  && < 0.4\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/zmthy/http-media\r\n";
    }