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
    flags = { example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "websockets"; version = "0.12.7.3"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010-2011 Siniša Biđin\n(c) 2011-2018 Jasper Van der Jeugt\n(c) 2011 Steffen Schuldenzucker\n(c) 2011 Alex Lang";
      maintainer = "Jasper Van der Jeugt <m@jaspervdj.be>";
      author = "Siniša Biđin <sinisa@bidin.cc>\nJasper Van der Jeugt <m@jaspervdj.be>\nSteffen Schuldenzucker <steffen.schuldenzucker@googlemail.com>\nAlex Lang <lang@tsurucapital.com>";
      homepage = "http://jaspervdj.be/websockets";
      url = "";
      synopsis = "A sensible and clean way to write WebSocket-capable servers in Haskell.";
      description = "This library allows you to write WebSocket-capable servers.\n\nAn example server:\n<https://github.com/jaspervdj/websockets/blob/master/example/server.lhs>\n\nAn example client:\n<https://github.com/jaspervdj/websockets/blob/master/example/client.hs>\n\nSee also:\n\n* The specification of the WebSocket protocol:\n<http://www.whatwg.org/specs/web-socket-protocol/>\n\n* The JavaScript API for dealing with WebSockets:\n<http://www.w3.org/TR/websockets/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
          ];
        buildable = true;
        };
      exes = {
        "websockets-example" = {
          depends = [
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = if !flags.example then false else true;
          };
        "websockets-autobahn" = {
          depends = [
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      tests = {
        "websockets-tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-mask" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/websockets-0.12.7.3.tar.gz";
      sha256 = "d3b0a8a0df7ac7c39c2572152ff903af2d5f009627dde4dada6fb81c89057f3c";
      });
    }) // {
    package-description-override = "Name:    websockets\r\nVersion: 0.12.7.3\r\nX-revision: 1\r\n\r\nSynopsis:\r\n  A sensible and clean way to write WebSocket-capable servers in Haskell.\r\n\r\nDescription:\r\n This library allows you to write WebSocket-capable servers.\r\n .\r\n An example server:\r\n <https://github.com/jaspervdj/websockets/blob/master/example/server.lhs>\r\n .\r\n An example client:\r\n <https://github.com/jaspervdj/websockets/blob/master/example/client.hs>\r\n .\r\n See also:\r\n .\r\n * The specification of the WebSocket protocol:\r\n <http://www.whatwg.org/specs/web-socket-protocol/>\r\n .\r\n * The JavaScript API for dealing with WebSockets:\r\n <http://www.w3.org/TR/websockets/>\r\n\r\nLicense:       BSD3\r\nLicense-file:  LICENCE\r\nCopyright:     (c) 2010-2011 Siniša Biđin\r\n               (c) 2011-2018 Jasper Van der Jeugt\r\n               (c) 2011 Steffen Schuldenzucker\r\n               (c) 2011 Alex Lang\r\nAuthor:        Siniša Biđin <sinisa@bidin.cc>\r\n               Jasper Van der Jeugt <m@jaspervdj.be>\r\n               Steffen Schuldenzucker <steffen.schuldenzucker@googlemail.com>\r\n               Alex Lang <lang@tsurucapital.com>\r\nMaintainer:    Jasper Van der Jeugt <m@jaspervdj.be>\r\nStability:     experimental\r\nCategory:      Network\r\nBuild-type:    Simple\r\nCabal-version: >= 1.10\r\n\r\nHomepage:    http://jaspervdj.be/websockets\r\nBug-reports: https://github.com/jaspervdj/websockets/issues\r\n\r\nExtra-source-files:\r\n  CHANGELOG\r\n\r\nSource-repository head\r\n  Type:     git\r\n  Location: https://github.com/jaspervdj/websockets\r\n\r\nFlag Example\r\n  Description: Build the example server\r\n  Default:     False\r\n  Manual:      True\r\n\r\nLibrary\r\n  Hs-source-dirs: src\r\n  Ghc-options:      -Wall\r\n  C-sources:        cbits/cbits.c\r\n  Default-language: Haskell2010\r\n\r\n  Exposed-modules:\r\n    Network.WebSockets\r\n    Network.WebSockets.Client\r\n    Network.WebSockets.Connection\r\n    Network.WebSockets.Extensions\r\n    Network.WebSockets.Stream\r\n    -- Network.WebSockets.Util.PubSub TODO\r\n\r\n  Other-modules:\r\n    Network.WebSockets.Connection.Options\r\n    Network.WebSockets.Extensions.Description\r\n    Network.WebSockets.Extensions.PermessageDeflate\r\n    Network.WebSockets.Extensions.StrictUnicode\r\n    Network.WebSockets.Http\r\n    Network.WebSockets.Hybi13\r\n    Network.WebSockets.Hybi13.Demultiplex\r\n    Network.WebSockets.Hybi13.Mask\r\n    Network.WebSockets.Protocol\r\n    Network.WebSockets.Server\r\n    Network.WebSockets.Types\r\n\r\n  Build-depends:\r\n    async             >= 2.2    && < 2.3,\r\n    attoparsec        >= 0.10   && < 0.15,\r\n    base              >= 4.8    && < 5,\r\n    base64-bytestring >= 0.1    && < 1.3,\r\n    binary            >= 0.8.1  && < 0.11,\r\n    bytestring        >= 0.9    && < 0.12,\r\n    bytestring-builder             < 0.11,\r\n    case-insensitive  >= 0.3    && < 1.3,\r\n    clock             >= 0.8    && < 0.9,\r\n    containers        >= 0.3    && < 0.7,\r\n    network           >= 2.3    && < 3.2,\r\n    random            >= 1.0    && < 1.3,\r\n    SHA               >= 1.5    && < 1.7,\r\n    streaming-commons >= 0.1    && < 0.3,\r\n    text              >= 0.10   && < 2.1,\r\n    entropy           >= 0.2.1  && < 0.5\r\n\r\nTest-suite websockets-tests\r\n  Type:             exitcode-stdio-1.0\r\n  Hs-source-dirs:   src tests/haskell\r\n  Main-is:          TestSuite.hs\r\n  Ghc-options:      -Wall\r\n  C-sources:        cbits/cbits.c\r\n  Default-language: Haskell2010\r\n\r\n  Other-modules:\r\n    Network.WebSockets\r\n    Network.WebSockets.Client\r\n    Network.WebSockets.Connection\r\n    Network.WebSockets.Connection.Options\r\n    Network.WebSockets.Extensions\r\n    Network.WebSockets.Extensions.Description\r\n    Network.WebSockets.Extensions.PermessageDeflate\r\n    Network.WebSockets.Extensions.PermessageDeflate.Tests\r\n    Network.WebSockets.Extensions.StrictUnicode\r\n    Network.WebSockets.Extensions.Tests\r\n    Network.WebSockets.Handshake.Tests\r\n    Network.WebSockets.Http\r\n    Network.WebSockets.Http.Tests\r\n    Network.WebSockets.Hybi13\r\n    Network.WebSockets.Hybi13.Demultiplex\r\n    Network.WebSockets.Hybi13.Demultiplex.Tests\r\n    Network.WebSockets.Hybi13.Mask\r\n    Network.WebSockets.Mask.Tests\r\n    Network.WebSockets.Protocol\r\n    Network.WebSockets.Server\r\n    Network.WebSockets.Server.Tests\r\n    Network.WebSockets.Stream\r\n    Network.WebSockets.Tests\r\n    Network.WebSockets.Tests.Util\r\n    Network.WebSockets.Types\r\n    Paths_websockets\r\n\r\n  Build-depends:\r\n    HUnit                      >= 1.2 && < 1.7,\r\n    QuickCheck                 >= 2.7 && < 2.15,\r\n    test-framework             >= 0.4 && < 0.9,\r\n    test-framework-hunit       >= 0.2 && < 0.4,\r\n    test-framework-quickcheck2 >= 0.2 && < 0.4,\r\n    -- Copied from regular dependencies...\r\n    async             >= 2.2    && < 2.3,\r\n    attoparsec        >= 0.10   && < 0.15,\r\n    base              >= 4      && < 5,\r\n    base64-bytestring >= 0.1    && < 1.3,\r\n    binary            >= 0.8.1  && < 0.11,\r\n    bytestring        >= 0.9    && < 0.12,\r\n    bytestring-builder             < 0.11,\r\n    case-insensitive  >= 0.3    && < 1.3,\r\n    clock             >= 0.8    && < 0.9,\r\n    containers        >= 0.3    && < 0.7,\r\n    network           >= 2.3    && < 3.2,\r\n    random            >= 1.0    && < 1.3,\r\n    SHA               >= 1.5    && < 1.7,\r\n    streaming-commons >= 0.1    && < 0.3,\r\n    text              >= 0.10   && < 2.1,\r\n    entropy           >= 0.2.1  && < 0.5\r\n\r\nExecutable websockets-example\r\n  If !flag(Example)\r\n    Buildable: False\r\n\r\n  Hs-source-dirs:   example\r\n  Main-is:          server.lhs\r\n  Ghc-options:      -Wall\r\n  Default-language: Haskell2010\r\n\r\n  Build-depends:\r\n    websockets,\r\n    -- Copied from regular dependencies...\r\n    async             >= 2.2    && < 2.3,\r\n    attoparsec        >= 0.10   && < 0.15,\r\n    base              >= 4      && < 5,\r\n    base64-bytestring >= 0.1    && < 1.3,\r\n    binary            >= 0.8.1  && < 0.11,\r\n    bytestring        >= 0.9    && < 0.12,\r\n    bytestring-builder             < 0.11,\r\n    case-insensitive  >= 0.3    && < 1.3,\r\n    clock             >= 0.8    && < 0.9,\r\n    containers        >= 0.3    && < 0.7,\r\n    network           >= 2.3    && < 3.2,\r\n    random            >= 1.0    && < 1.3,\r\n    SHA               >= 1.5    && < 1.7,\r\n    text              >= 0.10   && < 2.1,\r\n    entropy           >= 0.2.1  && < 0.5\r\n\r\nExecutable websockets-autobahn\r\n  If !flag(Example)\r\n    Buildable: False\r\n\r\n  Hs-source-dirs:   tests/autobahn\r\n  Main-is:          server.hs\r\n  Ghc-options:      -Wall -threaded -O2 -rtsopts \"-with-rtsopts=-N\"\r\n  Default-language: Haskell2010\r\n\r\n  Other-modules:\r\n    Paths_websockets\r\n\r\n  Build-depends:\r\n    websockets,\r\n    -- Copied from regular dependencies...\r\n    async             >= 2.2    && < 2.3,\r\n    attoparsec        >= 0.10   && < 0.15,\r\n    base              >= 4      && < 5,\r\n    base64-bytestring >= 0.1    && < 1.3,\r\n    binary            >= 0.8.1  && < 0.11,\r\n    bytestring        >= 0.9    && < 0.12,\r\n    bytestring-builder             < 0.11,\r\n    case-insensitive  >= 0.3    && < 1.3,\r\n    clock             >= 0.8    && < 0.9,\r\n    containers        >= 0.3    && < 0.7,\r\n    network           >= 2.3    && < 3.2,\r\n    random            >= 1.0    && < 1.3,\r\n    SHA               >= 1.5    && < 1.7,\r\n    text              >= 0.10   && < 2.1,\r\n    entropy           >= 0.2.1  && < 0.5\r\n\r\nBenchmark bench-mask\r\n  Type:             exitcode-stdio-1.0\r\n  Main-is:          mask.hs\r\n  C-sources:        cbits/cbits.c\r\n  Hs-source-dirs:   benchmarks, src\r\n  Default-language: Haskell2010\r\n\r\n  Other-modules:\r\n    Network.WebSockets.Hybi13.Mask\r\n\r\n  Build-depends:\r\n    criterion,\r\n    -- Copied from regular dependencies...\r\n    async             >= 2.2    && < 2.3,\r\n    attoparsec        >= 0.10   && < 0.15,\r\n    base              >= 4      && < 5,\r\n    base64-bytestring >= 0.1    && < 1.3,\r\n    binary            >= 0.8.1  && < 0.11,\r\n    bytestring        >= 0.9    && < 0.12,\r\n    bytestring-builder             < 0.11,\r\n    case-insensitive  >= 0.3    && < 1.3,\r\n    clock             >= 0.8    && < 0.9,\r\n    containers        >= 0.3    && < 0.7,\r\n    network           >= 2.3    && < 3.2,\r\n    random            >= 1.0    && < 1.3,\r\n    SHA               >= 1.5    && < 1.7,\r\n    text              >= 0.10   && < 2.1,\r\n    entropy           >= 0.2.1  && < 0.5\r\n";
    }