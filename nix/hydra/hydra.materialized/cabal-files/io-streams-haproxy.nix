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
      identifier = { name = "io-streams-haproxy"; version = "1.0.1.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2014 Google, Inc. and CONTRIBUTORS";
      maintainer = "greg@gregorycollins.net";
      author = "Gregory Collins";
      homepage = "http://snapframework.com/";
      url = "";
      synopsis = "HAProxy protocol 1.5 support for io-streams";
      description = "HAProxy protocol version 1.5 support (see\n<http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt>) for applications\nusing io-streams. The proxy protocol allows information about a networked\npeer (like remote address and port) to be propagated through a forwarding\nproxy that is configured to speak this protocol.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/io-streams-haproxy-1.0.1.0.tar.gz";
      sha256 = "b74eca9290fe838a0e3be857a38b62cf6fb7478acee400eac19e47471a2c96b5";
      });
    }) // {
    package-description-override = "name:                io-streams-haproxy\nversion:             1.0.1.0\nx-revision:          6\nsynopsis:            HAProxy protocol 1.5 support for io-streams\n\ndescription: HAProxy protocol version 1.5 support (see\n  <http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt>) for applications\n  using io-streams. The proxy protocol allows information about a networked\n  peer (like remote address and port) to be propagated through a forwarding\n  proxy that is configured to speak this protocol.\n\nhomepage:            http://snapframework.com/\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Gregory Collins\nmaintainer:          greg@gregorycollins.net\ncopyright:           (c) 2014 Google, Inc. and CONTRIBUTORS\ncategory:            Network, IO-Streams\nbuild-type:          Simple\nextra-source-files:\n  CONTRIBUTORS,\n  cbits/byteorder.c\n\ncabal-version:       >=1.10\nBug-Reports:         https://github.com/snapframework/io-streams-haproxy/issues\nTested-With:\n  GHC == 9.4.1\n  GHC == 9.2.4\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/snapframework/io-streams-haproxy.git\n\nlibrary\n  hs-source-dirs:    src\n  exposed-modules:   System.IO.Streams.Network.HAProxy\n  other-modules:     System.IO.Streams.Network.Internal.Address\n  c-sources:         cbits/byteorder.c\n\n  build-depends:     base              >= 4.5 && < 4.18,\n                     attoparsec        >= 0.7 && < 0.15,\n                     bytestring        >= 0.9 && < 0.12,\n                     io-streams        >= 1.3 && < 1.6,\n                     network           >= 2.3 && < 3.2,\n                     transformers      >= 0.3 && < 0.7\n  default-language:  Haskell2010\n\n  ghc-options:       -Wall -fwarn-tabs -funbox-strict-fields\n                     -fno-warn-unused-do-bind\n  if os(windows)\n    cpp-options:     -DWINDOWS\n    cc-options:      -DWINDOWS\n\ntest-suite testsuite\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    src test\n  Main-is:           TestSuite.hs\n  Default-language:  Haskell2010\n  c-sources:         cbits/byteorder.c\n\n  ghc-options:       -Wall -fwarn-tabs -funbox-strict-fields -threaded\n                     -fno-warn-unused-do-bind\n\n  Other-modules:     System.IO.Streams.Network.HAProxy,\n                     System.IO.Streams.Network.HAProxy.Tests,\n                     System.IO.Streams.Network.Internal.Address\n\n  build-depends:     base,\n                     attoparsec,\n                     bytestring,\n                     io-streams,\n                     network,\n                     transformers,\n                     ------------------------------\n                     HUnit                      >= 1.2      && <2,\n                     test-framework             >= 0.8.0.3  && <0.9,\n                     test-framework-hunit       >= 0.2.7    && <0.4\n  if os(windows)\n    cpp-options:     -DWINDOWS\n    cc-options:      -DWINDOWS\n";
    }