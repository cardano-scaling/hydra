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
      identifier = { name = "recv"; version = "0.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kazu@iij.ad.jp";
      author = "Kazu Yamamoto";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "Efficient netowrk recv";
      description = "Network recv based on buffer pools";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/recv-0.0.0.tar.gz";
      sha256 = "5c6872652e1025a701a73d9e97b02796f9433c42e2eec6d7e2acfb544b5ae9fb";
      });
    }) // {
    package-description-override = "Name:                recv\nVersion:             0.0.0\nSynopsis:            Efficient netowrk recv\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Kazu Yamamoto\nMaintainer:          kazu@iij.ad.jp\nHomepage:            http://github.com/yesodweb/wai\nCategory:            Network\nBuild-Type:          Simple\nCabal-Version:       >= 1.10\nStability:           Stable\ndescription:         Network recv based on buffer pools\n\nLibrary\n  Build-Depends:     base                      >= 4.12       && < 5\n                   , bytestring                >= 0.9.1.4\n                   , network                   >= 3.1.0\n  Exposed-modules:   Network.Socket.BufferPool\n  Other-modules:     Network.Socket.BufferPool.Buffer\n                     Network.Socket.BufferPool.Recv\n                     Network.Socket.BufferPool.Types\n                     Network.Socket.BufferPool.Windows\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n  Ghc-Options:       -Wall\n  Default-Language:  Haskell2010\n\nTest-Suite spec\n  Main-Is:           Spec.hs\n  Other-modules:     BufferPoolSpec\n                     Network.Socket.BufferPool\n                     Network.Socket.BufferPool.Buffer\n                     Network.Socket.BufferPool.Recv\n                     Network.Socket.BufferPool.Types\n                     Network.Socket.BufferPool.Windows\n  Hs-Source-Dirs:    test, .\n  Type:              exitcode-stdio-1.0\n  Build-Depends:     base                      >= 4.12       && < 5\n                   , bytestring                >= 0.9.1.4\n                   , network                   >= 3.1.0\n                   , hspec\n  Ghc-Options:       -Wall\n  Default-Language:  Haskell2010\n  Build-Tool-Depends: hspec-discover:hspec-discover\n";
    }