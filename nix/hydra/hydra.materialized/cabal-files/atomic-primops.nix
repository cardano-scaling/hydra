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
    flags = { debug = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "atomic-primops"; version = "0.8.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "rrnewton@gmail.com";
      author = "Ryan Newton";
      homepage = "https://github.com/rrnewton/haskell-lockfree/wiki";
      url = "";
      synopsis = "A safe approach to CAS and other atomic ops in Haskell.";
      description = "After GHC 7.4 a new `casMutVar#` primop became available, but it's\ndifficult to use safely, because pointer equality is a highly\nunstable property in Haskell.  This library provides a safer method\nbased on the concept of \"Tickets\".\n\nAlso, this library uses the \"foreign primop\" capability of GHC to\nadd access to other variants that may be of\ninterest, specifically, compare and swap inside an array.\n\nNote that as of GHC 7.8, the relevant primops have been included in GHC itself.\nThis library is engineered to work pre- and post-GHC-7.8, while exposing the\nsame interface.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/atomic-primops-0.8.4.tar.gz";
      sha256 = "22a8617eb9e221b5daee1ae26ccce279ce3d7a53d76e82c767708f90a6c72d3e";
      });
    }) // {
    package-description-override = "Name:                atomic-primops\nVersion:             0.8.4\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Ryan Newton\nMaintainer:          rrnewton@gmail.com\nCategory:            Data\n-- Portability:         non-portabile (x86_64)\nBuild-type:          Simple\nCabal-version:       >=1.18\ntested-with:         GHC == 8.4.3, GHC == 8.2.2, GHC == 8.0.2, GHC == 7.10.3\nHomePage: https://github.com/rrnewton/haskell-lockfree/wiki\nBug-Reports: https://github.com/rrnewton/haskell-lockfree/issues\n\nSynopsis: A safe approach to CAS and other atomic ops in Haskell.\n\nDescription:\n  After GHC 7.4 a new `casMutVar#` primop became available, but it's\n  difficult to use safely, because pointer equality is a highly\n  unstable property in Haskell.  This library provides a safer method\n  based on the concept of \"Tickets\".\n .\n  Also, this library uses the \"foreign primop\" capability of GHC to\n  add access to other variants that may be of\n  interest, specifically, compare and swap inside an array.\n .\n  Note that as of GHC 7.8, the relevant primops have been included in GHC itself.\n  This library is engineered to work pre- and post-GHC-7.8, while exposing the\n  same interface.\n\nExtra-Source-Files:  CHANGELOG.md, DEVLOG.md,\n                     testing/Test.hs, testing/test-atomic-primops.cabal, testing/ghci-test.hs\n                     testing/Makefile, testing/CommonTesting.hs, testing/Counter.hs, testing/CounterCommon.hs, testing/hello.hs, testing/Fetch.hs\n                     testing/Issue28.hs\n                     testing/TemplateHaskellSplices.hs\n                     testing/Raw781_test.hs\n\nFlag debug\n    Description: Enable extra internal checks.\n    Default: False\n\nLibrary\n  Default-Language: Haskell2010\n  exposed-modules:   Data.Atomics\n                     Data.Atomics.Internal\n                     Data.Atomics.Counter\n  ghc-options: -O2 -funbox-strict-fields\n  ghc-options: -Wall\n\n  build-depends:     base >= 4.8 && < 5\n                   , ghc-prim\n                   , primitive\n\n  if os(windows) {\n    Include-Dirs:     cbits\n    C-Sources:        cbits/RtsDup.c\n  }\n  CC-Options:       -Wall\n\n  if flag(debug)\n    cpp-options: -DDEBUG_ATOMICS\n\nSource-Repository head\n    Type:         git\n    Location:     https://github.com/rrnewton/haskell-lockfree/\n    Subdir:       atomic-primops\n";
    }