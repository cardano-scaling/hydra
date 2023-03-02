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
      identifier = { name = "dns"; version = "3.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "DNS library in Haskell";
      description = "A thread-safe DNS library for both clients and servers written\nin pure Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."split" or (errorHandler.buildDepError "split"));
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."iphlpapi" or (errorHandler.sysDepError "iphlpapi"));
        buildable = true;
        };
      tests = {
        "network" = {
          depends = [
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = true;
          };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dns-3.0.4.tar.gz";
      sha256 = "7b3433b536b7d225914d7b8495c7af1927d9554538d7d86c2644ccf9d3fa44a9";
      });
    }) // {
    package-description-override = "Name:                   dns\nVersion:                3.0.4\nx-revision: 1\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               DNS library in Haskell\nDescription:\n  A thread-safe DNS library for both clients and servers written\n  in pure Haskell.\nCategory:               Network\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\nExtra-Source-Files:     Changelog.md\n                        cbits/dns.c\n\nLibrary\n  Other-Extensions:     PatternSynonyms\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Network.DNS\n                        Network.DNS.Lookup\n                        Network.DNS.LookupRaw\n                        Network.DNS.Resolver\n                        Network.DNS.Utils\n                        Network.DNS.Types\n                        Network.DNS.Encode\n                        Network.DNS.Decode\n                        Network.DNS.IO\n  Other-Modules:        Network.DNS.Decode.Internal\n                        Network.DNS.Imports\n                        Network.DNS.Memo\n                        Network.DNS.StateBinary\n                        Network.DNS.Transport\n                        Network.DNS.Types.Internal\n  if impl(ghc < 8)\n    Build-Depends:      semigroups\n  Build-Depends:        base >= 4 && < 5\n                      , async\n                      , auto-update\n                      , attoparsec\n                      , base64-bytestring\n                      , binary\n                      , bytestring\n                      , containers\n                      , cryptonite\n                      , iproute >= 1.3.2\n                      , mtl\n                      , network >= 2.3\n                      , psqueues\n                      , safe == 0.3.*\n                      , time\n  if os(windows)\n    Build-Depends:    split\n    C-Sources:        cbits/dns.c\n    Extra-Libraries:  iphlpapi\n\nTest-Suite network\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test2\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        LookupSpec\n                        IOSpec\n  Build-Depends:        dns\n                      , base\n                      , bytestring\n                      , hspec\n                      , network\n\nTest-Suite spec\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        EncodeSpec\n                        DecodeSpec\n                        RoundTripSpec\n  Build-Depends:        dns\n                      , QuickCheck >= 2.9\n                      , base\n                      , bytestring\n                      , hspec\n                      , iproute >= 1.2.4\n                      , word8\n\nTest-Suite doctest\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test2\n  Ghc-Options:          -Wall\n  Main-Is:              doctests.hs\n  Build-Depends:        base\n                      , doctest\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/dns.git\n";
    }