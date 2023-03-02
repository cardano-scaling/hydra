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
      identifier = { name = "canonical-json"; version = "0.6.0.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2015-2018 Well-Typed LLP";
      maintainer = "duncan@well-typed.com, edsko@well-typed.com";
      author = "Duncan Coutts, Edsko de Vries";
      homepage = "https://github.com/well-typed/canonical-json";
      url = "";
      synopsis = "Canonical JSON for signing and hashing JSON values";
      description = "An implementation of Canonical JSON.\n\n<http://wiki.laptop.org/go/Canonical_JSON>\n\nThe \\\"canonical JSON\\\" format is designed to provide\nrepeatable hashes of JSON-encoded data. It is designed\nfor applications that need to hash, sign or authenitcate\nJSON data structures, including embedded signatures.\n\nCanonical JSON is parsable with any full JSON parser, and\nit allows whitespace for pretty-printed human readable\npresentation, but it can be put into a canonical form\nwhich then has a stable serialised representation and\nthus a stable hash.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "parse-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/canonical-json-0.6.0.1.tar.gz";
      sha256 = "33df39d9058d33357956cdf7f911184a26da20c49b90f844ec6374f6bf5ace7e";
      });
    }) // {
    package-description-override = "name:                canonical-json\nversion:             0.6.0.1\nsynopsis:            Canonical JSON for signing and hashing JSON values\ndescription:         An implementation of Canonical JSON.\n                     .\n                     <http://wiki.laptop.org/go/Canonical_JSON>\n                     .\n                     The \\\"canonical JSON\\\" format is designed to provide\n                     repeatable hashes of JSON-encoded data. It is designed\n                     for applications that need to hash, sign or authenitcate\n                     JSON data structures, including embedded signatures.\n                     .\n                     Canonical JSON is parsable with any full JSON parser, and\n                     it allows whitespace for pretty-printed human readable\n                     presentation, but it can be put into a canonical form\n                     which then has a stable serialised representation and\n                     thus a stable hash.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Duncan Coutts, Edsko de Vries\nmaintainer:          duncan@well-typed.com, edsko@well-typed.com\ncopyright:           Copyright 2015-2018 Well-Typed LLP\nhomepage:            https://github.com/well-typed/canonical-json\ncategory:            Text, JSON\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\ncabal-version:       >=1.10\n\nextra-source-files:  README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/well-typed/canonical-json.git\n\nlibrary\n  exposed-modules:     Text.JSON.Canonical\n                       Text.JSON.Canonical.Class\n                       Text.JSON.Canonical.Parse\n                       Text.JSON.Canonical.Types\n  other-extensions:    CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable,\n                       MultiParamTypeClasses, FlexibleInstances,\n                       ScopedTypeVariables, OverlappingInstances\n  build-depends:       base              >= 4.5     && < 5,\n                       bytestring        >= 0.10.4  && < 0.12,\n                       containers        >= 0.4     && < 0.7,\n                       deepseq           >= 1.2     && < 1.5,\n                       parsec            >= 3.1     && < 3.2,\n                       pretty            >= 1.0     && < 1.2\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\ntest-suite tests\n  type:                exitcode-stdio-1.0\n  main-is:             TestSuite.hs\n  hs-source-dirs:      tests\n  build-depends:       base,\n                       bytestring,\n                       canonical-json,\n                       containers,\n                       aeson             >= 1.4     && < 2.2,\n                       vector,\n                       unordered-containers,\n                       QuickCheck        >= 2.11    && < 2.16,\n                       tasty,\n                       tasty-quickcheck\n  default-language:    Haskell2010\n  -- -K100k to check for stack overflow:\n  ghc-options:         -Wall -with-rtsopts=-K100k\n\nbenchmark parse-bench\n  type:                exitcode-stdio-1.0\n  main-is:             Parse.hs\n  hs-source-dirs:      benchmark\n  build-depends:       base,\n                       bytestring,\n                       canonical-json,\n                       containers,\n                       criterion >= 1.1\n  default-language:    Haskell2010\n  ghc-options:         -Wall -rtsopts\n";
    }