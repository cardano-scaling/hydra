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
    flags = { bytestring--lt-0_10_4 = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "cassava"; version = "0.5.3.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012 Johan Tibell\n(c) 2012 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "https://github.com/haskell-hvr/cassava";
      author = "Johan Tibell";
      homepage = "https://github.com/haskell-hvr/cassava";
      url = "";
      synopsis = "A CSV parsing and encoding library";
      description = "@cassava@ is a library for parsing and encoding [RFC 4180](https://tools.ietf.org/html/rfc4180)\ncompliant [comma-separated values (CSV)](https://en.wikipedia.org/wiki/Comma-separated_values) data,\nwhich is a textual line-oriented format commonly used for exchanging tabular data.\n\n@cassava@'s API includes support for\n\n- Index-based record-conversion\n- Name-based record-conversion\n- Typeclass directed conversion of fields and records\n- Built-in field-conversion instances for standard types\n- Customizable record-conversion instance derivation via GHC generics\n- Low-level [bytestring](https://hackage.haskell.org/package/bytestring) builders (see \"Data.Csv.Builder\")\n- Incremental decoding and encoding API (see \"Data.Csv.Incremental\")\n- Streaming API for constant-space decoding (see \"Data.Csv.Streaming\")\n\nMoreover, this library is designed to be easy to use; for instance, here's a\nvery simple example of encoding CSV data:\n\n>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]\n\"John,27\\r\\nJane,28\\r\\n\"\n\nPlease refer to the documentation in \"Data.Csv\" and the included [README](#readme) for more usage examples.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."Only" or (errorHandler.buildDepError "Only"))
          ] ++ (if flags.bytestring--lt-0_10_4
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."nats" or (errorHandler.buildDepError "nats"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "unit-tests" = {
          depends = (([
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."nats" or (errorHandler.buildDepError "nats"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cassava-0.5.3.0.tar.gz";
      sha256 = "b4c8451f433ad7725cb8b9f7a7efe598ba103b16584713c91f48ae023829e9be";
      });
    }) // {
    package-description-override = "cabal-version:       1.12\nName:                cassava\nVersion:             0.5.3.0\nSynopsis:            A CSV parsing and encoding library\nDescription: {\n\n@cassava@ is a library for parsing and encoding [RFC 4180](https://tools.ietf.org/html/rfc4180)\ncompliant [comma-separated values (CSV)](https://en.wikipedia.org/wiki/Comma-separated_values) data,\nwhich is a textual line-oriented format commonly used for exchanging tabular data.\n.\n@cassava@'s API includes support for\n.\n- Index-based record-conversion\n- Name-based record-conversion\n- Typeclass directed conversion of fields and records\n- Built-in field-conversion instances for standard types\n- Customizable record-conversion instance derivation via GHC generics\n- Low-level [bytestring](https://hackage.haskell.org/package/bytestring) builders (see \"Data.Csv.Builder\")\n- Incremental decoding and encoding API (see \"Data.Csv.Incremental\")\n- Streaming API for constant-space decoding (see \"Data.Csv.Streaming\")\n.\nMoreover, this library is designed to be easy to use; for instance, here's a\nvery simple example of encoding CSV data:\n.\n>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]\n\"John,27\\r\\nJane,28\\r\\n\"\n.\nPlease refer to the documentation in \"Data.Csv\" and the included [README](#readme) for more usage examples.\n\n}\nHomepage:            https://github.com/haskell-hvr/cassava\nLicense:             BSD3\nLicense-file:        LICENSE\nBug-reports:         https://github.com/haskell-hvr/cassava/issues\nCopyright:           (c) 2012 Johan Tibell\n                     (c) 2012 Bryan O'Sullivan\n                     (c) 2011 MailRank, Inc.\nAuthor:              Johan Tibell\nMaintainer:          https://github.com/haskell-hvr/cassava\nCategory:            Text, Web, CSV\nBuild-type:          Simple\nExtra-source-files:  examples/*.hs,\n                     CHANGES.md,\n                     README.md\nTested-with:\n  GHC == 9.4.1\n  GHC == 9.2.3\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n\n----------------------------------------------------------------------------\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-hvr/cassava.git\n\nflag bytestring--LT-0_10_4\n  description: [bytestring](https://hackage.haskell.org/haskell/package/bytestring) < 0.10.4\n  default: False\n  manual:  False\n\nLibrary\n  default-language: Haskell2010\n  other-extensions:\n    BangPatterns\n    CPP\n    DataKinds\n    DefaultSignatures\n    DeriveFunctor\n    FlexibleContexts\n    FlexibleInstances\n    KindSignatures\n    MultiParamTypeClasses\n    OverloadedStrings\n    PolyKinds\n    Rank2Types\n    ScopedTypeVariables\n    TypeOperators\n    UndecidableInstances\n\n  if impl(ghc >= 8.0)\n    other-extensions:\n      DataKinds\n      PolyKinds\n\n  hs-source-dirs: src\n\n  Exposed-modules:\n    Data.Csv\n    Data.Csv.Builder\n    Data.Csv.Incremental\n    Data.Csv.Parser\n    Data.Csv.Streaming\n\n  Other-modules:\n    Data.Csv.Conversion\n    Data.Csv.Conversion.Internal\n    Data.Csv.Encoding\n    Data.Csv.Types\n    Data.Csv.Util\n\n  Build-depends:\n      base         >= 4.5      && < 4.18\n    , array        >= 0.4      && < 0.6\n    , attoparsec   >= 0.11.3.0 && < 0.15\n    , bytestring   >= 0.9.2    && < 0.12\n    , containers   >= 0.4.2    && < 0.7\n    , deepseq      >= 1.1      && < 1.5\n    , hashable                    < 1.5\n    , scientific   >= 0.3.4.7  && < 0.4\n    , text                        < 2.1\n    , transformers >= 0.2      && < 0.7\n    , unordered-containers        < 0.3\n    , vector       >= 0.8      && < 0.14\n    , Only         >= 0.1      && < 0.1.1\n\n  if flag(bytestring--LT-0_10_4)\n    build-depends: bytestring <  0.10.4\n                 , bytestring-builder >= 0.10.8 && < 0.11\n  else\n    build-depends: bytestring >= 0.10.4\n                 , text-short == 0.1.*\n\n  -- GHC.Generics lived in `ghc-prim` for GHC 7.2 & GHC 7.4 only\n  if impl(ghc < 7.6)\n    build-depends: ghc-prim == 0.2.*\n\n  -- For Numeric.Natural\n  if impl(ghc < 7.10)\n    build-depends: nats >= 1 && < 1.2\n\n  -- https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#Recommendationsforforward-compatibility\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n    if impl(ghc >= 8.8)\n      ghc-options: -Wno-star-is-type\n    else\n      ghc-options: -Wnoncanonical-monadfail-instances\n  else\n    -- provide/emulate `Control.Monad.Fail` and `Data.Semigroups` API for pre-GHC8\n    build-depends: fail == 4.9.*, semigroups >= 0.18.2 && <0.20\n\n  if impl(ghc >= 8.2)\n    ghc-options: -Wcpp-undef\n\n  ghc-options: -Wall\n\n----------------------------------------------------------------------------\n\nTest-suite unit-tests\n  default-language: Haskell2010\n\n  Type: exitcode-stdio-1.0\n  Main-is: UnitTests.hs\n  -- dependencies with version constraints inherited via lib:cassava\n  Build-depends: attoparsec\n               , base\n               , bytestring\n               , cassava\n               , hashable\n               , scientific\n               , text\n               , unordered-containers\n               , vector\n  -- extra dependencies not already used by lib:cassava\n  build-depends: HUnit < 1.7\n               , QuickCheck == 2.14.*\n               , quickcheck-instances >= 0.3.12 && < 0.4\n               , test-framework == 0.8.*\n               , test-framework-hunit == 0.3.*\n               , test-framework-quickcheck2 == 0.3.*\n\n  hs-source-dirs: tests\n\n  -- GHC.Generics lived in `ghc-prim` for GHC 7.2 & GHC 7.4 only\n  if impl(ghc < 7.6)\n    build-depends: ghc-prim == 0.2.*\n\n  -- For Numeric.Natural\n  if impl(ghc < 7.10)\n    build-depends: nats\n\n  -- https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#Recommendationsforforward-compatibility\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n    if impl(ghc < 8.8)\n      ghc-options:  -Wnoncanonical-monadfail-instances\n  else\n    -- provide/emulate `Control.Monad.Fail` and `Data.Semigroups` API for pre-GHC8\n    build-depends: fail, semigroups\n\n  if impl(ghc >= 8.2)\n    ghc-options: -Wcpp-undef\n\n  ghc-options: -Wall\n";
    }