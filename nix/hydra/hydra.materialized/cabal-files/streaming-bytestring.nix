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
      identifier = { name = "streaming-bytestring"; version = "0.2.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "andrew.thaddeus@gmail.com, what_is_it_to_do_anything@yahoo.com, colin@fosskers.ca";
      author = "michaelt";
      homepage = "https://github.com/haskell-streaming/streaming-bytestring";
      url = "";
      synopsis = "Fast, effectful byte streams.";
      description = "This library enables fast and safe streaming of byte data, in either @Word8@ or\n@Char@ form. It is a core addition to the <https://github.com/haskell-streaming streaming ecosystem>\nand avoids the usual pitfalls of combinbing lazy @ByteString@s with lazy @IO@.\n\nWe follow the philosophy shared by @streaming@ that \"the best API is the one\nyou already know\". Thus this library mirrors the API of the @bytestring@\nlibrary as closely as possible.\n\nSee the module documentation and the README for more information.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ] ++ (if compiler.isGhc && (compiler.version).lt "7.8"
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/streaming-bytestring-0.2.4.tar.gz";
      sha256 = "b0dfd86f8ee711dbbd7b51d04531cf2e1c79c6dc05f23bb3902b35175481e447";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               streaming-bytestring\nversion:            0.2.4\nsynopsis:           Fast, effectful byte streams.\ndescription:\n  This library enables fast and safe streaming of byte data, in either @Word8@ or\n  @Char@ form. It is a core addition to the <https://github.com/haskell-streaming streaming ecosystem>\n  and avoids the usual pitfalls of combinbing lazy @ByteString@s with lazy @IO@.\n  .\n  We follow the philosophy shared by @streaming@ that \"the best API is the one\n  you already know\". Thus this library mirrors the API of the @bytestring@\n  library as closely as possible.\n  .\n  See the module documentation and the README for more information.\n\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             michaelt\nmaintainer:\n  andrew.thaddeus@gmail.com, what_is_it_to_do_anything@yahoo.com, colin@fosskers.ca\n\n-- copyright:\ncategory:           Data, Pipes, Streaming\nbuild-type:         Simple\nextra-source-files:\n  README.md\n  CHANGELOG.md\n  tests/sample.txt\n  tests/groupBy.txt\n\nstability:          Experimental\nhomepage:           https://github.com/haskell-streaming/streaming-bytestring\nbug-reports:\n  https://github.com/haskell-streaming/streaming-bytestring/issues\n\ntested-with:\n  GHC ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.3\n   || ==9.0.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-streaming/streaming-bytestring\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   lib\n  ghc-options:      -Wall -O2\n  exposed-modules:\n    Data.ByteString.Streaming\n    Data.ByteString.Streaming.Char8\n    Data.ByteString.Streaming.Internal\n    Streaming.ByteString\n    Streaming.ByteString.Char8\n    Streaming.ByteString.Internal\n\n  -- other-modules:\n  other-extensions:\n    BangPatterns\n    CPP\n    DeriveDataTypeable\n    ForeignFunctionInterface\n    Unsafe\n\n  build-depends:\n      base               >=4.8     && <5.0\n    , bytestring\n    , deepseq\n    , exceptions\n    , ghc-prim           >=0.4     && <0.9\n    , mmorph             >=1.0     && <1.3\n    , mtl                >=2.1     && <2.3\n    , resourcet\n    , streaming          >=0.1.4.0 && <0.3\n    , transformers       >=0.3     && <0.6\n    , transformers-base\n\n  if impl(ghc <7.8)\n    build-depends:\n        bytestring          >=0 && <0.10.4.0\n      , bytestring-builder\n\n  else\n    if impl(ghc <8.0)\n      build-depends: bytestring >=0.10.4 && <0.11\n\n    else\n      build-depends: bytestring >=0.10.4 && <0.12\n\n  if impl(ghc <8.0)\n    build-depends: semigroups\n\ntest-suite test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Test.hs\n  build-depends:\n      base                  >=4        && <5\n    , bytestring\n    , resourcet             >=1.1\n    , smallcheck            >=1.1.1\n    , streaming\n    , streaming-bytestring\n    , tasty                 >=0.11.0.4\n    , tasty-hunit           >=0.9\n    , tasty-smallcheck      >=0.8.1\n    , transformers\n";
    }