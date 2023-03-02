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
      identifier = { name = "typed-protocols-examples"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Examples and tests for the typed-protocols framework";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/typed-protocols-examples-0.1.0.0.tar.gz";
      sha256 = "24ba0ba16f39a6b6ea290f3ef7b4519b321aa7a613e27c0535f590dfe3a1351e";
      });
    }) // {
    package-description-override = "name:                typed-protocols-examples\nversion:             0.1.0.0\nsynopsis:            Examples and tests for the typed-protocols framework\n-- description:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Duncan Coutts, Marcin Szamotulski\nmaintainer:          alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io\ncategory:            Control\nbuild-type:          Simple\n\n-- These should probably be added at some point.\n-- extra-source-files:  ChangeLog.md, README.md\n\ncabal-version:       >=1.10\n\nlibrary\n  exposed-modules:   Network.TypedProtocol.Channel\n                   , Network.TypedProtocol.Driver.Simple\n\n                   , Network.TypedProtocol.PingPong.Type\n                   , Network.TypedProtocol.PingPong.Client\n                   , Network.TypedProtocol.PingPong.Server\n                   , Network.TypedProtocol.PingPong.Codec\n                   , Network.TypedProtocol.PingPong.Codec.CBOR\n                   , Network.TypedProtocol.PingPong.Examples\n\n                   , Network.TypedProtocol.ReqResp.Type\n                   , Network.TypedProtocol.ReqResp.Client\n                   , Network.TypedProtocol.ReqResp.Server\n                   , Network.TypedProtocol.ReqResp.Codec\n                   , Network.TypedProtocol.ReqResp.Codec.CBOR\n                   , Network.TypedProtocol.ReqResp.Examples\n\n  other-extensions:  GADTs\n                   , RankNTypes\n                   , PolyKinds\n                   , DataKinds\n                   , ScopedTypeVariables\n                   , TypeFamilies\n                   , TypeOperators\n                   , BangPatterns\n  build-depends:     base,\n                     bytestring,\n                     cborg,\n                     serialise,\n                     contra-tracer,\n                     io-classes,\n                     time,\n                     typed-protocols,\n                     typed-protocols-cborg\n\n  hs-source-dirs:    src\n  default-language:  Haskell2010\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n\ntest-suite test\n  type:              exitcode-stdio-1.0\n  main-is:           Main.hs\n  hs-source-dirs:    test\n  other-modules:     Network.TypedProtocol.PingPong.Tests\n                   , Network.TypedProtocol.ReqResp.Tests\n  build-depends:     base\n                   , bytestring\n                   , contra-tracer\n                   , typed-protocols\n                   , typed-protocols-cborg\n                   , typed-protocols-examples\n                   , io-classes\n                   , io-sim\n                   , QuickCheck\n                   , tasty\n                   , tasty-quickcheck\n  default-language:  Haskell2010\n  ghc-options:       -rtsopts\n                     -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wno-orphans\n";
    }