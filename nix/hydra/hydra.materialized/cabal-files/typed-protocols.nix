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
      identifier = { name = "typed-protocols"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "A framework for strongly typed protocols";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/typed-protocols-0.1.0.0.tar.gz";
      sha256 = "6330301923039fc986a518d1e1356cc9db59fe994b5a2d67828af971952de6f4";
      });
    }) // {
    package-description-override = "name:                typed-protocols\nversion:             0.1.0.0\nsynopsis:            A framework for strongly typed protocols\n-- description:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Duncan Coutts, Marcin Szamotulski\nmaintainer:          alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io\ncategory:            Control\nbuild-type:          Simple\n\n-- These should probably be added at some point.\n-- extra-source-files:  ChangeLog.md, README.md\n\ncabal-version:       >=1.10\n\nlibrary\n  exposed-modules:   Network.TypedProtocol\n                   , Network.TypedProtocol.Core\n                   , Network.TypedProtocol.Codec\n                   , Network.TypedProtocol.Pipelined\n                   , Network.TypedProtocol.Driver\n                   , Network.TypedProtocol.Proofs\n\n  other-extensions:  GADTs\n                   , RankNTypes\n                   , PolyKinds\n                   , DataKinds\n                   , ScopedTypeVariables\n                   , TypeFamilies\n                   , TypeOperators\n                   , BangPatterns\n  build-depends:     base,\n                     io-classes\n\n  hs-source-dirs:    src\n  default-language:  Haskell2010\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n";
    }