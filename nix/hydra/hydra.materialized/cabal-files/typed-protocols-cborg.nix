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
      identifier = { name = "typed-protocols-cborg"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "CBOR codecs for typed-protocols";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/typed-protocols-cborg-0.1.0.0.tar.gz";
      sha256 = "45b88828e0dbdeaa8f942a3519c59bf0a657d2830d1cfb085ad2a4305eabbdbc";
      });
    }) // {
    package-description-override = "name:                typed-protocols-cborg\nversion:             0.1.0.0\nsynopsis:            CBOR codecs for typed-protocols\n-- description:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019-2021 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Duncan Coutts, Marcin Szamotulski\nmaintainer:          alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io\ncategory:            Control\nbuild-type:          Simple\n\n-- These should probably be added at some point.\nextra-source-files:  ChangeLog.md, README.md\n\ncabal-version:       >=1.10\n\nlibrary\n  exposed-modules:   Network.TypedProtocol.Codec.CBOR\n\n  build-depends:     base            >=4.12  && <4.15,\n                     bytestring      >=0.10  && <0.11,\n                     cborg           >=0.2.1 && <0.3,\n\n                     io-classes,\n                     typed-protocols\n\n  hs-source-dirs:    src\n  default-language:  Haskell2010\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n";
    }