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
      identifier = { name = "base58-bytestring"; version = "0.1.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "s9gf4ult@gmail.com";
      author = "Philippe Laprade, Jean-Pierre Rupp";
      homepage = "https://bitbucket.org/s9gf4ult/base58-bytestring";
      url = "";
      synopsis = "Implementation of BASE58 transcoding for ByteStrings";
      description = "Implementation of BASE58 transcoding copy-pasted from\nhaskoin package";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."quickcheck-assertions" or (errorHandler.buildDepError "quickcheck-assertions"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base58-bytestring-0.1.0.tar.gz";
      sha256 = "c2dbf598f3415053e12cca84b90fa7c0c1b02f3b784cce0157264baebf2d40d3";
      });
    }) // {
    package-description-override = "name:                base58-bytestring\nversion:             0.1.0\nsynopsis:            Implementation of BASE58 transcoding for ByteStrings\n\ndescription: Implementation of BASE58 transcoding copy-pasted from\n             haskoin package\n\nhomepage: https://bitbucket.org/s9gf4ult/base58-bytestring\nlicense:             PublicDomain\nlicense-file:        UNLICENSE\nauthor:              Philippe Laprade, Jean-Pierre Rupp\nmaintainer:          s9gf4ult@gmail.com\ncategory:            Data, ByteStrings\nbuild-type:          Simple\ncabal-version:       >=1.10\n\nsource-repository head\n  type:     git\n  location: https://s9gf4ult@bitbucket.org/s9gf4ult/base58-bytestring.git\n\nlibrary\n  exposed-modules: Data.ByteString.Base58\n                 , Data.ByteString.Base58.Internal\n\n  default-extensions: OverloadedStrings\n                    , DeriveDataTypeable\n                    , DeriveGeneric\n                    , GeneralizedNewtypeDeriving\n\n  build-depends:       base >=4.6 && < 5\n                     , bytestring\n\n\n  hs-source-dirs:      src\n  ghc-options: -Wall\n  default-language:    Haskell2010\n\ntest-suite test\n  type:    exitcode-stdio-1.0\n  main-is: Main.hs\n\n  hs-source-dirs:  test\n  ghc-options:     -Wall -rtsopts -threaded\n  default-language:    Haskell2010\n\n  build-depends:       base >=4.6 && < 5\n                     , base58-bytestring\n                     , bytestring\n                     , quickcheck-assertions >= 0.2.0\n                     , quickcheck-instances\n                     , tasty\n                     , tasty-quickcheck\n\n\nbenchmark bench\n  type:    exitcode-stdio-1.0\n  main-is: Main.hs\n\n  default-extensions: OverloadedStrings\n\n  hs-source-dirs:  bench\n  ghc-options:     -Wall -rtsopts -threaded\n  default-language:    Haskell2010\n\n  build-depends:       base >=4.6 && < 5\n                     , base58-bytestring\n                     , bytestring\n                     , criterion";
    }