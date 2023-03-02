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
      specVersion = "2.0";
      identifier = { name = "secp256k1-haskell"; version = "0.6.1"; };
      license = "MIT";
      copyright = "(c) 2017 Jean-Pierre Rupp";
      maintainer = "jprupp@protonmail.ch";
      author = "Jean-Pierre Rupp";
      homepage = "http://github.com/haskoin/secp256k1-haskell#readme";
      url = "";
      synopsis = "Bindings for secp256k1";
      description = "Sign and verify signatures using the secp256k1 library.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsecp256k1" or (errorHandler.pkgConfDepError "libsecp256k1"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."monad-par" or (errorHandler.buildDepError "monad-par"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."secp256k1-haskell" or (errorHandler.buildDepError "secp256k1-haskell"))
            (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
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
      url = "http://hackage.haskell.org/package/secp256k1-haskell-0.6.1.tar.gz";
      sha256 = "e664e85e650af13ba90a58e4cc964b6c0b05ab54f7957b26b37a67a269e57d29";
      });
    }) // {
    package-description-override = "cabal-version: 2.0\n\n-- This file has been generated from package.yaml by hpack version 0.34.4.\n--\n-- see: https://github.com/sol/hpack\n\nname:           secp256k1-haskell\nversion:        0.6.1\nsynopsis:       Bindings for secp256k1\ndescription:    Sign and verify signatures using the secp256k1 library.\ncategory:       Crypto\nhomepage:       http://github.com/haskoin/secp256k1-haskell#readme\nbug-reports:    https://github.com/haskoin/secp256k1-haskell.git/issues\nauthor:         Jean-Pierre Rupp\nmaintainer:     jprupp@protonmail.ch\ncopyright:      (c) 2017 Jean-Pierre Rupp\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    CHANGELOG.md\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/haskoin/secp256k1-haskell.git\n\nlibrary\n  exposed-modules:\n      Crypto.Secp256k1\n      Crypto.Secp256k1.Internal\n      Paths_secp256k1_haskell\n  autogen-modules:\n      Paths_secp256k1_haskell\n  hs-source-dirs:\n      src\n  pkgconfig-depends:\n      libsecp256k1\n  build-depends:\n      QuickCheck >=2.9.2 && <2.15\n    , base >=4.9 && <5\n    , base16 >=0.3.0.1\n    , bytestring >=0.10.8 && <0.12\n    , cereal >=0.5.4 && <0.6\n    , deepseq >=1.4.2 && <1.5\n    , entropy >=0.3.8 && <0.5\n    , hashable >=1.2.6 && <1.5\n    , string-conversions ==0.4.*\n    , unliftio-core >=0.1.0 && <0.3\n  default-language: Haskell2010\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Crypto.Secp256k1.InternalSpec\n      Crypto.Secp256k1Spec\n      Paths_secp256k1_haskell\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      HUnit\n    , QuickCheck >=2.9.2 && <2.15\n    , base >=4.9 && <5\n    , base16 >=0.3.0.1\n    , bytestring >=0.10.8 && <0.12\n    , cereal >=0.5.4 && <0.6\n    , deepseq >=1.4.2 && <1.5\n    , entropy >=0.3.8 && <0.5\n    , hashable >=1.2.6 && <1.5\n    , hspec\n    , monad-par\n    , mtl\n    , secp256k1-haskell\n    , string-conversions ==0.4.*\n    , unliftio-core >=0.1.0 && <0.3\n  default-language: Haskell2010\n  build-tool-depends: hspec-discover:hspec-discover\n";
    }