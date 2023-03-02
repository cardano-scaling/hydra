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
      specVersion = "1.12";
      identifier = { name = "genvalidity"; version = "1.1.0.0"; };
      license = "MIT";
      copyright = "Copyright: (c) 2016-2021 Tom Sydney Kerckhove";
      maintainer = "syd@cs-syd.eu";
      author = "Tom Sydney Kerckhove";
      homepage = "https://github.com/NorfairKing/validity#readme";
      url = "";
      synopsis = "Testing utilities for the validity library";
      description = "Note: There are companion instance packages for this library:\n\n* <https://hackage.haskell.org/package/genvalidity-aeson genvalidity-aeson>\n\n* <https://hackage.haskell.org/package/genvalidity-bytestring genvalidity-bytestring>\n\n* <https://hackage.haskell.org/package/genvalidity-containers genvalidity-containers>\n\n* <https://hackage.haskell.org/package/genvalidity-path genvalidity-path>\n\n* <https://hackage.haskell.org/package/genvalidity-scientific genvalidity-scientific>\n\n* <https://hackage.haskell.org/package/genvalidity-text genvalidity-text>\n\n* <https://hackage.haskell.org/package/genvalidity-time genvalidity-time>\n\n* <https://hackage.haskell.org/package/genvalidity-unordered-containers genvalidity-unordered-containers>\n\n* <https://hackage.haskell.org/package/genvalidity-uuid genvalidity-uuid>\n\n* <https://hackage.haskell.org/package/genvalidity-vector genvalidity-vector>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
          ];
        buildable = true;
        };
      tests = {
        "genvalidity-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/genvalidity-1.1.0.0.tar.gz";
      sha256 = "5897d7640a93ad6cd12177375009cad5189ba147edff80ded0c2a508ff5bbb23";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.7.\n--\n-- see: https://github.com/sol/hpack\n\nname:           genvalidity\nversion:        1.1.0.0\nsynopsis:       Testing utilities for the validity library\ndescription:    Note: There are companion instance packages for this library:\n                .\n                * <https://hackage.haskell.org/package/genvalidity-aeson genvalidity-aeson>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-bytestring genvalidity-bytestring>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-containers genvalidity-containers>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-path genvalidity-path>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-scientific genvalidity-scientific>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-text genvalidity-text>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-time genvalidity-time>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-unordered-containers genvalidity-unordered-containers>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-uuid genvalidity-uuid>\n                .\n                * <https://hackage.haskell.org/package/genvalidity-vector genvalidity-vector>\ncategory:       Testing\nhomepage:       https://github.com/NorfairKing/validity#readme\nbug-reports:    https://github.com/NorfairKing/validity/issues\nauthor:         Tom Sydney Kerckhove\nmaintainer:     syd@cs-syd.eu\ncopyright:      Copyright: (c) 2016-2021 Tom Sydney Kerckhove\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    LICENSE\n    CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/NorfairKing/validity\n\nlibrary\n  exposed-modules:\n      Data.GenValidity\n      Data.GenValidity.Utils\n  other-modules:\n      Paths_genvalidity\n  hs-source-dirs:\n      src\n  ghc-options: -Wno-redundant-constraints\n  build-depends:\n      QuickCheck >=2.13\n    , base >=4.13 && <5\n    , random >=1.1\n    , validity >=0.12\n  default-language: Haskell2010\n\ntest-suite genvalidity-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Data.GenValidity.GenericSpec\n      Data.GenValidity.ShrinkGenericSpec\n      Data.GenValiditySpec\n      Data.InstanceSpec\n      Paths_genvalidity\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall\n  build-depends:\n      QuickCheck\n    , base >=4.13 && <5\n    , genvalidity\n    , hspec\n    , hspec-core\n  default-language: Haskell2010\n";
    }