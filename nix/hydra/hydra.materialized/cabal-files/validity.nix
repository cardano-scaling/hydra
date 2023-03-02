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
      identifier = { name = "validity"; version = "0.12.0.1"; };
      license = "MIT";
      copyright = "Copyright: (c) 2016-2021 Tom Sydney Kerckhove";
      maintainer = "syd@cs-syd.eu";
      author = "Tom Sydney Kerckhove";
      homepage = "https://github.com/NorfairKing/validity#readme";
      url = "";
      synopsis = "Validity typeclass";
      description = "For more info, see <https://github.com/NorfairKing/validity the readme>.\n\nNote: There are companion instance packages for this library:\n\n* <https://hackage.haskell.org/package/validity-aeson validity-aeson>\n\n* <https://hackage.haskell.org/package/validity-bytestring validity-bytestring>\n\n* <https://hackage.haskell.org/package/validity-containers validity-containers>\n\n* <https://hackage.haskell.org/package/validity-path validity-path>\n\n* <https://hackage.haskell.org/package/validity-scientific validity-scientific>\n\n* <https://hackage.haskell.org/package/validity-text validity-text>\n\n* <https://hackage.haskell.org/package/validity-time validity-time>\n\n* <https://hackage.haskell.org/package/validity-unordered-containers validity-unordered-containers>\n\n* <https://hackage.haskell.org/package/validity-uuid validity-uuid>\n\n* <https://hackage.haskell.org/package/validity-vector validity-vector>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "validity-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/validity-0.12.0.1.tar.gz";
      sha256 = "1c3ce2052b73a47bd6ea6cb757e0dac5cf1cacf3558cb06b5ab725ad30d73ec9";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.7.\n--\n-- see: https://github.com/sol/hpack\n\nname:           validity\nversion:        0.12.0.1\nsynopsis:       Validity typeclass\ndescription:    For more info, see <https://github.com/NorfairKing/validity the readme>.\n                .\n                Note: There are companion instance packages for this library:\n                .\n                * <https://hackage.haskell.org/package/validity-aeson validity-aeson>\n                .\n                * <https://hackage.haskell.org/package/validity-bytestring validity-bytestring>\n                .\n                * <https://hackage.haskell.org/package/validity-containers validity-containers>\n                .\n                * <https://hackage.haskell.org/package/validity-path validity-path>\n                .\n                * <https://hackage.haskell.org/package/validity-scientific validity-scientific>\n                .\n                * <https://hackage.haskell.org/package/validity-text validity-text>\n                .\n                * <https://hackage.haskell.org/package/validity-time validity-time>\n                .\n                * <https://hackage.haskell.org/package/validity-unordered-containers validity-unordered-containers>\n                .\n                * <https://hackage.haskell.org/package/validity-uuid validity-uuid>\n                .\n                * <https://hackage.haskell.org/package/validity-vector validity-vector>\ncategory:       Validity\nhomepage:       https://github.com/NorfairKing/validity#readme\nbug-reports:    https://github.com/NorfairKing/validity/issues\nauthor:         Tom Sydney Kerckhove\nmaintainer:     syd@cs-syd.eu\ncopyright:      Copyright: (c) 2016-2021 Tom Sydney Kerckhove\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    LICENSE\n    CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/NorfairKing/validity\n\nlibrary\n  exposed-modules:\n      Data.Validity\n  other-modules:\n      Paths_validity\n  hs-source-dirs:\n      src\n  ghc-options: -Wno-redundant-constraints\n  build-depends:\n      base >=4.13 && <5\n  default-language: Haskell2010\n\ntest-suite validity-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Data.ValiditySpec\n      Paths_validity\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall\n  build-depends:\n      base >=4.13 && <5\n    , hspec\n    , validity\n  default-language: Haskell2010\n";
    }