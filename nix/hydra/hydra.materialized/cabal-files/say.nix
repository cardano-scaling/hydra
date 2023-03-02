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
      identifier = { name = "say"; version = "0.1.0.1"; };
      license = "MIT";
      copyright = "2016-2018 FP Complete";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/fpco/say#readme";
      url = "";
      synopsis = "Send textual messages to a Handle in a thread-friendly way";
      description = "Please see the README and documentation at <https://www.stackage.org/package/say>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "say-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."say" or (errorHandler.buildDepError "say"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "say-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."say" or (errorHandler.buildDepError "say"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/say-0.1.0.1.tar.gz";
      sha256 = "f639656fc21925c45f3f55769b9fb7a90699e943376a725e215a5deea473b3e4";
      });
    }) // {
    package-description-override = "cabal-version: >= 1.10\n\n-- This file has been generated from package.yaml by hpack version 0.29.6.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 2dfe6e8fbd947bd63280cdcdd1f4fb437beeed6a147b44e1da866b43b1f1722a\n\nname:           say\nversion:        0.1.0.1\nsynopsis:       Send textual messages to a Handle in a thread-friendly way\ndescription:    Please see the README and documentation at <https://www.stackage.org/package/say>\ncategory:       Text\nhomepage:       https://github.com/fpco/say#readme\nbug-reports:    https://github.com/fpco/say/issues\nauthor:         Michael Snoyman\nmaintainer:     michael@snoyman.com\ncopyright:      2016-2018 FP Complete\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    ChangeLog.md\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/fpco/say\n\nlibrary\n  exposed-modules:\n      Say\n  other-modules:\n      Paths_say\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.9.1 && <5\n    , bytestring >=0.10.4\n    , text >=1.2\n    , transformers\n  default-language: Haskell2010\n\ntest-suite say-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      SaySpec\n      Paths_say\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.9.1 && <5\n    , bytestring >=0.10.4\n    , hspec\n    , say\n    , text >=1.2\n    , transformers\n    , unliftio\n  default-language: Haskell2010\n\nbenchmark say-bench\n  type: exitcode-stdio-1.0\n  main-is: say-bench.hs\n  other-modules:\n      Paths_say\n  hs-source-dirs:\n      bench\n  ghc-options: -threaded -O2 -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.9.1 && <5\n    , bytestring >=0.10.4\n    , gauge\n    , say\n    , text >=1.2\n    , transformers\n    , unliftio\n  default-language: Haskell2010\n";
    }