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
    flags = { devel = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "async-timer"; version = "0.2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2018 Moritz Clasmeier";
      maintainer = "mtesseract@silverratio.net";
      author = "Moritz Clasmeier";
      homepage = "https://github.com/mtesseract/async-timer#readme";
      url = "";
      synopsis = "Provides API for timer based execution of IO actions";
      description = "This is a lightweight package built on top of the async package\nproviding easy to use periodic timers. This can be used for executing\nIO actions periodically.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ];
        buildable = true;
        };
      tests = {
        "async-timer-test" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."async-timer" or (errorHandler.buildDepError "async-timer"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/async-timer-0.2.0.0.tar.gz";
      sha256 = "0632bfc4c141aa47c461747b3edb59f76ef5523a66ac03be0f32868a5e04cee0";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.20.0.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 097822e1ed8a2252fc52e9ea9b7aad2825e1e6038a125bdce24d3e0b0d32cdd1\n\nname:           async-timer\nversion:        0.2.0.0\nsynopsis:       Provides API for timer based execution of IO actions\ndescription:    This is a lightweight package built on top of the async package\n                providing easy to use periodic timers. This can be used for executing\n                IO actions periodically.\ncategory:       Concurrency\nhomepage:       https://github.com/mtesseract/async-timer#readme\nbug-reports:    https://github.com/mtesseract/async-timer/issues\nauthor:         Moritz Clasmeier\nmaintainer:     mtesseract@silverratio.net\ncopyright:      (c) 2016-2018 Moritz Clasmeier\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\nextra-source-files:\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/mtesseract/async-timer\n\nflag devel\n  manual: True\n  default: False\n\nlibrary\n  hs-source-dirs:\n      src\n  build-depends:\n      async >=2.2.1 && <2.3\n    , base >=4.9.1.0 && <5\n    , safe-exceptions >=0.1.7.0 && <0.2\n    , unliftio >=0.2.4.0 && <0.3\n    , unliftio-core >=0.1.1.0 && <0.2\n  if flag(devel)\n    ghc-options: -Wall -fno-warn-type-defaults -Werror\n  else\n    ghc-options: -Wall -fno-warn-type-defaults\n  exposed-modules:\n      Control.Concurrent.Async.Timer\n  other-modules:\n      Control.Concurrent.Async.Timer.Internal\n      Paths_async_timer\n  default-language: Haskell2010\n\ntest-suite async-timer-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      test\n  default-extensions: OverloadedStrings\n  build-depends:\n      async\n    , async-timer\n    , base\n    , containers\n    , criterion\n    , tasty\n    , tasty-hunit\n  if flag(devel)\n    ghc-options: -Wall -fno-warn-type-defaults -Werror\n  else\n    ghc-options: -Wall -fno-warn-type-defaults\n  other-modules:\n      Paths_async_timer\n  default-language: Haskell2010\n";
    }