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
      specVersion = "2.2";
      identifier = { name = "quickcheck-dynamic"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "arnaud.bailly@iohk.io";
      author = "Ulf Norell";
      homepage = "https://github.com/input-output-hk/quickcheck-dynamic#readme";
      url = "";
      synopsis = "A library for stateful property-based testing";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/quickcheck-dynamic#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-dynamic-2.0.0.tar.gz";
      sha256 = "78082446894d9522a1e9f9589f758fef2fcb7778d9427fe8ce1cedc156ea48ed";
      });
    }) // {
    package-description-override = "cabal-version:   2.2\nname:            quickcheck-dynamic\nversion:         2.0.0\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:      arnaud.bailly@iohk.io\nauthor:          Ulf Norell\ncategory:        Testing\nsynopsis:\n  A library for stateful property-based testing\nhomepage:\n  https://github.com/input-output-hk/quickcheck-dynamic#readme\n\nbug-reports:\n  https://github.com/input-output-hk/quickcheck-dynamic/issues\n\ndescription:\n  Please see the README on GitHub at <https://github.com/input-output-hk/quickcheck-dynamic#readme>\n\nbuild-type:      Simple\nextra-doc-files: README.md\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/quickcheck-dynamic\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFunctor\n    DeriveDataTypeable\n    StandaloneDeriving\n    ImportQualifiedPost\n    TupleSections\n    LambdaCase\n    PatternSynonyms\n    GADTs\n    TypeApplications\n    ScopedTypeVariables\n    TypeFamilies\n    FlexibleContexts\n    FlexibleInstances\n    MultiParamTypeClasses\n    RankNTypes\n    ViewPatterns\n    TypeOperators\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wunused-packages\n    -Wincomplete-uni-patterns -Wincomplete-record-updates\n    -Wredundant-constraints -Widentities\n\nlibrary\n    import: lang\n    hs-source-dirs: src\n    exposed-modules:\n        Test.QuickCheck.DynamicLogic\n        Test.QuickCheck.DynamicLogic.CanGenerate\n        Test.QuickCheck.DynamicLogic.Core\n        Test.QuickCheck.DynamicLogic.Quantify\n        Test.QuickCheck.DynamicLogic.SmartShrinking\n        Test.QuickCheck.DynamicLogic.Utils\n        Test.QuickCheck.StateModel\n    build-depends:\n        QuickCheck -any,\n        base >=4.7 && <5,\n        random -any,\n        mtl -any\n";
    }