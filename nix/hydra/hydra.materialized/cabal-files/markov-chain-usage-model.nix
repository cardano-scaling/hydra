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
      identifier = { name = "markov-chain-usage-model"; version = "0.0.0"; };
      license = "BSD-2-Clause";
      copyright = "Copyright (C) 2018-2019, HERE Europe B.V.";
      maintainer = "stevan.andjelkovic@here.com";
      author = "Stevan Andjelkovic";
      homepage = "https://github.com/advancedtelematic/markov-chain-usage-model#readme";
      url = "";
      synopsis = "Computations for Markov chain usage models";
      description = "Please see the README on GitHub at <https://github.com/advancedtelematic/markov-chain-usage-model#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."matrix" or (errorHandler.buildDepError "matrix"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "markov-chain-usage-model-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."markov-chain-usage-model" or (errorHandler.buildDepError "markov-chain-usage-model"))
            (hsPkgs."matrix" or (errorHandler.buildDepError "matrix"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/markov-chain-usage-model-0.0.0.tar.gz";
      sha256 = "4c8e59c753ddcb8a3273c44712fc91e20be7a3f0f3a485f3f9f87003f71a2793";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\nname:           markov-chain-usage-model\nversion:        0.0.0\nsynopsis:       Computations for Markov chain usage models\ndescription:    Please see the README on GitHub at <https://github.com/advancedtelematic/markov-chain-usage-model#readme>\nhomepage:       https://github.com/advancedtelematic/markov-chain-usage-model#readme\nbug-reports:    https://github.com/advancedtelematic/markov-chain-usage-model/issues\nauthor:         Stevan Andjelkovic\nmaintainer:     stevan.andjelkovic@here.com\ncopyright:      Copyright (C) 2018-2019, HERE Europe B.V.\ncategory:       Testing\nlicense:        BSD2\nlicense-file:   LICENSE\nbuild-type:     Simple\ntested-with:    GHC == 8.2.2, GHC == 8.4.3, GHC == 8.6.3\nextra-source-files:\n    README.md\n    CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/advancedtelematic/markov-chain-usage-model\n\nlibrary\n  exposed-modules:\n      MarkovChain\n  other-modules:\n      Paths_markov_chain_usage_model\n  hs-source-dirs:\n      src\n  ghc-options:\n      -Weverything\n      -Wno-missing-exported-signatures\n      -Wno-missing-import-lists\n      -Wno-missed-specialisations\n      -Wno-all-missed-specialisations\n      -Wno-unsafe\n      -Wno-safe\n      -Wno-missing-local-signatures\n      -Wno-monomorphism-restriction\n  build-depends:\n      base >=4.10 && <5\n    , matrix >= 0.3.6.1\n    , vector >= 0.10\n  default-language: Haskell2010\n\ntest-suite markov-chain-usage-model-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Paths_markov_chain_usage_model\n    , Unit\n  hs-source-dirs:\n      test\n  ghc-options:\n      -threaded -rtsopts -with-rtsopts=-N\n      -Weverything\n      -Wno-missing-exported-signatures\n      -Wno-missing-import-lists\n      -Wno-missed-specialisations\n      -Wno-all-missed-specialisations\n      -Wno-unsafe\n      -Wno-safe\n      -Wno-missing-local-signatures\n      -Wno-monomorphism-restriction\n  build-depends:\n      base >=4.7 && <5\n    , doctest\n    , markov-chain-usage-model\n    , matrix\n    , tasty\n    , tasty-discover\n    , tasty-hunit\n    , vector\n  default-language: Haskell2010\n";
    }