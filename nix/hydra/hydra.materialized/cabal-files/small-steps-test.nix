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
      identifier = { name = "small-steps-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Small step semantics testing library";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."goblins" or (errorHandler.buildDepError "goblins"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          ];
        buildable = true;
        };
      tests = {
        "examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/small-steps-test-0.1.0.0.tar.gz";
      sha256 = "fea569fc798d5ea2b766969a4075e12c2698c038666d92b872c870a9184746af";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                small-steps-test\nversion:             0.1.0.0\nsynopsis:            Small step semantics testing library\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncategory:            Control\nbuild-type:          Simple\nextra-source-files:\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/small-steps-test\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:     Control.State.Transition.Invalid.Trace\n                     , Control.State.Transition.Generator\n                     , Control.State.Transition.Trace\n                     , Control.State.Transition.Trace.Generator.QuickCheck\n                     , Hedgehog.Extra.Manual\n  build-depends:       goblins\n                     , deepseq\n                     , hedgehog >= 1.0.4\n                     , tasty-hunit\n                     , microlens\n                     , microlens-th\n                     , mtl\n                     , nothunks\n                     , transformers >= 0.5\n                     , QuickCheck\n                     -- IOHK deps\n                     , small-steps\n                     , strict-containers\n  hs-source-dirs:      src\n\ntest-suite examples\n  import:             base, project-config\n\n  hs-source-dirs:      test\n  main-is:             examples/Main.hs\n  other-modules:       Control.State.Transition.Examples.Sum\n  other-modules:       Control.State.Transition.Examples.GlobalSum\n                     , Control.State.Transition.Examples.CommitReveal\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  build-depends:       containers\n                     , hedgehog >= 1.0.4\n                     , mtl\n                     , tasty\n                     , tasty-hedgehog\n                     , tasty-expected-failure\n                     , QuickCheck\n                     , tasty-quickcheck\n                     , tasty-hunit\n                     , Unique\n                     -- IOHK deps\n                     , cardano-crypto-class\n                     , cardano-binary\n                     -- Local deps\n                     , small-steps\n                     , small-steps-test\n  ghc-options:        -threaded\n";
    }