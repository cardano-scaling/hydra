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
      specVersion = "3.0";
      identifier = { name = "cardano-ledger-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "nicholas.clarke@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Testing harness, tests and benchmarks for Shelley style cardano ledgers";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
          (hsPkgs."genvalidity-scientific" or (errorHandler.buildDepError "genvalidity-scientific"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."hkd" or (errorHandler.buildDepError "hkd"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."monad-supply" or (errorHandler.buildDepError "monad-supply"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."QuickCheck-GenT" or (errorHandler.buildDepError "QuickCheck-GenT"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."writer-cps-mtl" or (errorHandler.buildDepError "writer-cps-mtl"))
          ];
        buildable = true;
        };
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-test" or (errorHandler.buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "benchProperty" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-test-0.1.0.0.tar.gz";
      sha256 = "af1697138e364da104aee737b829228f24a323cce5da07b5f162733555e5baca";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                cardano-ledger-test\nversion:             0.1.0.0\nsynopsis:\n  Testing harness, tests and benchmarks for Shelley style cardano ledgers\n-- description:\n-- bug-reports:\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          nicholas.clarke@iohk.io\ncopyright:           2020 Input Output (Hong Kong) Ltd.\n-- category:\nbuild-type:          Simple\nextra-source-files:\n  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/cardano-ledger-test\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wpartial-fields\n                      -Wredundant-constraints\n                      -Wunused-packages\n\n\nlibrary\n  import:             base, project-config\n  hs-source-dirs:     src\n  other-modules:\n    Data.Group.GrpMap\n    Data.Functor.PiecewiseConstant\n    Test.Cardano.Ledger.Orphans\n  exposed-modules:\n    Test.Cardano.Ledger.Alonzo.Tools\n    Test.Cardano.Ledger.BaseTypes\n    Test.Cardano.Ledger.Examples.BabbageFeatures\n    Test.Cardano.Ledger.Examples.TwoPhaseValidation\n    Test.Cardano.Ledger.Generic.AggPropTests\n    Test.Cardano.Ledger.Generic.ApplyTx\n    Test.Cardano.Ledger.Generic.Indexed\n    Test.Cardano.Ledger.Generic.Fields\n    Test.Cardano.Ledger.Generic.Functions\n    Test.Cardano.Ledger.Generic.GenState\n    Test.Cardano.Ledger.Generic.TxGen\n    Test.Cardano.Ledger.Generic.Types\n    Test.Cardano.Ledger.Generic.Proof\n    Test.Cardano.Ledger.Generic.MockChain\n    Test.Cardano.Ledger.Generic.ModelState\n    Test.Cardano.Ledger.Generic.PrettyCore\n    Test.Cardano.Ledger.Generic.Properties\n    Test.Cardano.Ledger.Generic.Scriptic\n    Test.Cardano.Ledger.Generic.Trace\n    Test.Cardano.Ledger.Generic.Updaters\n    Test.Cardano.Ledger.Model.API\n    Test.Cardano.Ledger.Model.Acnt\n    Test.Cardano.Ledger.Model.BaseTypes\n    Test.Cardano.Ledger.Model.Elaborators\n    Test.Cardano.Ledger.Model.Elaborators.Alonzo\n    Test.Cardano.Ledger.Model.Elaborators.Shelley\n    Test.Cardano.Ledger.Model.FeatureSet\n    Test.Cardano.Ledger.Model.Fixup\n    Test.Cardano.Ledger.Model.Generators\n    Test.Cardano.Ledger.Model.Generators.Address\n    Test.Cardano.Ledger.Model.Generators.Certificates\n    Test.Cardano.Ledger.Model.Generators.Chain\n    Test.Cardano.Ledger.Model.Generators.Script\n    Test.Cardano.Ledger.Model.Generators.Shrinking\n    Test.Cardano.Ledger.Model.Generators.Tx\n    Test.Cardano.Ledger.Model.Generators.TxOut\n    Test.Cardano.Ledger.Model.Generators.Value\n    Test.Cardano.Ledger.Model.LedgerState\n    Test.Cardano.Ledger.Model.PParams\n    Test.Cardano.Ledger.Model.Properties\n    Test.Cardano.Ledger.Model.Properties.Utils\n    Test.Cardano.Ledger.Model.Prov\n    Test.Cardano.Ledger.Model.Rewards\n    Test.Cardano.Ledger.Model.Rules\n    Test.Cardano.Ledger.Model.Script\n    Test.Cardano.Ledger.Model.Snapshot\n    Test.Cardano.Ledger.Model.Tx\n    Test.Cardano.Ledger.Model.TxOut\n    Test.Cardano.Ledger.Model.UTxO\n    Test.Cardano.Ledger.Model.Value\n    Test.Cardano.Ledger.Rational\n    Test.Cardano.Ledger.TestableEra\n    Test.Cardano.Ledger.ValueFromList\n  build-depends:\n    aeson >= 2,\n    array,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-alonzo,\n    cardano-ledger-alonzo-test,\n    cardano-ledger-babbage,\n    cardano-ledger-core,\n    cardano-ledger-pretty,\n    cardano-ledger-shelley-ma,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    containers,\n    vector-map,\n    data-default-class,\n    deepseq,\n    genvalidity,\n    genvalidity-scientific,\n    groups,\n    hkd,\n    lens,\n    monad-supply,\n    mtl,\n    plutus-ledger-api,\n    plutus-core,\n    plutus-tx,\n    prettyprinter,\n    profunctors,\n    QuickCheck,\n    quickcheck-instances,\n    QuickCheck-GenT,\n    quiet,\n    random,\n    scientific,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-test,\n    cardano-ledger-babbage-test,\n    semigroupoids,\n    small-steps,\n    small-steps-test,\n    set-algebra,\n    some,\n    strict-containers,\n    tagged,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    text,\n    time,\n    transformers,\n    vector,\n    writer-cps-mtl,\n\n\ntest-suite cardano-ledger-test\n  import:             base, project-config\n\n  type:                exitcode-stdio-1.0\n  main-is:             Tests.hs\n  ghc-options: -rtsopts -threaded\n\n  hs-source-dirs:      test\n  other-modules:\n  build-depends:\n    cardano-ledger-test,\n    cardano-ledger-shelley-test,\n    tasty,\n    data-default-class,\n\n\n\nbenchmark bench\n  import:             base, project-config\n\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:\n    bench\n  main-is:          Main.hs\n  other-modules:\n    Bench.Cardano.Ledger.ApplyTx\n    Bench.Cardano.Ledger.ApplyTx.Gen\n    Bench.Cardano.Ledger.Balance\n    Bench.Cardano.Ledger.EpochBoundary\n    Bench.Cardano.Ledger.Serialisation.Generators\n    Bench.Cardano.Ledger.SumStake\n    Bench.Cardano.Ledger.TxOut\n  build-depends:\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-alonzo,\n    cardano-ledger-alonzo-test,\n    cardano-ledger-core,\n    cardano-ledger-shelley-ma-test,\n    cardano-ledger-shelley-ma,\n    vector-map,\n    containers,\n    criterion,\n    data-default-class,\n    deepseq,\n    QuickCheck,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-test,\n    random,\n    small-steps,\n    small-steps-test,\n    text\n  ghc-options:\n      -threaded\n      -rtsopts\n      -O2\n\nbenchmark benchProperty\n  import:             base, project-config\n\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:\n    benchProperty\n  main-is:          Main.hs\n  other-modules:\n  build-depends:\n    cardano-ledger-alonzo,\n    cardano-ledger-alonzo-test,\n    cardano-ledger-shelley-ma-test,\n    QuickCheck,\n    small-steps,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-test,\n    tasty-quickcheck,\n    tasty\n  ghc-options:\n      -threaded\n      -rtsopts\n      -O2\n";
    }