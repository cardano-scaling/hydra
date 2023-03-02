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
      identifier = {
        name = "cardano-ledger-shelley-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Test helpers from cardano-ledger-shelley exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "cardano-ledger-shelley-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "mainbench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-shelley-test-0.1.0.0.tar.gz";
      sha256 = "3eae1ed8939db1d5aaab558183a27a3ccedd7324bd0f5aa28f42477fd07c476c";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-shelley-test\nversion:             0.1.0.0\ndescription:         Test helpers from cardano-ledger-shelley exposed to other packages\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\nbuild-type:          Simple\n\nextra-source-files:\n  cddl-files/shelley.cddl\n  cddl-files/real/crypto.cddl\n  cddl-files/mock/extras.cddl\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/cardano-ledger.git\n  subdir:   eras/shelley/test-suite\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n  hs-source-dirs:     src\n  exposed-modules:\n    Test.Cardano.Crypto.VRF.Fake\n    Test.Cardano.Ledger.TerseTools\n    Test.Cardano.Ledger.Shelley.Address.Bootstrap\n    Test.Cardano.Ledger.Shelley.Address.CompactAddr\n    Test.Cardano.Ledger.Shelley.BenchmarkFunctions\n    Test.Cardano.Ledger.Shelley.ByronTranslation\n    Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes\n    Test.Cardano.Ledger.Shelley.Examples.Cast\n    Test.Cardano.Ledger.Shelley.Examples.Consensus\n    Test.Cardano.Ledger.Shelley.Examples.Federation\n    Test.Cardano.Ledger.Shelley.Generator.Block\n    Test.Cardano.Ledger.Shelley.Generator.Constants\n    Test.Cardano.Ledger.Shelley.Generator.Core\n    Test.Cardano.Ledger.Shelley.Generator.Delegation\n    Test.Cardano.Ledger.Shelley.Generator.Metadata\n    Test.Cardano.Ledger.Shelley.Generator.Presets\n    Test.Cardano.Ledger.Shelley.Generator.Trace.Chain\n    Test.Cardano.Ledger.Shelley.Generator.Trace.DCert\n    Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger\n    Test.Cardano.Ledger.Shelley.Generator.Update\n    Test.Cardano.Ledger.Shelley.Generator.Utxo\n    Test.Cardano.Ledger.Shelley.Generator.EraGen\n    Test.Cardano.Ledger.Shelley.Generator.ScriptClass\n    Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen\n    Test.Cardano.Ledger.Shelley.LaxBlock\n    Test.Cardano.Ledger.Shelley.Orphans\n    Test.Cardano.Ledger.Shelley.PropertyTests\n    Test.Cardano.Ledger.Shelley.Rewards\n    Test.Cardano.Ledger.Shelley.Rules.Chain\n    Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces\n    Test.Cardano.Ledger.Shelley.Rules.TestChain\n    Test.Cardano.Ledger.Shelley.Rules.TestDeleg\n    Test.Cardano.Ledger.Shelley.Rules.TestPool\n    Test.Cardano.Ledger.Shelley.Rules.TestPoolreap\n    Test.Cardano.Ledger.Shelley.Serialisation.CDDLUtils\n    Test.Cardano.Ledger.Shelley.Serialisation.Generators\n    Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators\n    Test.Cardano.Ledger.Shelley.Serialisation.Generators.Bootstrap\n    Test.Cardano.Ledger.Shelley.Serialisation.Generators.Genesis\n    Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils\n    Test.Cardano.Ledger.Shelley.ShelleyTranslation\n    Test.Cardano.Ledger.Shelley.Shrinkers\n    Test.Cardano.Ledger.Shelley.Utils\n    Test.TestScenario\n  build-depends:\n    base16-bytestring >= 1,\n    binary,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-crypto-test,\n    cardano-crypto-wrapper,\n    cardano-crypto,\n    cardano-data,\n    cardano-ledger-byron,\n    cardano-ledger-byron-test,\n    cardano-ledger-core,\n    cardano-ledger-pretty,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    containers,\n    vector-map,\n    data-default-class,\n    deepseq,\n    generic-random,\n    hashable,\n    hedgehog-quickcheck,\n    hedgehog >= 1.0.4,\n    iproute,\n    mtl,\n    nothunks,\n    process-extras,\n    plutus-ledger-api,\n    QuickCheck >= 2.13.2,\n    serialise,\n    cardano-ledger-shelley,\n    set-algebra,\n    small-steps-test,\n    small-steps,\n    strict-containers,\n    tasty-hunit,\n    tasty-quickcheck,\n    tasty,\n    text,\n    time,\n    transformers,\n    tree-diff,\n    unliftio,\n    vector,\n\ntest-suite cardano-ledger-shelley-test\n  import:             base, project-config\n\n  type:                exitcode-stdio-1.0\n  main-is:             Tests.hs\n  other-modules:\n      Test.Cardano.Ledger.Shelley.Examples\n      Test.Cardano.Ledger.Shelley.Examples.Combinators\n      Test.Cardano.Ledger.Shelley.Examples.EmptyBlock\n      Test.Cardano.Ledger.Shelley.Examples.Init\n      Test.Cardano.Ledger.Shelley.Examples.GenesisDelegation\n      Test.Cardano.Ledger.Shelley.Examples.NetworkID\n      Test.Cardano.Ledger.Shelley.Examples.Mir\n      Test.Cardano.Ledger.Shelley.Examples.MirTransfer\n      Test.Cardano.Ledger.Shelley.Examples.PoolLifetime\n      Test.Cardano.Ledger.Shelley.Examples.PoolReReg\n      Test.Cardano.Ledger.Shelley.Examples.TwoPools\n      Test.Cardano.Ledger.Shelley.Examples.Updates\n      Test.Cardano.Ledger.Shelley.Fees\n      Test.Cardano.Ledger.Shelley.MultiSigExamples\n      Test.Cardano.Ledger.Shelley.Pretty\n      Test.Cardano.Ledger.Shelley.SafeHash\n      Test.Cardano.Ledger.Shelley.Serialisation\n      Test.Cardano.Ledger.Shelley.Serialisation.CDDL\n      Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address\n      Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding\n      Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis\n      Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR\n      Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON\n      Test.Cardano.Ledger.Shelley.RulesTests\n      Test.Cardano.Ledger.Shelley.UnitTests\n\n  hs-source-dirs:      test\n  ghc-options:\n      -threaded\n      -rtsopts\n      -with-rtsopts=-N\n      -- We set a bound here so that we're alerted of potential space\n      -- leaks in our generators (or test) code.\n      --\n      -- The 4 megabytes stack bound and 250 megabytes heap bound were\n      -- determined ad-hoc.\n      \"-with-rtsopts=-K4m -M250m\"\n  build-depends:\n      aeson >= 2,\n      base16-bytestring >= 1,\n      binary,\n      bytestring,\n      cardano-binary,\n      cardano-data,\n      cardano-crypto-class,\n      cardano-ledger-byron,\n      cardano-ledger-core,\n      cardano-ledger-pretty,\n      cardano-protocol-tpraos,\n      cardano-slotting,\n      cborg,\n      containers,\n      data-default-class,\n      groups,\n      hedgehog >= 1.0.4,\n      iproute,\n      prettyprinter,\n      QuickCheck,\n      cardano-ledger-shelley-test,\n      cardano-ledger-shelley,\n      scientific,\n      small-steps-test,\n      small-steps,\n      strict-containers,\n      tasty-hedgehog,\n      tasty-hunit,\n      tasty-quickcheck,\n      tasty,\n      time,\n\n\nbenchmark mainbench\n  import:             base, project-config\n\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:\n    bench\n  main-is:          Main.hs\n  other-modules:\n    Bench.Control.Iterate.SetAlgebra.Bimap,\n    BenchUTxOAggregate,\n    BenchValidation,\n    Cardano.Ledger.Shelley.Bench.Gen\n    Cardano.Ledger.Shelley.Bench.Rewards\n\n  build-depends:\n    cardano-crypto-class,\n    cardano-crypto-praos,\n    cardano-data,\n    cardano-ledger-core,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    containers,\n    criterion,\n    data-default-class,\n    deepseq,\n    mtl,\n    QuickCheck,\n    cardano-ledger-shelley-test,\n    cardano-ledger-shelley,\n    set-algebra,\n    small-steps,\n    small-steps-test,\n    strict-containers,\n    transformers\n  ghc-options:\n      -threaded\n      -rtsopts\n      -O2\n";
    }