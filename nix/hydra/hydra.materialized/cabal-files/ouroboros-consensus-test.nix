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
      identifier = { name = "ouroboros-consensus-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Tests of the consensus layer";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          ];
        buildable = true;
        };
      tests = {
        "test-consensus" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-mock" or (errorHandler.buildDepError "ouroboros-consensus-mock"))
            (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
            ];
          buildable = true;
          };
        "test-storage" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
            ];
          buildable = true;
          };
        "test-infra" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-test" or (errorHandler.buildDepError "ouroboros-consensus-test"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-test-0.1.0.0.tar.gz";
      sha256 = "fb76c15de6ccc9e1bd3dcfeafa901aeca8d255d108216b2226307219d8ee3ee1";
      });
    }) // {
    package-description-override = "name:                  ouroboros-consensus-test\nversion:               0.1.0.0\nsynopsis:              Tests of the consensus layer\n-- description:\nlicense:               Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:             2020 Input Output (Hong Kong) Ltd.\nauthor:                IOHK Engineering Team\nmaintainer:            operations@iohk.io\ncategory:              Network\nbuild-type:            Simple\ncabal-version:         >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:\n                       Test.ThreadNet.General\n                       Test.ThreadNet.Network\n                       Test.ThreadNet.Ref.PBFT\n                       Test.ThreadNet.Rekeying\n                       Test.ThreadNet.TxGen\n                       Test.ThreadNet.Util\n                       Test.ThreadNet.Util.Expectations\n                       Test.ThreadNet.Util.HasCreator\n                       Test.ThreadNet.Util.NodeJoinPlan\n                       Test.ThreadNet.Util.NodeRestarts\n                       Test.ThreadNet.Util.NodeToNodeVersion\n                       Test.ThreadNet.Util.NodeTopology\n                       Test.ThreadNet.Util.Seed\n\n                       Test.Util.Blob\n                       Test.Util.BoolProps\n                       Test.Util.ChainUpdates\n                       Test.Util.ChunkInfo\n                       Test.Util.Classify\n                       Test.Util.Corruption\n                       Test.Util.FileLock\n                       Test.Util.FS.Sim.Error\n                       Test.Util.FS.Sim.FsTree\n                       Test.Util.FS.Sim.MockFS\n                       Test.Util.FS.Sim.Pure\n                       Test.Util.FS.Sim.STM\n                       Test.Util.HardFork.Future\n                       Test.Util.HardFork.OracularClock\n                       Test.Util.InvertedMap\n                       Test.Util.LogicalClock\n                       Test.Util.MockChain\n                       Test.Util.Nightly\n                       Test.Util.Orphans.Arbitrary\n                       Test.Util.Orphans.IOLike\n                       Test.Util.Orphans.NoThunks\n                       Test.Util.Orphans.SignableRepresentation\n                       Test.Util.Orphans.Slotting.Arbitrary\n                       Test.Util.Orphans.ToExpr\n                       Test.Util.Paths\n                       Test.Util.QSM\n                       Test.Util.QuickCheck\n                       Test.Util.Range\n                       Test.Util.RefEnv\n                       Test.Util.Schedule\n                       Test.Util.Serialisation.Golden\n                       Test.Util.Serialisation.Roundtrip\n                       Test.Util.Shrink\n                       Test.Util.SOP\n                       Test.Util.Slots\n                       Test.Util.Split\n                       Test.Util.Stream\n                       Test.Util.TestBlock\n                       Test.Util.Time\n                       Test.Util.Tracer\n                       Test.Util.WithEq\n\n  build-depends:       base              >=4.9 && <4.15\n                     , base16-bytestring\n                     , bytestring        >=0.10  && <0.11\n                     , cardano-crypto-class\n                     , cardano-prelude\n                     , cardano-slotting\n                     , cborg             >=0.2.2 && <0.3\n                     , containers        >=0.5   && <0.7\n                     , contra-tracer\n                     , deepseq\n                     , directory\n                     , fgl\n                     , file-embed\n                     , filepath\n                     , generics-sop\n                     , graphviz\n                     , mtl               >=2.2   && <2.3\n                     , nothunks\n                     , QuickCheck\n                     , quickcheck-state-machine\n                     , quiet             >=0.2   && <0.3\n                     , random\n                     , serialise         >=0.2   && <0.3\n                     , sop-core\n                     , strict-containers\n                     , tasty\n                     , tasty-golden\n                     , tasty-quickcheck\n                     , typed-protocols\n                     , template-haskell\n                     , text              >=1.2   && <1.3\n                     , time\n                     , transformers\n                     , tree-diff\n                     , utf8-string\n\n                     , io-classes\n                     , io-sim\n                     , ouroboros-network\n                     , ouroboros-network-framework\n                     , ouroboros-consensus\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n\ntest-suite test-consensus\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test-consensus\n  main-is:             Main.hs\n  other-modules:\n                       Test.Consensus.BlockchainTime.Simple\n                       Test.Consensus.HardFork.Forecast\n                       Test.Consensus.HardFork.History\n                       Test.Consensus.HardFork.Infra\n                       Test.Consensus.HardFork.Summary\n                       Test.Consensus.HardFork.Combinator\n                       Test.Consensus.HardFork.Combinator.A\n                       Test.Consensus.HardFork.Combinator.B\n                       Test.Consensus.MiniProtocol.ChainSync.Client\n                       Test.Consensus.MiniProtocol.LocalStateQuery.Server\n                       Test.Consensus.Mempool\n                       Test.Consensus.Node\n                       Test.Consensus.ResourceRegistry\n                       Test.Consensus.Util.MonadSTM.RAWLock\n                       Test.Consensus.Util.Versioned\n\n  build-depends:       base\n                     , binary\n                     , bytestring\n                     , cardano-binary\n                     , cardano-crypto-class\n                     , cardano-slotting\n                     , cborg\n                     , containers\n                     , contra-tracer\n                     , directory\n                     , generics-sop\n                     , mtl\n                     , nothunks\n                     , QuickCheck\n                     , quickcheck-state-machine\n                     , quiet\n                     , serialise\n                     , sop-core\n                     , tasty\n                     , tasty-hunit\n                     , tasty-quickcheck\n                     , temporary\n                     , time\n                     , tree-diff\n\n                     , io-classes\n                     , io-sim\n                     , typed-protocols\n                     , ouroboros-network\n                     , ouroboros-network-framework\n                     , ouroboros-consensus\n                     , ouroboros-consensus-mock\n                     , ouroboros-consensus-test\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n                       -threaded\n                       -rtsopts\n\ntest-suite test-storage\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test-storage\n  main-is:             Main.hs\n  other-modules:\n                       Test.Ouroboros.Storage\n                       Test.Ouroboros.Storage.ChainDB\n                       Test.Ouroboros.Storage.ChainDB.Iterator\n                       Test.Ouroboros.Storage.ChainDB.GcSchedule\n                       Test.Ouroboros.Storage.ChainDB.Model\n                       Test.Ouroboros.Storage.ChainDB.Model.Test\n                       Test.Ouroboros.Storage.ChainDB.Paths\n                       Test.Ouroboros.Storage.ChainDB.StateMachine\n                       Test.Ouroboros.Storage.FS\n                       Test.Ouroboros.Storage.FS.StateMachine\n                       Test.Ouroboros.Storage.ImmutableDB\n                       Test.Ouroboros.Storage.ImmutableDB.Mock\n                       Test.Ouroboros.Storage.ImmutableDB.Model\n                       Test.Ouroboros.Storage.ImmutableDB.Primary\n                       Test.Ouroboros.Storage.ImmutableDB.StateMachine\n                       Test.Ouroboros.Storage.LedgerDB\n                       Test.Ouroboros.Storage.LedgerDB.DiskPolicy\n                       Test.Ouroboros.Storage.LedgerDB.InMemory\n                       Test.Ouroboros.Storage.LedgerDB.OnDisk\n                       Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary\n                       Test.Ouroboros.Storage.Orphans\n                       Test.Ouroboros.Storage.TestBlock\n                       Test.Ouroboros.Storage.VolatileDB\n                       Test.Ouroboros.Storage.VolatileDB.Mock\n                       Test.Ouroboros.Storage.VolatileDB.Model\n                       Test.Ouroboros.Storage.VolatileDB.StateMachine\n\n  build-depends:       base\n                     , bifunctors\n                     , binary\n                     , bytestring\n                     , cardano-crypto-class\n                     , cardano-slotting\n                     , cborg\n                     , containers\n                     , contra-tracer\n                     , directory\n                     , generics-sop\n                     , hashable\n                     , mtl\n                     , nothunks\n                     , pretty-show\n                     , QuickCheck\n                     , quickcheck-state-machine >=0.7.0\n                     , random\n                     , serialise\n                     , strict-containers\n                     , tasty\n                     , tasty-hunit\n                     , tasty-quickcheck\n                     , temporary\n                     , text\n                     , time\n                     , transformers\n                     , tree-diff\n                     , vector\n\n                     , io-classes\n                     , io-sim\n                     , ouroboros-network\n                     , ouroboros-consensus\n                     , ouroboros-consensus-test\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n\ntest-suite test-infra\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test-infra\n  main-is:             Main.hs\n  other-modules:\n                       Test.ThreadNet.Util.Tests\n                       Test.Util.ChainUpdates.Tests\n                       Test.Util.Schedule.Tests\n                       Test.Util.Split.Tests\n\n  build-depends:       base\n                     , QuickCheck\n                     , tasty\n                     , tasty-quickcheck\n\n                     , ouroboros-consensus\n                     , ouroboros-consensus-test\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -fno-ignore-asserts\n";
    }