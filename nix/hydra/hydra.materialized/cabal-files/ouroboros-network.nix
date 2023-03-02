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
    flags = { asserts = false; ipv6 = false; cddl = true; };
    package = {
      specVersion = "3.0";
      identifier = { name = "ouroboros-network"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "A networking layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      sublibs = {
        "protocol-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            ];
          buildable = true;
          };
        };
      exes = {
        "demo-chain-sync" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            ];
          buildable = true;
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."deque" or (errorHandler.buildDepError "deque"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-framework".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-framework:testlib"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."ouroboros-network".components.sublibs.protocol-tests or (errorHandler.buildDepError "ouroboros-network:protocol-tests"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          };
        "cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network".components.sublibs.protocol-tests or (errorHandler.buildDepError "ouroboros-network:protocol-tests"))
            ];
          buildable = if flags.cddl then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-network-0.1.0.1.tar.gz";
      sha256 = "2e7590a902eb5451e9535b80bda478f82528182f5900436c0b0258ddab57be7f";
      });
    }) // {
    package-description-override = "cabal-version:       3.0\nname:                ouroboros-network\nversion:             0.1.0.1\nsynopsis:            A networking layer for the Ouroboros blockchain protocol\n-- description:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer:\ncategory:            Network\nbuild-type:          Simple\nextra-source-files:\n  ChangeLog.md\ndata-files:\n  test-cddl/specs/handshake-node-to-node.cddl\n  test-cddl/specs/handshake-node-to-client.cddl\n  test-cddl/specs/chain-sync.cddl\n  test-cddl/specs/block-fetch.cddl\n  test-cddl/specs/tx-submission2.cddl\n  test-cddl/specs/keep-alive.cddl\n  test-cddl/specs/local-tx-submission.cddl\n  test-cddl/specs/local-state-query.cddl\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nFlag ipv6\n  Description: Enable IPv6 test cases\n  Manual: True\n  -- Default to False since travis lacks IPv6 support\n  Default: False\n\nflag cddl\n  Description: Enable CDDL based tests of the CBOR encoding\n  Manual: True\n  -- These tests need the cddl and the cbor-diag Ruby-package\n  Default: True\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:     Ouroboros.Network.AnchoredFragment\n                       Ouroboros.Network.AnchoredSeq\n                       Ouroboros.Network.Block\n                       Ouroboros.Network.BlockFetch\n                       Ouroboros.Network.BlockFetch.Client\n                       Ouroboros.Network.BlockFetch.ClientRegistry\n                       Ouroboros.Network.BlockFetch.ClientState\n                       Ouroboros.Network.BlockFetch.Decision\n                       Ouroboros.Network.BlockFetch.DeltaQ\n                       Ouroboros.Network.BlockFetch.State\n                       Ouroboros.Network.DeltaQ\n                       Ouroboros.Network.Diffusion\n                       Ouroboros.Network.Diffusion.P2P\n                       Ouroboros.Network.Diffusion.NonP2P\n                       Ouroboros.Network.Diffusion.Policies\n                       Ouroboros.Network.KeepAlive\n                       Ouroboros.Network.Magic\n                       Ouroboros.Network.NodeToNode\n                       Ouroboros.Network.NodeToNode.Version\n                       Ouroboros.Network.NodeToClient\n                       Ouroboros.Network.NodeToClient.Version\n                       Ouroboros.Network.Tracers\n                       Ouroboros.Network.Point\n                       Ouroboros.Network.PeerSelection.Types\n                       Ouroboros.Network.PeerSelection.EstablishedPeers\n                       Ouroboros.Network.PeerSelection.KnownPeers\n                       Ouroboros.Network.PeerSelection.LedgerPeers\n                       Ouroboros.Network.PeerSelection.LocalRootPeers\n                       Ouroboros.Network.PeerSelection.PeerMetric\n                       Ouroboros.Network.PeerSelection.PeerMetric.Type\n                       Ouroboros.Network.PeerSelection.PeerStateActions\n                       Ouroboros.Network.PeerSelection.RelayAccessPoint\n                       Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions\n                       Ouroboros.Network.PeerSelection.RootPeersDNS\n                       Ouroboros.Network.PeerSelection.Governor\n                       Ouroboros.Network.PeerSelection.Simple\n                       Ouroboros.Network.Protocol.ChainSync.Client\n                       Ouroboros.Network.Protocol.ChainSync.ClientPipelined\n                       Ouroboros.Network.Protocol.ChainSync.Codec\n                       Ouroboros.Network.Protocol.ChainSync.Server\n                       Ouroboros.Network.Protocol.ChainSync.Type\n                       Ouroboros.Network.Protocol.ChainSync.PipelineDecision\n                       -- ChainSync.Examples module is needed by test-consensus\n                       Ouroboros.Network.Protocol.ChainSync.Examples\n                       Ouroboros.Network.Protocol.BlockFetch.Type\n                       Ouroboros.Network.Protocol.BlockFetch.Client\n                       Ouroboros.Network.Protocol.BlockFetch.Server\n                       Ouroboros.Network.Protocol.BlockFetch.Codec\n                       Ouroboros.Network.Protocol.LocalStateQuery.Client\n                       Ouroboros.Network.Protocol.LocalStateQuery.Codec\n                       -- LocalStateQuery.Examples module is needed by test-consensus\n                       Ouroboros.Network.Protocol.LocalStateQuery.Examples\n                       Ouroboros.Network.Protocol.LocalStateQuery.Server\n                       Ouroboros.Network.Protocol.LocalStateQuery.Type\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Type\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Client\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Server\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Codec\n                       Ouroboros.Network.Protocol.TxSubmission2.Type\n                       Ouroboros.Network.Protocol.TxSubmission2.Codec\n                       Ouroboros.Network.Protocol.TxSubmission2.Client\n                       Ouroboros.Network.Protocol.TxSubmission2.Server\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Type\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Client\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Server\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Codec\n                       Ouroboros.Network.Protocol.KeepAlive.Type\n                       Ouroboros.Network.Protocol.KeepAlive.Client\n                       Ouroboros.Network.Protocol.KeepAlive.Server\n                       Ouroboros.Network.Protocol.KeepAlive.Codec\n                       Ouroboros.Network.TxSubmission.Inbound\n                       Ouroboros.Network.TxSubmission.Mempool.Reader\n                       Ouroboros.Network.TxSubmission.Outbound\n  other-modules:       Ouroboros.Network.Diffusion.Common\n                       Ouroboros.Network.PeerSelection.Governor.ActivePeers\n                       Ouroboros.Network.PeerSelection.Governor.EstablishedPeers\n                       Ouroboros.Network.PeerSelection.Governor.KnownPeers\n                       Ouroboros.Network.PeerSelection.Governor.Monitor\n                       Ouroboros.Network.PeerSelection.Governor.RootPeers\n                       Ouroboros.Network.PeerSelection.Governor.Types\n\n  default-language:    Haskell2010\n  other-extensions:    BangPatterns,\n                       DataKinds,\n                       EmptyCase,\n                       ExistentialQuantification,\n                       FlexibleContexts,\n                       FlexibleInstances,\n                       FunctionalDependencies,\n                       GADTs,\n                       GADTSyntax,\n                       GeneralizedNewtypeDeriving,\n                       MultiParamTypeClasses,\n                       NamedFieldPuns,\n                       OverloadedStrings,\n                       PolyKinds,\n                       RankNTypes,\n                       RecordWildCards,\n                       ScopedTypeVariables,\n                       TemplateHaskell,\n                       TupleSections,\n                       TypeApplications,\n                       TypeFamilies,\n                       TypeInType\n  build-depends:       base              >=4.9 && <4.15,\n                       aeson,\n                       async             >=2.2 && <2.3,\n                       base16-bytestring,\n                       bytestring        >=0.10 && <0.11,\n                       cborg             >=0.2.1 && <0.3,\n                       containers,\n                       deepseq,\n                       directory,\n                       dns,\n                       fingertree        >=0.1.4.2 && <0.2,\n                       iproute,\n                       nothunks,\n                       network           >=3.1.2 && <3.2,\n                       pretty-simple,\n                       psqueues          >=0.2.3 && <0.3,\n                       serialise         >=0.2   && <0.3,\n                       random,\n                       strict-containers,\n\n                       cardano-binary,\n                       cardano-prelude,\n                       cardano-slotting,\n                       contra-tracer,\n                       monoidal-synchronisation,\n\n                       io-classes        >=0.1 && <0.3,\n                       network-mux       >=0.1 && <1.0,\n                       ouroboros-network-framework\n                                         >=0.1 && <1.0,\n                       strict-stm        >=0.1 && <0.2,\n                       typed-protocols   >=0.1 && <1.0,\n                       typed-protocols-cborg\n                                         >=0.1 && <1.0\n  if !os(windows)\n    build-depends:     unix\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n  if flag(asserts)\n    ghc-options:       -fno-ignore-asserts\n\n\n  -- Still in the lib for now as they're used in ouroboros-consensus.\n  -- They should be moved to the separate test lib if they're still needed.\n  exposed-modules:     Ouroboros.Network.MockChain.Chain\n                       Ouroboros.Network.MockChain.ProducerState\n                       Ouroboros.Network.Testing.ConcreteBlock\n  build-depends:       hashable          >=1.2 && <1.4,\n                       text              >=1.2 && <1.3,\n                       time              >=1.9.1 && <1.11\n\nlibrary protocol-tests\n  hs-source-dirs:      protocol-tests\n  default-language:    Haskell2010\n  exposed-modules:     Ouroboros.Network.Protocol.BlockFetch.Direct\n                       Ouroboros.Network.Protocol.BlockFetch.Examples\n                       Ouroboros.Network.Protocol.BlockFetch.Test\n                       Ouroboros.Network.Protocol.ChainSync.Direct\n                       Ouroboros.Network.Protocol.ChainSync.DirectPipelined\n                       Ouroboros.Network.Protocol.ChainSync.ExamplesPipelined\n                       Ouroboros.Network.Protocol.ChainSync.Test\n                       Ouroboros.Network.Protocol.Handshake.Direct\n                       Ouroboros.Network.Protocol.Handshake.Test\n                       Ouroboros.Network.Protocol.LocalStateQuery.Direct\n                       Ouroboros.Network.Protocol.LocalStateQuery.Test\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Direct\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Examples\n                       Ouroboros.Network.Protocol.LocalTxSubmission.Test\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Direct\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Examples\n                       Ouroboros.Network.Protocol.LocalTxMonitor.Test\n                       Ouroboros.Network.Protocol.TxSubmission2.Direct\n                       Ouroboros.Network.Protocol.TxSubmission2.Test\n                       Ouroboros.Network.Protocol.TxSubmission2.Examples\n                       Ouroboros.Network.Protocol.KeepAlive.Direct\n                       Ouroboros.Network.Protocol.KeepAlive.Examples\n                       Ouroboros.Network.Protocol.KeepAlive.Test\n\n                       Test.ChainGenerators\n                       Test.ChainProducerState\n                       Test.Ouroboros.Network.Testing.Utils\n  build-depends:       base,\n                       bytestring,\n                       cborg,\n                       containers,\n                       hashable,\n                       pipes,\n                       QuickCheck,\n                       quickcheck-instances,\n                       serialise,\n                       strict-containers,\n                       tasty,\n                       tasty-quickcheck,\n                       text,\n\n                       cardano-slotting,\n                       contra-tracer,\n\n                       io-classes,\n                       io-sim,\n                       network-mux,\n                       ouroboros-network,\n                       ouroboros-network-framework,\n                       ouroboros-network-testing,\n                       strict-stm,\n                       typed-protocols\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Ouroboros.Network.BlockFetch.Examples\n                       Ouroboros.Network.MockNode\n\n                       Test.AnchoredFragment\n                       Test.Chain\n                       Test.LedgerPeers\n                       Test.Ouroboros.Network.Diffusion.Node\n                       Test.Ouroboros.Network.Diffusion.Node.NodeKernel\n                       Test.Ouroboros.Network.Diffusion.Node.MiniProtocols\n                       Test.Ouroboros.Network.Diffusion.Policies\n                       Test.Ouroboros.Network.BlockFetch\n                       Test.Ouroboros.Network.KeepAlive\n                       Test.Ouroboros.Network.MockNode\n                       Test.Ouroboros.Network.TxSubmission\n                       Test.Ouroboros.Network.PeerSelection\n                       Test.Ouroboros.Network.PeerSelection.Instances\n                       Test.Ouroboros.Network.PeerSelection.LocalRootPeers\n                       Test.Ouroboros.Network.PeerSelection.RootPeersDNS\n                       Test.Ouroboros.Network.PeerSelection.Json\n                       Test.Ouroboros.Network.PeerSelection.MockEnvironment\n                       Test.Ouroboros.Network.PeerSelection.PeerGraph\n                       Test.Ouroboros.Network.NodeToNode.Version\n                       Test.Ouroboros.Network.NodeToClient.Version\n                       Test.Ouroboros.Network.ShrinkCarefully\n                       Test.Ouroboros.Network.Testnet\n                       Test.Ouroboros.Network.Testnet.Simulation.Node\n                       Test.Mux\n                       Test.Pipe\n                       Test.Socket\n                       Test.PeerState\n                       Test.Version\n  default-language:    Haskell2010\n  build-depends:       base,\n                       QuickCheck,\n                       aeson,\n                       array,\n                       async,\n                       bytestring,\n                       cborg,\n                       containers,\n                       dns,\n                       deque,\n                       hashable,\n                       iproute,\n                       mtl,\n                       network,\n                       pipes,\n                       pretty-simple,\n                       process,\n                       psqueues,\n                       random,\n                       serialise,\n                       tasty,\n                       tasty-hunit,\n                       tasty-quickcheck,\n                       text,\n                       time,\n\n                       cardano-prelude,\n                       cardano-slotting,\n                       contra-tracer,\n                       nothunks,\n\n                       io-classes,\n                       io-sim,\n                       monoidal-synchronisation,\n                       network-mux,\n                       ouroboros-network,\n                       ouroboros-network-framework,\n                       ouroboros-network-framework:testlib,\n                       ouroboros-network-testing,\n                       protocol-tests,\n                       strict-stm,\n                       typed-protocols,\n                       typed-protocols-examples\n\n  if os(windows)\n    build-depends:     Win32-network                 <0.2.0.0,\n                       Win32           >= 2.5.4.1 && <3.0\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -fno-ignore-asserts\n                       -threaded\n                       -rtsopts\n                       +RTS -T -RTS\n  if flag(ipv6)\n    cpp-options:       -DOUROBOROS_NETWORK_IPV6\n\n\ntest-suite cddl\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test-cddl\n  main-is:             Main.hs\n  if flag(cddl)\n    buildable: True\n  else\n    buildable: False\n  default-language:    Haskell2010\n  build-depends:       base,\n                       bytestring,\n                       cborg,\n                       containers,\n                       directory,\n                       filepath,\n                       mtl,\n                       process-extras,\n                       serialise,\n                       text,\n                       temporary,\n\n                       QuickCheck,\n                       quickcheck-instances,\n                       tasty,\n                       tasty-hunit,\n                       tasty-quickcheck,\n\n                       typed-protocols,\n                       ouroboros-network-framework,\n                       ouroboros-network,\n                       protocol-tests\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -fno-ignore-asserts\n\nexecutable demo-chain-sync\n  hs-source-dirs:      demo\n  main-is:             chain-sync.hs\n  build-depends:       base,\n                       async,\n                       bytestring,\n                       containers,\n                       directory,\n                       random,\n                       serialise,\n                       stm,\n\n                       contra-tracer,\n\n                       typed-protocols,\n                       network-mux,\n                       ouroboros-network-framework,\n                       ouroboros-network\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -threaded\n                       -rtsopts\n";
    }