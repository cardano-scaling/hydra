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
      identifier = {
        name = "ouroboros-network-framework";
        version = "0.1.0.1";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            ];
          buildable = true;
          };
        };
      exes = {
        "demo-ping-pong" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          };
        "demo-connection-manager" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ];
          buildable = true;
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-framework".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-framework:testlib"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-network-framework-0.1.0.1.tar.gz";
      sha256 = "054ddf47403058dfea73a6b11051f62d76c135fd074682af6d3bf8dc6de657fb";
      });
    }) // {
    package-description-override = "cabal-version:       3.0\n-- Initial package description 'typed-protocols-testing.cabal' generated by\n--  'cabal init'.  For further documentation, see\n-- http://haskell.org/cabal/users-guide/\n\nname:                ouroboros-network-framework\nversion:             0.1.0.1\n-- synopsis:\n-- description:\n-- bug-reports:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Duncan Coutts, Marcin Szamotulski\nmaintainer:          alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io\n-- category:\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nlibrary\n  exposed-modules:     Data.Cache\n                       Data.Wedge\n                       Ouroboros.Network.CodecCBORTerm\n                       Ouroboros.Network.Channel\n                       Ouroboros.Network.Driver\n                       Ouroboros.Network.Driver.Simple\n                       Ouroboros.Network.Driver.Limits\n                       Ouroboros.Network.ErrorPolicy\n                       Ouroboros.Network.IOManager\n                       Ouroboros.Network.Mux\n                       Ouroboros.Network.MuxMode\n                       Ouroboros.Network.Util.ShowProxy\n\n                       Ouroboros.Network.Protocol.Handshake\n                       Ouroboros.Network.Protocol.Handshake.Type\n                       Ouroboros.Network.Protocol.Handshake.Codec\n                       Ouroboros.Network.Protocol.Handshake.Client\n                       Ouroboros.Network.Protocol.Handshake.Server\n                       Ouroboros.Network.Protocol.Handshake.Version\n                       Ouroboros.Network.Protocol.Handshake.Unversioned\n                       Ouroboros.Network.Protocol.Limits\n\n                       Ouroboros.Network.ConnectionId\n                       Ouroboros.Network.ConnectionHandler\n                       Ouroboros.Network.ConnectionManager.Types\n                       Ouroboros.Network.ConnectionManager.Core\n                       Ouroboros.Network.InboundGovernor\n                       Ouroboros.Network.InboundGovernor.Event\n                       Ouroboros.Network.InboundGovernor.State\n                       Ouroboros.Network.InboundGovernor.ControlChannel\n                       Ouroboros.Network.RethrowPolicy\n                       Ouroboros.Network.Server.ConnectionTable\n                       Ouroboros.Network.Server.Socket\n                       Ouroboros.Network.Server.RateLimiting\n                       Ouroboros.Network.Server2\n                       Ouroboros.Network.Snocket\n                       Ouroboros.Network.Socket\n\n                       Ouroboros.Network.Subscription\n                       Ouroboros.Network.Subscription.Client\n                       Ouroboros.Network.Subscription.Dns\n                       Ouroboros.Network.Subscription.Ip\n                       Ouroboros.Network.Subscription.PeerState\n                       Ouroboros.Network.Subscription.Subscriber\n                       Ouroboros.Network.Subscription.Worker\n\n                       Simulation.Network.Snocket\n\n  other-modules:       Ouroboros.Network.Linger\n  -- other-extensions:\n  build-depends:       base            >=4.12  && <4.15\n                     , async           >=2.1   && <2.3\n                     , bytestring      >=0.10  && <0.11\n                     , cborg           >=0.2.1 && <0.3\n                     , containers      >=0.5   && <0.7\n                     , dns                        < 4.0\n                     , iproute         >=1.7   && < 1.8\n                     , hashable\n                     , mtl\n                     , nothunks\n                     , random\n                     , stm\n                     , text\n                     , time\n                     , quiet\n\n                     , cardano-prelude\n                     , contra-tracer\n\n                     , io-classes      >=0.1   && < 0.3\n                     , monoidal-synchronisation\n                                       >=0.1   && < 0.2\n                     , network         >=3.1.2 && < 3.2\n                     , network-mux     >=0.1   && < 0.2\n                     , ouroboros-network-testing\n                     , strict-stm      >=0.1   && < 0.2\n                     , typed-protocols >=0.1   && < 0.2\n                     , typed-protocols-cborg\n                                       >=0.1   && < 0.2\n                     , Win32-network   >=0.1   && < 0.2\n\n  if os(windows)\n    build-depends:     Win32           >= 2.5.4.1 && <3.0\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n\nlibrary testlib\n    visibility: public\n    hs-source-dirs: testlib\n\n    exposed-modules: TestLib.ConnectionManager\n                     TestLib.InboundGovernor\n                     TestLib.Utils\n\n    other-modules:\n\n    build-depends:   base\n                   , bytestring\n                   , containers\n\n                   , QuickCheck\n\n                   , io-sim\n                   , io-classes\n                   , typed-protocols\n                   , ouroboros-network-framework\n\n  default-language:  Haskell2010\n  ghc-options:       -rtsopts\n                     -threaded\n                     -Wall\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  main-is:             Main.hs\n  hs-source-dirs:      test\n  other-modules:       Test.Ouroboros.Network.ConnectionManager\n                       Test.Ouroboros.Network.Driver\n                       Test.Ouroboros.Network.Orphans\n                       Test.Ouroboros.Network.Server2\n                       Test.Ouroboros.Network.Socket\n                       Test.Ouroboros.Network.Subscription\n                       Test.Ouroboros.Network.RateLimiting\n                       Test.Simulation.Network.Snocket\n\n  build-depends:       base\n                     , bytestring\n                     , cborg\n                     , containers\n                     , directory\n                     , dns\n                     , iproute\n                     , network\n                     , pretty-simple\n                     , serialise\n                     , text\n                     , time\n                     , quiet\n\n                     , QuickCheck\n                     , quickcheck-instances\n                     , tasty\n                     , tasty-quickcheck\n                     , these\n\n                     , cardano-prelude\n                     , contra-tracer\n\n                     , io-sim\n                     , io-classes\n                     , strict-stm\n                     , network-mux\n                     , monoidal-synchronisation\n                     , ouroboros-network-framework\n                     , ouroboros-network-framework:testlib\n                     , ouroboros-network-testing\n                     , typed-protocols\n                     , typed-protocols-cborg\n                     , typed-protocols-examples\n\n  if os(windows)\n    build-depends:     Win32-network                <0.2,\n                       Win32           >=2.5.4.1 && <3.0\n\n  default-language:    Haskell2010\n  ghc-options:         -rtsopts\n                       -threaded\n                       -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n\nexecutable demo-ping-pong\n  hs-source-dirs:      demo test\n  main-is:             ping-pong.hs\n  build-depends:       base,\n                       async,\n                       bytestring,\n                       cborg,\n                       directory,\n\n                       contra-tracer,\n\n                       io-classes,\n                       network-mux,\n                       ouroboros-network-framework,\n                       typed-protocols,\n                       typed-protocols-examples\n\n  if os(windows)\n    build-depends:     Win32-network                <0.2,\n                       Win32           >=2.5.4.1 && <3.0\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -threaded\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n\nexecutable demo-connection-manager\n  hs-source-dirs:      demo test\n  main-is:             connection-manager.hs\n  build-depends:       base,\n                       bytestring,\n                       cborg,\n                       network,\n                       optparse-applicative,\n                       random,\n                       serialise,\n\n                       contra-tracer,\n\n                       io-classes,\n                       network-mux,\n                       ouroboros-network-framework,\n                       strict-stm,\n                       typed-protocols,\n                       typed-protocols-examples\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -threaded\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n";
    }