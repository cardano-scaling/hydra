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
    flags = { asserts = false; ipv6 = false; tracetcpinfo = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "network-mux"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "duncan@well-typed.com, marcin.szamotulski@iohk.io, marc.fontaine@iohk.io, karl.knutsson@iohk.io, alex@well-typed.com, neil.davies@pnsol.com";
      author = "Duncan Coutts, Marc Fontaine, Karl Knutsson, Marcin Szamotulski, Alexander Vieth, Neil Davies";
      homepage = "";
      url = "";
      synopsis = "Multiplexing library";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."statistics-linreg" or (errorHandler.buildDepError "statistics-linreg"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ];
        buildable = true;
        };
      exes = {
        "mux-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            ] ++ (if system.isWindows
            then [
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
              (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
              ]
            else [
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ]);
          buildable = true;
          };
        "cardano-ping" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."tdigest" or (errorHandler.buildDepError "tdigest"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if system.isWindows then false else true;
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/network-mux-0.1.0.1.tar.gz";
      sha256 = "54f392a1d84e40f406ab2e8e8788da4352448a92498f0c194696c96481578abe";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\nname:                network-mux\nversion:             0.1.0.1\nsynopsis:            Multiplexing library\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Duncan Coutts, Marc Fontaine, Karl Knutsson, Marcin Szamotulski, Alexander Vieth, Neil Davies\nmaintainer:          duncan@well-typed.com, marcin.szamotulski@iohk.io, marc.fontaine@iohk.io, karl.knutsson@iohk.io, alex@well-typed.com, neil.davies@pnsol.com\ncategory:            Network\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nFlag ipv6\n  Description: Enable IPv6 test cases\n  Manual: True\n  -- Default to False since travis lacks IPv6 support\n  Default: False\n\nFlag tracetcpinfo\n  Description: Enable costly Linux only tracing of the kernel's tcpinfo\n  Manual: True\n  Default: False\n\ncommon demo-deps\n  default-language:    Haskell2010\n  ghc-options:         -threaded\n                       -Wall\n                       -fno-ignore-asserts\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n\nlibrary\n  build-depends:       base            >=4.9 && <4.15,\n                       io-classes      >=0.1 && <0.3,\n                       strict-stm      >=0.1 && <0.2,\n                       contra-tracer   >=0.1 && <0.2,\n                       monoidal-synchronisation\n                                       >=0.1 && <0.2,\n\n                       array           >=0.5 && <0.6,\n                       binary          >=0.8 && <0.11,\n                       bytestring      >=0.10 && <0.11,\n                       containers      >=0.5 && <0.7,\n                       -- The Windows version of network-3.1.2 is missing\n                       -- functions, see\n                       -- https://github.com/haskell/network/issues/484\n                       network         >=3.1.2 && <3.2,\n                       process         >=1.6 && <1.7,\n                       statistics-linreg\n                                       >=0.3 && <0.4,\n                       vector          >=0.12 && <0.13,\n                       time            >=1.9.1 && <1.11,\n                       quiet\n\n  if os(windows)\n    build-depends:     Win32           >= 2.5.4.1 && <3.0,\n                       Win32-network   >=0.1 && <0.2\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n  if flag(asserts)\n     ghc-options:      -fno-ignore-asserts\n\n  if flag(tracetcpinfo)\n    cpp-options:       -DMUX_TRACE_TCPINFO\n  hs-source-dirs:      src\n  exposed-modules:     Network.Mux\n                       Network.Mux.Channel\n                       Network.Mux.Codec\n                       Network.Mux.Compat\n                       Network.Mux.Egress\n                       Network.Mux.Ingress\n                       Network.Mux.Time\n                       Network.Mux.Timeout\n                       Network.Mux.Types\n                       Network.Mux.Trace\n                       Network.Mux.Bearer.AttenuatedChannel\n                       Network.Mux.Bearer.Pipe\n                       Network.Mux.Bearer.Queues\n                       Network.Mux.Bearer.Socket\n                       Network.Mux.DeltaQ.TraceStats\n                       Network.Mux.DeltaQ.TraceStatsSupport\n                       Network.Mux.DeltaQ.TraceTransformer\n                       Network.Mux.DeltaQ.TraceTypes\n                       Network.Mux.TCPInfo\n                       Control.Concurrent.JobPool\n\n  if os(linux)\n    other-modules:     Network.Mux.TCPInfo.Linux\n\n  if os(windows)\n    exposed-modules:\n                       Network.Mux.Bearer.NamedPipe\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.Mux\n                       Test.Mux.ReqResp\n                       Test.Mux.Timeout\n  default-language:    Haskell2010\n  build-depends:       base,\n                       io-classes,\n                       strict-stm,\n                       io-sim            >=0.2 && < 0.3,\n                       contra-tracer,\n                       network-mux,\n                       Win32-network,\n\n                       binary,\n                       bytestring,\n                       cborg,\n                       containers,\n                       network,\n                       process,\n                       QuickCheck,\n                       splitmix,\n                       serialise,\n                       tasty,\n                       tasty-quickcheck,\n                       time\n\n  if os(windows)\n    build-depends:     Win32           >= 2.5.4.1 && <3.0,\n  ghc-options:         -threaded\n                       -Wall\n                       -fno-ignore-asserts\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n  if flag(ipv6)\n    cpp-options:       -DOUROBOROS_NETWORK_IPV6\n\nexecutable mux-demo\n  import:              demo-deps\n  hs-source-dirs:      demo, test\n  main-is:             mux-demo.hs\n  other-modules:       Test.Mux.ReqResp\n  build-depends:       base,\n                       directory,\n                       network-mux,\n                       io-classes,\n                       contra-tracer,\n                       stm,\n\n                       bytestring,\n                       cborg,\n                       serialise\n  if os(windows)\n    build-depends:     Win32,\n                       Win32-network\n  else\n    build-depends:     network\n\nexecutable cardano-ping\n  import:              demo-deps\n  hs-source-dirs:      tools\n  main-is:             cardano-ping.hs\n  other-modules:       Linger\n  build-depends:       base,\n                       aeson,\n                       network-mux,\n                       io-classes,\n                       strict-stm,\n                       contra-tracer,\n\n                       bytestring,\n                       cborg,\n                       network,\n                       tdigest,\n                       text\n  if os(windows)\n    buildable:         False\n  else\n    buildable:         True\n";
    }