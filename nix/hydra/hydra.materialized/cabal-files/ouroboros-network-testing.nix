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
    flags = { nightly = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-network-testing"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knuttson";
      homepage = "";
      url = "";
      synopsis = "Common modules used for testing in ouroboros-network and ouroboros-consensus";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deque" or (errorHandler.buildDepError "deque"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-network-testing-0.1.0.1.tar.gz";
      sha256 = "b064eaed175f0aa84fc9ede76c6691c0877f98eae2b03155f3864950b30c7e91";
      });
    }) // {
    package-description-override = "name:                ouroboros-network-testing\nversion:             0.1.0.1\nsynopsis:            Common modules used for testing in ouroboros-network and ouroboros-consensus\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knuttson\nmaintainer:\ncategory:            Network\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\ncabal-version:       >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n\nflag nightly\n  Description: Enable nightly tests\n  Manual:      False\n  Default:     False\n\nlibrary\n  hs-source-dirs:      src\n\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:\n                       Ouroboros.Network.Testing.Serialise\n                       Ouroboros.Network.Testing.QuickCheck\n                       Ouroboros.Network.Testing.Utils\n                       Ouroboros.Network.Testing.Data.Signal\n                       Ouroboros.Network.Testing.Data.Script\n                       Ouroboros.Network.Testing.Data.AbsBearerInfo\n  default-language:    Haskell2010\n  other-extensions:    BangPatterns,\n                       DataKinds,\n                       EmptyCase,\n                       ExistentialQuantification,\n                       FlexibleContexts,\n                       FlexibleInstances,\n                       FunctionalDependencies,\n                       GADTs,\n                       GADTSyntax,\n                       GeneralizedNewtypeDeriving,\n                       MultiParamTypeClasses,\n                       NamedFieldPuns,\n                       OverloadedStrings,\n                       PolyKinds,\n                       RankNTypes,\n                       RecordWildCards,\n                       ScopedTypeVariables,\n                       TemplateHaskell,\n                       TupleSections,\n                       TypeApplications,\n                       TypeFamilies,\n                       TypeInType\n  build-depends:       base              >=4.9 && <4.15,\n                       containers,\n                       contra-tracer,\n                       deque,\n                       io-classes,\n                       io-sim,\n                       psqueues          >=0.2.3 && <0.3,\n                       tasty,\n                       tasty-expected-failure,\n\n                       cborg             >=0.2.1 && <0.3,\n                       serialise         >=0.2 && <0.3,\n                       network-mux,\n                       QuickCheck\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -fno-ignore-asserts\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n\n  if flag(nightly)\n    cpp-options:       -DNIGHTLY\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  main-is:             Main.hs\n  hs-source-dirs:      test\n  other-modules:       Test.Ouroboros.Network.Testing.Data.AbsBearerInfo\n\n  build-depends:       base\n\n                     , QuickCheck\n                     , tasty\n                     , tasty-quickcheck\n\n                     , ouroboros-network-testing\n\n  default-language:    Haskell2010\n  ghc-options:         -rtsopts\n                       -threaded\n                       -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wno-unticked-promoted-constructors\n";
    }