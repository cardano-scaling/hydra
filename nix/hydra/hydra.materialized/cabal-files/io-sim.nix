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
    flags = { asserts = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "io-sim"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2020 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Duncan Coutts, Marcin Szamotulski, Alexander Vieth";
      homepage = "";
      url = "";
      synopsis = "A pure simulator for monadic concurrency with STM";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deque" or (errorHandler.buildDepError "deque"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/io-sim-0.2.0.0.tar.gz";
      sha256 = "ee89de7e30b170a74a641e2261ba94c40830582ff9ba1cd211cc18260474f31f";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nname:                io-sim\nversion:             0.2.0.0\nsynopsis:            A pure simulator for monadic concurrency with STM\n-- description:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019-2020 Input Output (Hong Kong) Ltd.\nauthor:              Duncan Coutts, Marcin Szamotulski, Alexander Vieth\nmaintainer:\ncategory:            Testing\nbuild-type:          Simple\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n  subdir:   io-sim\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Data.List.Trace,\n                       Control.Monad.IOSim,\n                       Control.Monad.IOSim.Types\n  other-modules:       Control.Monad.IOSim.CommonTypes,\n                       Control.Monad.IOSim.Internal,\n                       Control.Monad.IOSim.InternalTypes,\n                       Control.Monad.IOSim.STM,\n                       Control.Monad.IOSimPOR.Internal,\n                       Control.Monad.IOSimPOR.Types,\n                       Control.Monad.IOSimPOR.QuickCheckUtils,\n                       Control.Monad.IOSimPOR.Timeout\n  default-language:    Haskell2010\n  other-extensions:    BangPatterns,\n                       CPP,\n                       ExistentialQuantification,\n                       FlexibleInstances,\n                       GADTSyntax,\n                       GeneralizedNewtypeDeriving,\n                       MultiParamTypeClasses,\n                       NamedFieldPuns,\n                       RankNTypes,\n                       ScopedTypeVariables,\n                       TypeFamilies\n  build-depends:       base              >=4.9 && <4.15,\n                       io-classes        >=0.2 && <0.3,\n                       exceptions        >=0.10,\n                       containers,\n                       deque,\n                       parallel,\n                       pretty-simple,\n                       psqueues          >=0.2 && <0.3,\n                       text,\n                       time              >=1.9.1 && <1.11,\n                       quiet,\n                       QuickCheck,\n                       syb\n\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n\n  if flag(asserts)\n     ghc-options:      -fno-ignore-asserts\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.IOSim\n                       Test.STM\n                       Test.Control.Monad.IOSimPOR\n  default-language:    Haskell2010\n  build-depends:       base,\n                       array,\n                       containers,\n                       io-classes,\n                       io-sim,\n                       parallel,\n                       QuickCheck,\n                       strict-stm,\n                       tasty,\n                       tasty-quickcheck,\n                       time               >= 1.9.1\n\n  ghc-options:         -Wall\n                       -fno-ignore-asserts\n\nbenchmark bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Main.hs\n  default-language:    Haskell2010\n  build-depends:       base,\n                       criterion,\n\n                       contra-tracer,\n                       io-classes,\n                       io-sim,\n                       typed-protocols,\n                       typed-protocols-cborg,\n                       typed-protocols-examples\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n";
    }