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
    flags = { checktvarinvariant = false; asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "io-classes"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Type classes for concurrency with STM, ST and timing";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/io-classes-0.2.0.0.tar.gz";
      sha256 = "cdd6e29f505ed180c83ad85ee90fc63268d5579786b8c5ae2b0ca0866157df08";
      });
    }) // {
    package-description-override = "name:                io-classes\nversion:             0.2.0.0\nsynopsis:            Type classes for concurrency with STM, ST and timing\n-- description:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer:\ncategory:            Control\nbuild-type:          Simple\ncabal-version:       >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n  subdir:   io-classes\n\nflag checktvarinvariant\n  Description: Enable runtime invariant checks on StrictT(M)Var\n  Manual: True\n  Default: False\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:\n                       Control.Monad.Class.MonadAsync\n                       Control.Monad.Class.MonadEventlog\n                       Control.Monad.Class.MonadFork\n                       Control.Monad.Class.MonadSay\n                       Control.Monad.Class.MonadST\n                       Control.Monad.Class.MonadSTM\n                       Control.Monad.Class.MonadThrow\n                       Control.Monad.Class.MonadTime\n                       Control.Monad.Class.MonadTimer\n                       Control.Monad.Class.MonadTest\n  default-language:    Haskell2010\n  other-extensions:    CPP\n                       TypeFamilies\n                       TypeFamilyDependencies\n                       MultiParamTypeClasses\n                       FunctionalDependencies\n                       FlexibleInstances\n                       FlexibleContexts\n                       ScopedTypeVariables\n                       RankNTypes\n  build-depends:       base  >=4.9 && <4.15,\n                       async >=2.1,\n                       bytestring,\n                       mtl   >=2.2 && <2.3,\n                       stm   >=2.5 && <2.6,\n                       time  >=1.9.1 && <1.11\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.MonadTimer\n  default-language:    Haskell2010\n  build-depends:       base,\n                       io-classes,\n\n                       QuickCheck,\n                       tasty,\n                       tasty-quickcheck\n";
    }