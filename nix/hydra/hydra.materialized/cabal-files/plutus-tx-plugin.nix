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
    flags = { use-ghc-stub = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-tx-plugin"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "The Plutus Tx compiler and GHC plugin";
      description = "The Plutus Tx compiler and GHC plugin.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ] ++ (if flags.use-ghc-stub
          then [
            (hsPkgs."plutus-ghc-stub" or (errorHandler.buildDepError "plutus-ghc-stub"))
            ]
          else [ (hsPkgs."ghc" or (errorHandler.buildDepError "ghc")) ]);
        buildable = true;
        };
      tests = {
        "plutus-tx-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = if flags.use-ghc-stub then false else true;
          };
        "size" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/plutus-tx-plugin-1.0.0.0.tar.gz";
      sha256 = "387cc2a0e0394c414cd2ad7fc6909dfaeaf6847a25c3e071e97eb6f10a6259ce";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\nname: plutus-tx-plugin\nversion: 1.0.0.0\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nmaintainer: michael.peyton-jones@iohk.io\nauthor: Michael Peyton Jones\nsynopsis: The Plutus Tx compiler and GHC plugin\ndescription:\n    The Plutus Tx compiler and GHC plugin.\ncategory: Language\nbuild-type: Simple\nextra-doc-files: README.md\n\nsource-repository head\n    type: git\n    location: https://github.com/input-output-hk/plutus\n\ncommon lang\n    default-language: Haskell2010\n    default-extensions: ExplicitForAll ScopedTypeVariables\n                        DeriveGeneric StandaloneDeriving DeriveLift\n                        GeneralizedNewtypeDeriving DeriveFunctor DeriveFoldable\n                        DeriveTraversable DerivingStrategies\n                        ImportQualifiedPost\n    ghc-options: -Wall -Wnoncanonical-monad-instances\n                 -Wincomplete-uni-patterns -Wincomplete-record-updates\n                 -Wredundant-constraints -Widentities -Wunused-packages\n                 -Wmissing-deriving-strategies\n                 -- See Plutus Tx readme\n                 -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\nflag use-ghc-stub\n    description:\n        Use the `plutus-ghc-stub` package instead of `ghc`.\n    default: False\n    manual: True\n\nlibrary\n    import: lang\n    hs-source-dirs: src\n    exposed-modules:\n        PlutusTx.Plugin\n        PlutusTx.Compiler.Error\n    other-modules:\n        PlutusTx.Compiler.Binders\n        PlutusTx.Compiler.Builtins\n        PlutusTx.Compiler.Expr\n        PlutusTx.Compiler.Kind\n        PlutusTx.Compiler.Laziness\n        PlutusTx.Compiler.Names\n        PlutusTx.Compiler.Type\n        PlutusTx.Compiler.Types\n        PlutusTx.Compiler.Utils\n        PlutusTx.PIRTypes\n        PlutusTx.PLCTypes\n    build-depends:\n        base >=4.9 && <5,\n        bytestring -any,\n        containers -any,\n        extra -any,\n        flat -any,\n        ghc-prim -any,\n        plutus-core ^>=1.0,\n        lens -any,\n        mtl -any,\n        prettyprinter -any,\n        template-haskell -any,\n        text -any,\n        transformers -any,\n        plutus-tx ^>=1.0,\n        array -any\n    if flag(use-ghc-stub)\n        build-depends: plutus-ghc-stub\n        ghc-options: -Wno-unused-packages -Wno-unused-imports -Wno-overlapping-patterns\n    else\n        build-depends: ghc >=8.10 && <9\n\ntest-suite plutus-tx-tests\n    import: lang\n    if flag(use-ghc-stub)\n        buildable: False\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test\n    main-is: Spec.hs\n    other-modules:\n        Budget.Spec\n        IsData.Spec\n        Lift.Spec\n        Optimization.Spec\n        Plugin.Spec\n        Plugin.Basic.Spec\n        Plugin.Data.Spec\n        Plugin.Errors.Spec\n        Plugin.Functions.Spec\n        Plugin.Laziness.Spec\n        Plugin.NoTrace.Spec\n        Plugin.Primitives.Spec\n        Plugin.Profiling.Spec\n        Plugin.Typeclasses.Spec\n        Plugin.Typeclasses.Lib\n        Plugin.Coverage.Spec\n        Plugin.Strict.Spec\n        Plugin.Lib\n        StdLib.Spec\n        TH.Spec\n        TH.TestTH\n        Lib\n    build-depends:\n        base >=4.9 && <5,\n        flat -any,\n        deepseq -any,\n        integer-gmp -any,\n        plutus-core ^>=1.0,\n        plutus-core:plutus-core-testlib -any,\n        plutus-tx ^>=1.0,\n        plutus-tx:plutus-tx-testlib -any,\n        plutus-tx-plugin ^>=1.0,\n        prettyprinter -any,\n        mtl -any,\n        template-haskell -any,\n        tasty -any,\n        tasty-hedgehog -any,\n        tasty-hunit -any,\n        text -any,\n        hedgehog -any,\n        lens -any,\n        ghc-prim -any,\n        containers -any\n    -- NOTE: -g makes the plugin give better errors\n    ghc-options: -g\n\ntest-suite size\n    import: lang\n    -- needs plutus-tx-plugin but it looks unused\n    ghc-options: -Wno-unused-packages\n    type: exitcode-stdio-1.0\n    main-is: Main.hs\n    hs-source-dirs: test/size\n    build-depends:\n        base >= 4.9 && < 5.0,\n        plutus-tx:plutus-tx-testlib -any,\n        plutus-tx ^>=1.0,\n        plutus-tx-plugin ^>=1.0,\n        tagged -any,\n        tasty -any\n";
    }