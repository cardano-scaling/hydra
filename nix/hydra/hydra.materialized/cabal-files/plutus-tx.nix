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
      identifier = { name = "plutus-tx"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "Libraries for Plutus Tx and its prelude";
      description = "Libraries for Plutus Tx and its prelude";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      sublibs = {
        "plutus-tx-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "plutus-tx-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-fn" or (errorHandler.buildDepError "hedgehog-fn"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest.components.exes.doctest or (pkgs.buildPackages.doctest or (errorHandler.buildToolDepError "doctest:doctest")))
            ];
          buildable = if compiler.isGhcjs && true || system.isWindows
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/plutus-tx-1.0.0.0.tar.gz";
      sha256 = "e65642af9f8ea7752bcb0e072724ce57fa6a50d8c81f2d92ff124ff11f1a8c08";
      });
    }) // {
    package-description-override = "cabal-version:   3.0\nname:            plutus-tx\nversion:         1.0.0.0\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:      michael.peyton-jones@iohk.io\nauthor:          Michael Peyton Jones\nsynopsis:        Libraries for Plutus Tx and its prelude\ndescription:     Libraries for Plutus Tx and its prelude\ncategory:        Language\nbuild-type:      Simple\nextra-doc-files: README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/plutus\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    NoImplicitPrelude\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    ExplicitForAll\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n    -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\n-- See Plutus Tx readme\nlibrary\n  import:          lang\n  hs-source-dirs:  src\n  exposed-modules:\n    PlutusTx\n    PlutusTx.Applicative\n    PlutusTx.AssocMap\n    PlutusTx.Base\n    PlutusTx.Bool\n    PlutusTx.Builtins\n    PlutusTx.Builtins.Class\n    PlutusTx.Builtins.Internal\n    PlutusTx.Code\n    PlutusTx.Coverage\n    PlutusTx.Either\n    PlutusTx.Enum\n    PlutusTx.Eq\n    PlutusTx.ErrorCodes\n    PlutusTx.Foldable\n    PlutusTx.Functor\n    PlutusTx.Integer\n    PlutusTx.IsData\n    PlutusTx.IsData.Class\n    PlutusTx.Lattice\n    PlutusTx.Lift\n    PlutusTx.Lift.Class\n    PlutusTx.List\n    PlutusTx.Maybe\n    PlutusTx.Monoid\n    PlutusTx.Numeric\n    PlutusTx.Ord\n    PlutusTx.Plugin.Utils\n    PlutusTx.Prelude\n    PlutusTx.Ratio\n    PlutusTx.Semigroup\n    PlutusTx.Sqrt\n    PlutusTx.TH\n    PlutusTx.These\n    PlutusTx.Trace\n    PlutusTx.Traversable\n    PlutusTx.Utils\n\n  other-modules:\n    PlutusTx.IsData.Instances\n    PlutusTx.IsData.TH\n    PlutusTx.Lift.Instances\n    PlutusTx.Lift.THUtils\n\n  build-depends:\n    , aeson\n    , base              >=4.9      && <5\n    , bytestring\n    , containers\n    , deepseq\n    , flat\n    , ghc-prim\n    , hashable\n    , lens\n    , memory\n    , mtl\n    , plutus-core       ^>=1.0\n    , prettyprinter\n    , serialise\n    , template-haskell  >=2.13.0.0\n    , text\n    , th-abstraction\n    , th-compat\n    , transformers\n\nlibrary plutus-tx-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules: PlutusTx.Test\n  build-depends:\n    , base           >=4.9 && <5\n    , filepath\n    , flat\n    , ghc-prim\n    , lens\n    , mtl\n    , plutus-core\n    , plutus-tx\n    , plutus-core:plutus-core-testlib ^>=1.0\n    , prettyprinter\n    , tagged\n    , tasty\n    , text\n\ntest-suite plutus-tx-test\n  import:             lang\n\n  if (impl(ghcjs) || os(windows))\n    buildable: False\n\n  type:               exitcode-stdio-1.0\n  main-is:            Spec.hs\n  other-modules:\n    Suites.Laws\n    Suites.Laws.Additive\n    Suites.Laws.Construction\n    Suites.Laws.Eq\n    Suites.Laws.Helpers\n    Suites.Laws.Module\n    Suites.Laws.Multiplicative\n    Suites.Laws.Ord\n    Suites.Laws.Other\n    Suites.Laws.Ring\n    Suites.Laws.Serialization\n\n  hs-source-dirs:     test\n  build-tool-depends: doctest:doctest -any\n  build-depends:\n    , aeson\n    , base            >=4.9 && <5\n    , bytestring\n    , cborg\n    , hedgehog\n    , hedgehog-fn\n    , plutus-core     ^>=1.0\n    , plutus-tx\n    , pretty-show\n    , serialise\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n";
    }