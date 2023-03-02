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
    flags = { enable-inspect = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "generic-data"; version = "1.0.0.0"; };
      license = "MIT";
      copyright = "2018-2020 Li-yao Xia";
      maintainer = "lysxia@gmail.com";
      author = "Li-yao Xia";
      homepage = "https://github.com/Lysxia/generic-data#readme";
      url = "";
      synopsis = "Deriving instances with GHC.Generics and related utilities";
      description = "Generic implementations of standard type classes.\nOperations on generic representations to help using \"GHC.Generics\".\nSee README.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ap-normalize" or (errorHandler.buildDepError "ap-normalize"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."show-combinators" or (errorHandler.buildDepError "show-combinators"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        };
      tests = {
        "unit-test" = {
          depends = [
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."show-combinators" or (errorHandler.buildDepError "show-combinators"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        "record-test" = {
          depends = [
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        "example-test" = {
          depends = [
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        "microsurgery-test" = {
          depends = [
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        "lens-surgery-test" = {
          depends = [
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        "one-liner-surgery-test" = {
          depends = [
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          };
        "inspect" = {
          depends = (pkgs.lib).optionals (!(!flags.enable-inspect)) [
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = if !flags.enable-inspect then false else true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = if !(compiler.isGhc && (compiler.version).ge "8.6")
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generic-data-1.0.0.0.tar.gz";
      sha256 = "0fa021b8d0d879d9f1d81c792bf596ccc88ebdaf94dc0c1d1d3cadcf47eacab1";
      });
    }) // {
    package-description-override = "name:                generic-data\nversion:             1.0.0.0\nsynopsis:            Deriving instances with GHC.Generics and related utilities\ndescription:\n  Generic implementations of standard type classes.\n  Operations on generic representations to help using \"GHC.Generics\".\n  See README.\nhomepage:            https://github.com/Lysxia/generic-data#readme\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Li-yao Xia\nmaintainer:          lysxia@gmail.com\ncopyright:           2018-2020 Li-yao Xia\ncategory:            Generics\nbuild-type:          Simple\nextra-source-files:  README.md, CHANGELOG.md\ncabal-version:       >=1.10\ntested-with:         GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.3,\n                     GHC == 8.6.5, GHC == 8.8.2, GHC == 9.0.2,\n                     GHC == 9.2.4, GHC == 9.4.1\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:\n    Generic.Data\n    Generic.Data.Types\n    Generic.Data.Microsurgery\n    Generic.Data.Internal.Compat\n    Generic.Data.Internal.Data\n    Generic.Data.Internal.Enum\n    Generic.Data.Internal.Error\n    Generic.Data.Internal.Functions\n    Generic.Data.Internal.Generically\n    Generic.Data.Internal.Meta\n    Generic.Data.Internal.Microsurgery\n    Generic.Data.Internal.Newtype\n    Generic.Data.Internal.Prelude\n    Generic.Data.Internal.Read\n    Generic.Data.Internal.Resolvers\n    Generic.Data.Internal.Show\n    Generic.Data.Internal.Traversable\n    Generic.Data.Internal.Utils\n  build-depends:\n    ap-normalize >= 0.1 && < 0.2,\n    base-orphans >= 0.8,\n    contravariant,\n    ghc-boot-th,\n    show-combinators,\n    base >= 4.9 && < 5\n  hs-source-dirs:      orphans\n  exposed-modules:\n    Generic.Data.Orphans\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n\ntest-suite unit-test\n  hs-source-dirs: test\n  main-is: unit.hs\n  build-depends:\n    tasty,\n    tasty-hunit,\n    generic-data,\n    show-combinators >= 0.2,\n    base\n  ghc-options: -Wall\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n\ntest-suite record-test\n  hs-source-dirs: test\n  main-is: record.hs\n  build-depends:\n    generic-data,\n    base\n  ghc-options: -Wall\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n\ntest-suite example-test\n  hs-source-dirs: test\n  main-is: example.hs\n  build-depends:\n    generic-data,\n    base\n  ghc-options: -Wall\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n\ntest-suite microsurgery-test\n  hs-source-dirs: test\n  main-is: microsurgery.hs\n  build-depends:\n    tasty,\n    tasty-hunit,\n    generic-data,\n    base\n  ghc-options: -Wall\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n\ntest-suite lens-surgery-test\n  hs-source-dirs: test\n  main-is: lens-surgery.hs\n  build-depends:\n    tasty,\n    tasty-hunit,\n    generic-data,\n    generic-lens >= 1.1.0.0,\n    base\n  ghc-options: -Wall\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n\ntest-suite one-liner-surgery-test\n  hs-source-dirs: test\n  main-is: one-liner-surgery.hs\n  build-depends:\n    tasty,\n    tasty-hunit,\n    generic-data,\n    generic-lens >= 1.1.0.0,\n    one-liner >= 1.0,\n    base\n  ghc-options: -Wall -threaded\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n\ntest-suite inspect\n  hs-source-dirs: test\n  main-is: inspection.hs\n  other-modules:\n    Inspection.Boilerplate\n  ghc-options: -Wall -threaded\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n  if !flag(enable-inspect)\n    buildable: False\n  else\n    build-depends:\n      generic-data,\n      inspection-testing,\n      template-haskell,\n      unordered-containers,\n      base\n\nbenchmark bench\n  hs-source-dirs: test\n  main-is: bench.hs\n  build-depends:\n    criterion,\n    deepseq,\n    generic-data,\n    base\n  ghc-options: -Wall\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n  if !impl(ghc >= 8.6)\n    buildable: False\n\nflag enable-inspect\n  description: Enable inspection tests (broken on ghc < 8.2 or >= 9.2)\n  default: False\n  manual: True\n\nsource-repository head\n  type:     git\n  location: https://github.com/Lysxia/generic-data\n";
    }