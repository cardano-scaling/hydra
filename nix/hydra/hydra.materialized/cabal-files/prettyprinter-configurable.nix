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
      specVersion = "2.4";
      identifier = {
        name = "prettyprinter-configurable";
        version = "0.1.0.0";
        };
      license = "NONE";
      copyright = "";
      maintainer = "plutus@iohk.io";
      author = "David Luposchainsky, effectfully";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          ];
        buildable = true;
        };
      tests = {
        "prettyprinter-configurable-test" = {
          depends = [
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        "prettyprinter-configurable-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/prettyprinter-configurable-0.1.0.0.tar.gz";
      sha256 = "17c21e9e8fd392d853f6f0e2cfc317ae532a6e94600143a37b426b4c2a9453e7";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nname:                prettyprinter-configurable\nversion:             0.1.0.0\n-- synopsis:\n-- description:\n-- homepage:            https://github.com/githubuser/prettyprinter-configurable#readme\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              David Luposchainsky, effectfully\nmaintainer:          plutus@iohk.io\ncategory:            User Interfaces, Text\nbuild-type:          Custom\nextra-source-files:  README.md\n\ncommon lang\n    default-language: Haskell2010\n    default-extensions: ExplicitForAll FlexibleContexts ScopedTypeVariables\n                        DeriveGeneric StandaloneDeriving DeriveLift\n                        GeneralizedNewtypeDeriving DeriveFunctor DeriveFoldable\n                        DeriveTraversable DerivingStrategies DerivingVia\n                        ImportQualifiedPost\n    ghc-options: -Wall -Wnoncanonical-monad-instances\n                 -Wincomplete-uni-patterns -Wincomplete-record-updates\n                 -Wredundant-constraints -Widentities -Wunused-packages\n                 -Wmissing-deriving-strategies\n\ncustom-setup\n    setup-depends:\n        base,\n        Cabal,\n        cabal-doctest\n\nlibrary\n    import: lang\n    hs-source-dirs: src\n    exposed-modules:\n        Text.Fixity\n        Text.Fixity.Internal\n        Text.Pretty\n        Text.PrettyBy\n        Text.PrettyBy.Default\n        Text.PrettyBy.Fixity\n        Text.PrettyBy.Internal\n        Text.PrettyBy.Internal.Utils\n        Text.PrettyBy.Monad\n    build-depends:\n        base,\n        prettyprinter,\n        mtl,\n        text,\n        microlens\n    ghc-options: -O2\n\ntest-suite prettyprinter-configurable-test\n    import: lang\n    type: exitcode-stdio-1.0\n    main-is: Main.hs\n    hs-source-dirs: test\n    other-modules:\n        Default\n        NonDefault\n        Universal\n        Expr\n    build-depends:\n        prettyprinter-configurable,\n        base,\n        prettyprinter,\n        text,\n        megaparsec,\n        parser-combinators,\n        QuickCheck,\n        quickcheck-text,\n        tasty,\n        tasty-hunit,\n        tasty-quickcheck\n    ghc-options:\n        -threaded -rtsopts -with-rtsopts=-N\n\ntest-suite prettyprinter-configurable-doctest\n    import: lang\n    type: exitcode-stdio-1.0\n    main-is: Main.hs\n    hs-source-dirs: doctest\n    build-depends:\n        base,\n        doctest\n    ghc-options:\n        -threaded -rtsopts -with-rtsopts=-N\n";
    }