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
      specVersion = "1.10";
      identifier = { name = "quickcheck-state-machine"; version = "0.7.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2017-2018, ATS Advanced Telematic Systems GmbH;\n2018-2019, HERE Europe B.V.;\n2019-2021, Stevan Andjelkovic.";
      maintainer = "Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>";
      author = "Stevan Andjelkovic";
      homepage = "https://github.com/stevana/quickcheck-state-machine#readme";
      url = "";
      synopsis = "Test monadic programs using state machine based models";
      description = "See README at <https://github.com/stevana/quickcheck-state-machine#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
          (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
          (hsPkgs."markov-chain-usage-model" or (errorHandler.buildDepError "markov-chain-usage-model"))
          (hsPkgs."matrix" or (errorHandler.buildDepError "matrix"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
        buildable = true;
        };
      tests = {
        "quickcheck-state-machine-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."filelock" or (errorHandler.buildDepError "filelock"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hashtables" or (errorHandler.buildDepError "hashtables"))
            (hsPkgs."hs-rqlite" or (errorHandler.buildDepError "hs-rqlite"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."persistent" or (errorHandler.buildDepError "persistent"))
            (hsPkgs."persistent-postgresql" or (errorHandler.buildDepError "persistent-postgresql"))
            (hsPkgs."persistent-sqlite" or (errorHandler.buildDepError "persistent-sqlite"))
            (hsPkgs."persistent-template" or (errorHandler.buildDepError "persistent-template"))
            (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
            (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-state-machine-0.7.1.tar.gz";
      sha256 = "dfed2c4d24fcf4596adc2ef16bcd8777dd3aaae81d4f0d5aac70ade9801fd268";
      });
    }) // {
    package-description-override = "name:                quickcheck-state-machine\nversion:             0.7.1\nsynopsis:            Test monadic programs using state machine based models\ndescription:         See README at <https://github.com/stevana/quickcheck-state-machine#readme>\nhomepage:            https://github.com/stevana/quickcheck-state-machine#readme\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Stevan Andjelkovic\nmaintainer:          Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>\ncopyright:           Copyright (C) 2017-2018, ATS Advanced Telematic Systems GmbH;\n                                   2018-2019, HERE Europe B.V.;\n                                   2019-2021, Stevan Andjelkovic.\ncategory:            Testing\nbuild-type:          Simple\nextra-source-files:  README.md\n                   , CHANGELOG.md\n                   , CONTRIBUTING.md\ncabal-version:       >=1.10\ntested-with:         GHC == 8.4.3, GHC == 8.6.5, GHC == 8.8.3\n\nlibrary\n  hs-source-dirs:      src\n  ghc-options:\n              -Weverything\n              -Wno-missing-exported-signatures\n              -Wno-missing-import-lists\n              -Wno-missed-specialisations\n              -Wno-all-missed-specialisations\n              -Wno-unsafe\n              -Wno-safe\n              -Wno-missing-local-signatures\n              -Wno-monomorphism-restriction\n  exposed-modules:     Test.StateMachine\n                     , Test.StateMachine.BoxDrawer\n                     , Test.StateMachine.ConstructorName\n                     , Test.StateMachine.DotDrawing\n                     , Test.StateMachine.Labelling\n                     , Test.StateMachine.Lockstep.Auxiliary\n                     , Test.StateMachine.Lockstep.NAry\n                     , Test.StateMachine.Lockstep.Simple\n                     , Test.StateMachine.Logic\n                     , Test.StateMachine.Markov\n                     , Test.StateMachine.Parallel\n                     , Test.StateMachine.Sequential\n                     , Test.StateMachine.Types\n                     , Test.StateMachine.Types.Environment\n                     , Test.StateMachine.Types.GenSym\n                     , Test.StateMachine.Types.History\n                     , Test.StateMachine.Types.Rank2\n                     , Test.StateMachine.Types.References\n                     , Test.StateMachine.Utils\n                     , Test.StateMachine.Z\n  other-modules:\n      Paths_quickcheck_state_machine\n  build-depends:\n        ansi-wl-pprint >=0.6.7.3,\n        base >=4.10 && <5,\n        containers >=0.5.7.1,\n        directory >=1.0.0.0,\n        exceptions >=0.8.3,\n        filepath >=1.0,\n        generic-data >=0.3.0.0,\n        graphviz >= 2999.20.0.3,\n        markov-chain-usage-model >=0.0.0,\n        matrix >=0.3.5.0,\n        mtl >=2.2.1,\n        time >=1.7,\n        pretty-show >=1.6.16,\n        process >=1.2.0.0,\n        QuickCheck >=2.12,\n        random >=1.1,\n        sop-core,\n        split,\n        text,\n        tree-diff >=0.0.2.1,\n        unliftio >=0.2.7.0\n  default-language:    Haskell2010\n\ntest-suite quickcheck-state-machine-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  build-depends:       aeson,\n                       array,\n                       base,\n                       bifunctors,\n                       bytestring,\n                       containers,\n                       directory,\n                       doctest,\n                       filelock,\n                       filepath,\n                       hashable,\n                       hashtables,\n                       hs-rqlite >= 0.1.2.0,\n                       HTTP,\n                       http-client,\n                       monad-logger,\n                       mtl,\n                       network,\n                       persistent >= 2.10.3,\n                       persistent-postgresql,\n                       persistent-sqlite,\n                       persistent-template,\n                       postgresql-simple,\n                       pretty-show,\n                       process,\n                       QuickCheck,\n                       quickcheck-instances,\n                       quickcheck-state-machine,\n                       random,\n                       resourcet,\n                       resource-pool,\n                       servant,\n                       servant-client,\n                       servant-server,\n                       split,\n                       stm,\n                       strict,\n                       string-conversions,\n                       tasty,\n                       tasty-hunit,\n                       tasty-quickcheck,\n                       text,\n                       tree-diff,\n                       time,\n                       vector >=0.12.0.1,\n                       wai,\n                       warp,\n                       unliftio,\n                       unliftio-core\n\n  other-modules:       Bookstore,\n                       CircularBuffer,\n                       Cleanup,\n                       CrudWebserverDb,\n                       DieHard,\n                       Echo,\n                       ErrorEncountered,\n                       Hanoi,\n                       IORefs,\n                       MemoryReference,\n                       Mock,\n                       Overflow,\n                       ProcessRegistry,\n                       Schema,\n                       RQlite,\n                       ShrinkingProps,\n                       SQLite,\n                       TicketDispenser,\n                       UnionFind\n\n  ghc-options:\n              -threaded -rtsopts -with-rtsopts=-N\n              -fno-ignore-asserts\n              -Weverything\n              -Wno-missing-exported-signatures\n              -Wno-missing-import-lists\n              -Wno-missed-specialisations\n              -Wno-all-missed-specialisations\n              -Wno-unsafe\n              -Wno-safe\n              -Wno-missing-local-signatures\n              -Wno-monomorphism-restriction\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/stevana/quickcheck-state-machine\n";
    }