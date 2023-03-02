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
    flags = { containers042 = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "fgl"; version = "5.8.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ivan.Miljenovic@gmail.com";
      author = "Martin Erwig, Ivan Lazar Miljenovic";
      homepage = "";
      url = "";
      synopsis = "Martin Erwig's Functional Graph Library";
      description = "An inductive representation of manipulating graph data structures.\n\nOriginal website can be found at <http://web.engr.oregonstate.edu/~erwig/fgl/haskell>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ] ++ (if flags.containers042
          then [
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ]
          else [
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "fgl-tests" = {
          depends = [
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "fgl-benchmark" = {
          depends = [
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."microbench" or (errorHandler.buildDepError "microbench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = if flags.containers042 then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fgl-5.8.0.0.tar.gz";
      sha256 = "d60ec09472f9bac0e54a3b55a274b5872ad28a048d5230e006a28c5adc8b8d09";
      });
    }) // {
    package-description-override = "name:          fgl\nversion:       5.8.0.0\nlicense:       BSD3\nlicense-file:  LICENSE\nauthor:        Martin Erwig, Ivan Lazar Miljenovic\nmaintainer:    Ivan.Miljenovic@gmail.com\ncategory:      Data Structures, Graphs\nsynopsis:      Martin Erwig's Functional Graph Library\n\ndescription:   {\nAn inductive representation of manipulating graph data structures.\n.\nOriginal website can be found at <http://web.engr.oregonstate.edu/~erwig/fgl/haskell>.\n}\ncabal-version: >= 1.10\nbuild-type:    Simple\nextra-source-files:\n               ChangeLog\n\ntested-with:   GHC == 7.2.2,  GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4,\n               GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.3,\n               GHC == 8.6.2,  GHC == 8.8.2, GHC == 8.10.7, GHC == 9.0.2,\n               GHC == 9.2.3, GHC == 9.4.2\n\nsource-repository head\n    type:         git\n    location:     https://github.com/haskell/fgl.git\n\nflag containers042 {\n    manual:  False\n    default: True\n}\n\nlibrary {\n    default-language: Haskell98\n\n    exposed-modules:\n        Data.Graph.Inductive.Internal.Heap,\n        Data.Graph.Inductive.Internal.Queue,\n        Data.Graph.Inductive.Internal.RootPath,\n        Data.Graph.Inductive.Internal.Thread,\n        Data.Graph.Inductive.Basic,\n        Data.Graph.Inductive.Example,\n        Data.Graph.Inductive.Graph,\n        Data.Graph.Inductive.Monad,\n        Data.Graph.Inductive.NodeMap,\n        Data.Graph.Inductive.PatriciaTree,\n        Data.Graph.Inductive.Query,\n        Data.Graph.Inductive.Tree,\n        Data.Graph.Inductive.Monad.IOArray,\n        Data.Graph.Inductive.Monad.STArray,\n        Data.Graph.Inductive.Query.ArtPoint,\n        Data.Graph.Inductive.Query.BCC,\n        Data.Graph.Inductive.Query.BFS,\n        Data.Graph.Inductive.Query.DFS,\n        Data.Graph.Inductive.Query.Dominators,\n        Data.Graph.Inductive.Query.GVD,\n        Data.Graph.Inductive.Query.Indep,\n        Data.Graph.Inductive.Query.MST,\n        Data.Graph.Inductive.Query.MaxFlow,\n        Data.Graph.Inductive.Query.MaxFlow2,\n        Data.Graph.Inductive.Query.Monad,\n        Data.Graph.Inductive.Query.SP,\n        Data.Graph.Inductive.Query.TransClos,\n        Data.Graph.Inductive\n\n    other-modules:\n        Paths_fgl\n\n    build-depends:    base >= 4.3 && < 5\n                    , transformers\n                    , array\n\n    if flag(containers042)\n        build-depends:    containers >= 0.4.2\n                        , deepseq >= 1.1.0.0 && < 1.5\n    else\n        build-depends:    containers < 0.4.2\n\n    if impl(ghc >= 7.2) && impl(ghc < 7.6)\n        build-depends:\n            ghc-prim\n\n    ghc-options:      -Wall\n\n}\n\ntest-suite fgl-tests {\n    default-language: Haskell98\n\n    type:             exitcode-stdio-1.0\n\n    build-depends:    fgl\n                    , base\n                    , QuickCheck >= 2.8 && < 2.15\n                    , hspec >= 2.1 && < 2.11\n                    , containers\n\n    hs-source-dirs:   test\n                      fgl-arbitrary\n\n    main-is:          TestSuite.hs\n\n    other-modules:    Data.Graph.Inductive.Arbitrary\n                    , Data.Graph.Inductive.Graph.Properties\n                    , Data.Graph.Inductive.Proxy\n                    , Data.Graph.Inductive.Query.Properties\n\n    ghc-options:      -Wall\n\n}\n\nbenchmark fgl-benchmark {\n    if flag(containers042)\n        buildable:    True\n    else\n        buildable:    False\n\n    default-language: Haskell98\n\n    type:             exitcode-stdio-1.0\n\n    hs-source-dirs:   test\n\n    main-is:          benchmark.hs\n\n    other-modules:    Data.Graph.Inductive.Proxy\n\n    build-depends:    fgl\n                    , base\n                    , microbench\n                    , deepseq\n\n    ghc-options:      -Wall\n\n}\n";
    }