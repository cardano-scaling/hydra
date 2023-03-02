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
      identifier = { name = "psqueues"; version = "0.2.7.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jasper Van der Jeugt <jaspervdj@gmail.com>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Pure priority search queues";
      description = "The psqueues package provides\n<http://en.wikipedia.org/wiki/Priority_queue Priority Search Queues> in\nthree different flavors.\n\n* @OrdPSQ k p v@, which uses the @Ord k@ instance to provide fast insertion,\ndeletion and lookup. This implementation is based on Ralf Hinze's\n<http://citeseer.ist.psu.edu/hinze01simple.html A Simple Implementation Technique for Priority Search Queues>.\nHence, it is similar to the\n<http://hackage.haskell.org/package/PSQueue PSQueue> library, although it is\nconsiderably faster and provides a slightly different API.\n\n* @IntPSQ p v@ is a far more efficient implementation. It fixes the key type\nto @Int@ and uses a <http://en.wikipedia.org/wiki/Radix_tree radix tree>\n(like @IntMap@) with an additional min-heap property.\n\n* @HashPSQ k p v@ is a fairly straightforward extension of @IntPSQ@: it\nsimply uses the keys' hashes as indices in the @IntPSQ@. If there are any\nhash collisions, it uses an @OrdPSQ@ to resolve those. The performance of\nthis implementation is comparable to that of @IntPSQ@, but it is more widely\napplicable since the keys are not restricted to @Int@, but rather to any\n@Hashable@ datatype.\n\nEach of the three implementations provides the same API, so they can be used\ninterchangeably. The benchmarks show how they perform relative to one\nanother, and also compared to the other Priority Search Queue\nimplementations on Hackage:\n<http://hackage.haskell.org/package/PSQueue PSQueue>\nand\n<http://hackage.haskell.org/package/fingertree-psqueue fingertree-psqueue>.\n\n<<http://i.imgur.com/KmbDKR6.png>>\n\n<<http://i.imgur.com/ClT181D.png>>\n\nTypical applications of Priority Search Queues include:\n\n* Caches, and more specifically LRU Caches;\n\n* Schedulers;\n\n* Pathfinding algorithms, such as Dijkstra's and A*.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "6.10") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "psqueues-tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "psqueues-benchmarks" = {
          depends = [
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."fingertree-psqueue" or (errorHandler.buildDepError "fingertree-psqueue"))
            (hsPkgs."PSQueue" or (errorHandler.buildDepError "PSQueue"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/psqueues-0.2.7.3.tar.gz";
      sha256 = "d09750ba3578d905b54d0b3a60a7b468910a60b3165e5de98bf6f4efae3ebfb2";
      });
    }) // {
    package-description-override = "Name:          psqueues\nVersion:       0.2.7.3\nLicense:       BSD3\nLicense-file:  LICENSE\nMaintainer:    Jasper Van der Jeugt <jaspervdj@gmail.com>\nBug-reports:   https://github.com/jaspervdj/psqueues/issues\nSynopsis:      Pure priority search queues\nCategory:      Data Structures\nBuild-type:    Simple\nCabal-version: >=1.10\n\nDescription:\n    The psqueues package provides\n    <http://en.wikipedia.org/wiki/Priority_queue Priority Search Queues> in\n    three different flavors.\n    .\n    * @OrdPSQ k p v@, which uses the @Ord k@ instance to provide fast insertion,\n    deletion and lookup. This implementation is based on Ralf Hinze's\n    <http://citeseer.ist.psu.edu/hinze01simple.html A Simple Implementation Technique for Priority Search Queues>.\n    Hence, it is similar to the\n    <http://hackage.haskell.org/package/PSQueue PSQueue> library, although it is\n    considerably faster and provides a slightly different API.\n    .\n    * @IntPSQ p v@ is a far more efficient implementation. It fixes the key type\n    to @Int@ and uses a <http://en.wikipedia.org/wiki/Radix_tree radix tree>\n    (like @IntMap@) with an additional min-heap property.\n    .\n    * @HashPSQ k p v@ is a fairly straightforward extension of @IntPSQ@: it\n    simply uses the keys' hashes as indices in the @IntPSQ@. If there are any\n    hash collisions, it uses an @OrdPSQ@ to resolve those. The performance of\n    this implementation is comparable to that of @IntPSQ@, but it is more widely\n    applicable since the keys are not restricted to @Int@, but rather to any\n    @Hashable@ datatype.\n    .\n    Each of the three implementations provides the same API, so they can be used\n    interchangeably. The benchmarks show how they perform relative to one\n    another, and also compared to the other Priority Search Queue\n    implementations on Hackage:\n    <http://hackage.haskell.org/package/PSQueue PSQueue>\n    and\n    <http://hackage.haskell.org/package/fingertree-psqueue fingertree-psqueue>.\n    .\n    <<http://i.imgur.com/KmbDKR6.png>>\n    .\n    <<http://i.imgur.com/ClT181D.png>>\n    .\n    Typical applications of Priority Search Queues include:\n    .\n    * Caches, and more specifically LRU Caches;\n    .\n    * Schedulers;\n    .\n    * Pathfinding algorithms, such as Dijkstra's and A*.\n\nExtra-source-files:\n    CHANGELOG\n\nSource-repository head\n    type:     git\n    location: http://github.com/jaspervdj/psqueues.git\n\nLibrary\n    Default-language: Haskell2010\n    Ghc-options:      -O2 -Wall\n    Hs-source-dirs:   src\n\n    Build-depends:\n          base     >= 4.2     && < 5\n        , deepseq  >= 1.2     && < 1.5\n        , hashable >= 1.1.2.3 && < 1.5\n\n    if impl(ghc>=6.10)\n        Build-depends: ghc-prim\n\n    Exposed-modules:\n        Data.HashPSQ\n        Data.IntPSQ\n        Data.OrdPSQ\n    Other-modules:\n        Data.BitUtil\n        Data.HashPSQ.Internal\n        Data.IntPSQ.Internal\n        Data.OrdPSQ.Internal\n\nBenchmark psqueues-benchmarks\n    Default-language: Haskell2010\n    Ghc-options:      -Wall\n    Hs-source-dirs:   src benchmarks\n    Main-is:          Main.hs\n    Type:             exitcode-stdio-1.0\n\n    Other-modules:\n        BenchmarkTypes\n        Data.BitUtil\n        Data.FingerTree.PSQueue.Benchmark\n        Data.HashPSQ\n        Data.HashPSQ.Benchmark\n        Data.HashPSQ.Internal\n        Data.IntPSQ\n        Data.IntPSQ.Benchmark\n        Data.IntPSQ.Internal\n        Data.OrdPSQ\n        Data.OrdPSQ.Benchmark\n        Data.OrdPSQ.Internal\n        Data.PSQueue.Benchmark\n\n    Build-depends:\n          containers           >= 0.5\n        , unordered-containers >= 0.2.4\n        , criterion            >= 0.8\n        , mtl                  >= 2.1\n        , fingertree-psqueue   >= 0.3\n        , PSQueue              >= 1.1\n        , random               >= 1.0\n\n        , base\n        , deepseq\n        , ghc-prim\n        , hashable\n        , psqueues\n\nTest-suite psqueues-tests\n    Cpp-options:      -DTESTING -DSTRICT\n    Default-language: Haskell2010\n    Ghc-options:      -Wall\n    Hs-source-dirs:   src tests\n    Main-is:          Main.hs\n    Type:             exitcode-stdio-1.0\n\n    Other-modules:\n        Data.BitUtil\n        Data.HashPSQ\n        Data.HashPSQ.Internal\n        Data.HashPSQ.Tests\n        Data.IntPSQ\n        Data.IntPSQ.Internal\n        Data.IntPSQ.Tests\n        Data.OrdPSQ\n        Data.OrdPSQ.Internal\n        Data.OrdPSQ.Tests\n        Data.PSQ.Class\n        Data.PSQ.Class.Gen\n        Data.PSQ.Class.Tests\n        Data.PSQ.Class.Util\n\n    Build-depends:\n          HUnit            >= 1.2 && < 1.7\n        , QuickCheck       >= 2.7 && < 2.15\n        , tasty            >= 1.2 && < 1.5\n        , tasty-hunit      >= 0.9 && < 0.11\n        , tasty-quickcheck >= 0.8 && < 0.11\n\n        , base\n        , array\n        , deepseq\n        , ghc-prim\n        , hashable\n        , psqueues\n        , tagged\n";
    }