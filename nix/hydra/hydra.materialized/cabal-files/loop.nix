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
      specVersion = "1.8";
      identifier = { name = "loop"; version = "0.3.0"; };
      license = "MIT";
      copyright = "2014 Niklas Hambüchen <mail@nh2.me>";
      maintainer = "Niklas Hambüchen <mail@nh2.me>";
      author = "Niklas Hambüchen <mail@nh2.me>";
      homepage = "https://github.com/nh2/loop";
      url = "";
      synopsis = "Fast loops (for when GHC can't optimize forM_)";
      description = "This package provides a convenient and fast alternative to the common\n`forM_ [1..n]` idiom, which in many cases GHC cannot fuse to efficient\ncode.\n\nSee <https://ghc.haskell.org/trac/ghc/ticket/8763>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."loop" or (errorHandler.buildDepError "loop"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."loop" or (errorHandler.buildDepError "loop"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            ];
          buildable = true;
          };
        "bench-folds" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."loop" or (errorHandler.buildDepError "loop"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        "bench-traverse-w32" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."loop" or (errorHandler.buildDepError "loop"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "bench-foldl-and-iorefs-are-slow" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/loop-0.3.0.tar.gz";
      sha256 = "92962010bdab28cc0092dd3fe42819d6f215c717dd10d9349626d92a0d0b3ecf";
      });
    }) // {
    package-description-override = "name:          loop\nversion:       0.3.0\nlicense:       MIT\ncopyright:     2014 Niklas Hambüchen <mail@nh2.me>\nauthor:        Niklas Hambüchen <mail@nh2.me>\nmaintainer:    Niklas Hambüchen <mail@nh2.me>\ncategory:      Control\nbuild-type:    Simple\nstability:     experimental\ntested-with:   GHC==7.6.3\ncabal-version: >= 1.8\nhomepage:      https://github.com/nh2/loop\nbug-reports:   https://github.com/nh2/loop/issues\nsynopsis:      Fast loops (for when GHC can't optimize forM_)\ndescription:\n  This package provides a convenient and fast alternative to the common\n  `forM_ [1..n]` idiom, which in many cases GHC cannot fuse to efficient\n  code.\n  .\n  See <https://ghc.haskell.org/trac/ghc/ticket/8763>.\n\nsource-repository head\n  type:      git\n  location:  git://github.com/nh2/loop.git\n\n\nlibrary\n  exposed-modules:\n    Control.Loop\n    -- Internal\n    Control.Loop.Internal\n  hs-source-dirs:\n    src\n  build-depends:\n      base                >= 4 && < 5\n  ghc-options:\n    -Wall -O2\n\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs:\n    test\n  main-is:\n    Main.hs\n  build-depends:\n      base                >= 4 && < 5\n    , loop\n    , hspec               >= 1.3.0.1\n    , mtl                 >= 2.1.2\n  ghc-options:\n    -Wall -O2\n\n\nbenchmark bench\n  type: exitcode-stdio-1.0\n  hs-source-dirs:\n    bench\n  main-is:\n    Bench.hs\n  build-depends:\n      base                >= 4 && < 5\n    , loop\n    , criterion           >= 0.6.0.0\n    , random              >= 1.0.1.1\n    , vector              >= 0.10.9.1\n    , foldl               >= 1.0.0\n  ghc-options:\n    -Wall -O2\n\n\nbenchmark bench-folds\n  type: exitcode-stdio-1.0\n  hs-source-dirs:\n    bench\n  main-is:\n    BenchFolds.hs\n  build-depends:\n      base                >= 4 && < 5\n    , loop\n    , criterion           >= 0.6.0.0\n    , mtl                 >= 2.1.2\n  ghc-options:\n    -Wall -O2\n\n\nbenchmark bench-traverse-w32\n  type: exitcode-stdio-1.0\n  hs-source-dirs:\n    bench\n  main-is:\n    TraverseW32.hs\n  build-depends:\n      base                >= 4 && < 5\n    , loop\n    , criterion           >= 0.6.0.0\n    , vector              >= 0.10.9.1\n  ghc-options:\n    -Wall -O2\n\n\nbenchmark bench-foldl-and-iorefs-are-slow\n  type: exitcode-stdio-1.0\n  hs-source-dirs:\n    bench\n  main-is:\n    FoldlAndIORefAreSlow.hs\n  build-depends:\n      base                >= 4.6 && < 5\n    , criterion           >= 0.6.0.0\n    , mtl                 >= 2.1.2\n    , vector              >= 0.10.9.1\n  ghc-options:\n    -Wall -O2\n";
    }