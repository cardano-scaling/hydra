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
      identifier = { name = "moo"; version = "1.2.0.0.0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Sergey Astanin <s.astanin@gmail.com>";
      author = "Sergey Astanin <s.astanin@gmail.com>";
      homepage = "http://www.github.com/astanin/moo/";
      url = "";
      synopsis = "Genetic algorithm library";
      description = "Moo library provides building blocks to build custom\ngenetic algorithms in Haskell. They can be used to\nfind solutions to optimization and search problems.\n\nVariants supported out of the box: binary (using\nbit-strings) and continuous (real-coded).\nPotentially supported variants: permutation,\ntree, hybrid encodings (require customizations).\n\nBinary GAs: binary and Gray encoding; point mutation;\none-point, two-point, and uniform crossover.\nContinuous GAs: Gaussian mutation; BLX-α, UNDX, and\nSBX crossover.\nSelection operators: roulette, tournament, and\nstochastic universal sampling (SUS);\nwith optional niching, ranking, and scaling.\nReplacement strategies: generational with elitism\nand steady state.\nConstrained optimization: random constrained\ninitialization, death penalty, constrained\nselection without a penalty function.\nMulti-objective optimization: NSGA-II\nand constrained NSGA-II.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
          (hsPkgs."mersenne-random-pure64" or (errorHandler.buildDepError "mersenne-random-pure64"))
          (hsPkgs."gray-code" or (errorHandler.buildDepError "gray-code"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "moo-tests" = {
          depends = [
            (hsPkgs."moo" or (errorHandler.buildDepError "moo"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
            (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
            (hsPkgs."mersenne-random-pure64" or (errorHandler.buildDepError "mersenne-random-pure64"))
            (hsPkgs."gray-code" or (errorHandler.buildDepError "gray-code"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/moo-1.2.0.0.0.0.1.tar.gz";
      sha256 = "53703a319e9fec8ea45ed2f2d885426c53577450b58c39f37b248b8efdfa8716";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.8\nname:               moo\nversion:            1.2.0.0.0.0.1\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         Sergey Astanin <s.astanin@gmail.com>\nauthor:             Sergey Astanin <s.astanin@gmail.com>\nstability:          experimental\nhomepage:           http://www.github.com/astanin/moo/\nsynopsis:           Genetic algorithm library\ndescription:\n    Moo library provides building blocks to build custom\n    genetic algorithms in Haskell. They can be used to\n    find solutions to optimization and search problems.\n    .\n    Variants supported out of the box: binary (using\n    bit-strings) and continuous (real-coded).\n    Potentially supported variants: permutation,\n    tree, hybrid encodings (require customizations).\n    .\n    Binary GAs: binary and Gray encoding; point mutation;\n    one-point, two-point, and uniform crossover.\n    Continuous GAs: Gaussian mutation; BLX-α, UNDX, and\n    SBX crossover.\n    Selection operators: roulette, tournament, and\n    stochastic universal sampling (SUS);\n    with optional niching, ranking, and scaling.\n    Replacement strategies: generational with elitism\n    and steady state.\n    Constrained optimization: random constrained\n    initialization, death penalty, constrained\n    selection without a penalty function.\n    Multi-objective optimization: NSGA-II\n    and constrained NSGA-II.\n\ncategory:           AI, Algorithms, Optimisation, Optimization\nbuild-type:         Simple\nextra-source-files:\n    README.md\n    examples/README.md\n    examples/ExampleMain.hs\n    examples/beale.hs\n    examples/cp_himmelblau.hs\n    examples/cp_sphere2.hs\n    examples/knapsack.hs\n    examples/mop_constr2.hs\n    examples/mop_kursawe.hs\n    examples/mop_minsum_maxprod.hs\n    examples/rosenbrock.hs\n    examples/schaffer2.hs\n\nsource-repository head\n    type:     git\n    location: git://github.com/astanin/moo.git\n\nlibrary\n    exposed-modules:\n        Moo.GeneticAlgorithm\n        Moo.GeneticAlgorithm.Binary\n        Moo.GeneticAlgorithm.Constraints\n        Moo.GeneticAlgorithm.Continuous\n        Moo.GeneticAlgorithm.Multiobjective\n        Moo.GeneticAlgorithm.Random\n        Moo.GeneticAlgorithm.Run\n        Moo.GeneticAlgorithm.Statistics\n        Moo.GeneticAlgorithm.Types\n\n    other-modules:\n        Moo.GeneticAlgorithm.Crossover\n        Moo.GeneticAlgorithm.LinAlg\n        Moo.GeneticAlgorithm.Multiobjective.NSGA2\n        Moo.GeneticAlgorithm.Multiobjective.Types\n        Moo.GeneticAlgorithm.Multiobjective.Metrics\n        Moo.GeneticAlgorithm.Selection\n        Moo.GeneticAlgorithm.StopCondition\n        Moo.GeneticAlgorithm.Utilities\n        Moo.GeneticAlgorithm.Niching\n\n    ghc-options:     -Wall -fno-warn-name-shadowing -fno-warn-orphans\n    build-depends:\n        base >=4 && <5,\n        MonadRandom,\n        mersenne-random-pure64,\n        gray-code >=0.2.1,\n        random >=0.1,\n        random-shuffle >=0.0.2,\n        mtl >=2,\n        time,\n        array,\n        parallel >=3.0,\n        vector,\n        containers\n\ntest-suite moo-tests\n    type:          exitcode-stdio-1.0\n    main-is:       moo-tests.hs\n    other-modules:\n        Tests.Common\n        Tests.Internals.TestControl\n        Tests.Internals.TestCrossover\n        Tests.Internals.TestFundamentals\n        Tests.Internals.TestMultiobjective\n        Tests.Internals.TestSelection\n        Tests.Internals.TestConstraints\n        Tests.Problems.Rosenbrock\n        Moo.GeneticAlgorithm\n        Moo.GeneticAlgorithm.Binary\n        Moo.GeneticAlgorithm.Constraints\n        Moo.GeneticAlgorithm.Continuous\n        Moo.GeneticAlgorithm.Crossover\n        Moo.GeneticAlgorithm.Niching\n        Moo.GeneticAlgorithm.Run\n        Moo.GeneticAlgorithm.Random\n        Moo.GeneticAlgorithm.Utilities\n        Moo.GeneticAlgorithm.LinAlg\n        Moo.GeneticAlgorithm.Multiobjective\n        Moo.GeneticAlgorithm.Multiobjective.NSGA2\n        Moo.GeneticAlgorithm.Multiobjective.Types\n        Moo.GeneticAlgorithm.Multiobjective.Metrics\n        Moo.GeneticAlgorithm.Selection\n        Moo.GeneticAlgorithm.Statistics\n        Moo.GeneticAlgorithm.StopCondition\n        Moo.GeneticAlgorithm.Types\n\n    build-depends:\n        moo,\n        base <5,\n        HUnit,\n        random >=0.1,\n        random-shuffle >=0.0.2,\n        MonadRandom,\n        mersenne-random-pure64,\n        gray-code >=0.2.1,\n        mtl,\n        time,\n        array,\n        containers,\n        parallel >=3.0,\n        vector,\n        containers\n";
    }