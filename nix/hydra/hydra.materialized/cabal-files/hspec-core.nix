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
      specVersion = "1.12";
      identifier = { name = "hspec-core"; version = "2.10.6"; };
      license = "MIT";
      copyright = "(c) 2011-2022 Simon Hengel,\n(c) 2011-2012 Trystan Spangler,\n(c) 2011 Greg Weber";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "";
      homepage = "http://hspec.github.io/";
      url = "";
      synopsis = "A Testing Framework for Haskell";
      description = "This package exposes internal types and functions that can be used to extend Hspec's functionality.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
          (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "8.2.1") [
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4.1") (hsPkgs."stm" or (errorHandler.buildDepError "stm"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = ([
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."hspec-meta" or (errorHandler.buildDepError "hspec-meta"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "8.2.1") [
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4.1") (hsPkgs."stm" or (errorHandler.buildDepError "stm"));
          build-tools = [
            (hsPkgs.buildPackages.hspec-meta.components.exes.hspec-meta-discover or (pkgs.buildPackages.hspec-meta-discover or (errorHandler.buildToolDepError "hspec-meta:hspec-meta-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-core-2.10.6.tar.gz";
      sha256 = "95efece0ef1e3874711ea6aa378b54dfd06e4b9d757cf8027121c9d90715e0b3";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.35.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:             hspec-core\nversion:          2.10.6\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2011-2022 Simon Hengel,\n                  (c) 2011-2012 Trystan Spangler,\n                  (c) 2011 Greg Weber\nmaintainer:       Simon Hengel <sol@typeful.net>\nbuild-type:       Simple\nextra-source-files:\n    version.yaml\n    help.txt\ncategory:         Testing\nstability:        experimental\nbug-reports:      https://github.com/hspec/hspec/issues\nhomepage:         http://hspec.github.io/\nsynopsis:         A Testing Framework for Haskell\ndescription:      This package exposes internal types and functions that can be used to extend Hspec's functionality.\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/hspec\n  subdir: hspec-core\n\nlibrary\n  hs-source-dirs:\n      src\n      vendor\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\n  build-depends:\n      HUnit ==1.6.*\n    , QuickCheck >=2.13.1\n    , ansi-terminal >=0.6.2\n    , array\n    , base >=4.5.0.0 && <5\n    , call-stack >=0.2.0\n    , deepseq\n    , directory\n    , filepath\n    , hspec-expectations ==0.8.2.*\n    , process\n    , quickcheck-io >=0.2.0\n    , random\n    , setenv\n    , tf-random\n    , time\n    , transformers >=0.2.2.0\n  exposed-modules:\n      Test.Hspec.Core.Spec\n      Test.Hspec.Core.Hooks\n      Test.Hspec.Core.Runner\n      Test.Hspec.Core.Format\n      Test.Hspec.Core.Formatters\n      Test.Hspec.Core.Formatters.V1\n      Test.Hspec.Core.Formatters.V2\n      Test.Hspec.Core.QuickCheck\n      Test.Hspec.Core.Util\n  other-modules:\n      GetOpt.Declarative\n      GetOpt.Declarative.Environment\n      GetOpt.Declarative.Interpret\n      GetOpt.Declarative.Types\n      GetOpt.Declarative.Util\n      NonEmpty\n      Test.Hspec.Core.Clock\n      Test.Hspec.Core.Compat\n      Test.Hspec.Core.Config\n      Test.Hspec.Core.Config.Definition\n      Test.Hspec.Core.Config.Options\n      Test.Hspec.Core.Example\n      Test.Hspec.Core.Example.Location\n      Test.Hspec.Core.FailureReport\n      Test.Hspec.Core.Formatters.Diff\n      Test.Hspec.Core.Formatters.Internal\n      Test.Hspec.Core.Formatters.Pretty\n      Test.Hspec.Core.Formatters.Pretty.Parser\n      Test.Hspec.Core.Formatters.Pretty.Parser.Types\n      Test.Hspec.Core.Formatters.Pretty.Unicode\n      Test.Hspec.Core.Formatters.V1.Free\n      Test.Hspec.Core.Formatters.V1.Monad\n      Test.Hspec.Core.QuickCheckUtil\n      Test.Hspec.Core.Runner.Eval\n      Test.Hspec.Core.Runner.PrintSlowSpecItems\n      Test.Hspec.Core.Runner.Result\n      Test.Hspec.Core.Shuffle\n      Test.Hspec.Core.Spec.Monad\n      Test.Hspec.Core.Timer\n      Test.Hspec.Core.Tree\n      Control.Concurrent.Async\n      Data.Algorithm.Diff\n      Paths_hspec_core\n  default-language: Haskell2010\n  if impl(ghc >= 8.2.1)\n    build-depends:\n        ghc\n      , ghc-boot-th\n  if impl(ghc >= 8.4.1)\n    build-depends:\n        stm >=2.2\n  else\n    other-modules:\n        Control.Concurrent.STM.TMVar\n    hs-source-dirs:\n        vendor/stm-2.5.0.1/\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      src\n      vendor\n      test\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\n  cpp-options: -DTEST\n  build-depends:\n      HUnit ==1.6.*\n    , QuickCheck >=2.14\n    , ansi-terminal >=0.6.2\n    , array\n    , base >=4.5.0.0 && <5\n    , base-orphans\n    , call-stack >=0.2.0\n    , deepseq\n    , directory\n    , filepath\n    , hspec-expectations ==0.8.2.*\n    , hspec-meta ==2.10.5\n    , process\n    , quickcheck-io >=0.2.0\n    , random\n    , setenv\n    , silently >=1.2.4\n    , temporary\n    , tf-random\n    , time\n    , transformers >=0.2.2.0\n  build-tool-depends:\n      hspec-meta:hspec-meta-discover\n  other-modules:\n      GetOpt.Declarative\n      GetOpt.Declarative.Environment\n      GetOpt.Declarative.Interpret\n      GetOpt.Declarative.Types\n      GetOpt.Declarative.Util\n      NonEmpty\n      Test.Hspec.Core.Clock\n      Test.Hspec.Core.Compat\n      Test.Hspec.Core.Config\n      Test.Hspec.Core.Config.Definition\n      Test.Hspec.Core.Config.Options\n      Test.Hspec.Core.Example\n      Test.Hspec.Core.Example.Location\n      Test.Hspec.Core.FailureReport\n      Test.Hspec.Core.Format\n      Test.Hspec.Core.Formatters\n      Test.Hspec.Core.Formatters.Diff\n      Test.Hspec.Core.Formatters.Internal\n      Test.Hspec.Core.Formatters.Pretty\n      Test.Hspec.Core.Formatters.Pretty.Parser\n      Test.Hspec.Core.Formatters.Pretty.Parser.Types\n      Test.Hspec.Core.Formatters.Pretty.Unicode\n      Test.Hspec.Core.Formatters.V1\n      Test.Hspec.Core.Formatters.V1.Free\n      Test.Hspec.Core.Formatters.V1.Monad\n      Test.Hspec.Core.Formatters.V2\n      Test.Hspec.Core.Hooks\n      Test.Hspec.Core.QuickCheck\n      Test.Hspec.Core.QuickCheckUtil\n      Test.Hspec.Core.Runner\n      Test.Hspec.Core.Runner.Eval\n      Test.Hspec.Core.Runner.PrintSlowSpecItems\n      Test.Hspec.Core.Runner.Result\n      Test.Hspec.Core.Shuffle\n      Test.Hspec.Core.Spec\n      Test.Hspec.Core.Spec.Monad\n      Test.Hspec.Core.Timer\n      Test.Hspec.Core.Tree\n      Test.Hspec.Core.Util\n      Control.Concurrent.Async\n      Data.Algorithm.Diff\n      GetOpt.Declarative.EnvironmentSpec\n      GetOpt.Declarative.UtilSpec\n      Helper\n      Mock\n      SpecHook\n      Test.Hspec.Core.ClockSpec\n      Test.Hspec.Core.CompatSpec\n      Test.Hspec.Core.Config.DefinitionSpec\n      Test.Hspec.Core.Config.OptionsSpec\n      Test.Hspec.Core.ConfigSpec\n      Test.Hspec.Core.Example.LocationSpec\n      Test.Hspec.Core.ExampleSpec\n      Test.Hspec.Core.FailureReportSpec\n      Test.Hspec.Core.FormatSpec\n      Test.Hspec.Core.Formatters.DiffSpec\n      Test.Hspec.Core.Formatters.InternalSpec\n      Test.Hspec.Core.Formatters.Pretty.ParserSpec\n      Test.Hspec.Core.Formatters.Pretty.UnicodeSpec\n      Test.Hspec.Core.Formatters.PrettySpec\n      Test.Hspec.Core.Formatters.V1Spec\n      Test.Hspec.Core.Formatters.V2Spec\n      Test.Hspec.Core.HooksSpec\n      Test.Hspec.Core.QuickCheckUtilSpec\n      Test.Hspec.Core.Runner.EvalSpec\n      Test.Hspec.Core.Runner.PrintSlowSpecItemsSpec\n      Test.Hspec.Core.Runner.ResultSpec\n      Test.Hspec.Core.RunnerSpec\n      Test.Hspec.Core.ShuffleSpec\n      Test.Hspec.Core.SpecSpec\n      Test.Hspec.Core.TimerSpec\n      Test.Hspec.Core.UtilSpec\n      Paths_hspec_core\n  default-language: Haskell2010\n  if impl(ghc >= 8.2.1)\n    build-depends:\n        ghc\n      , ghc-boot-th\n  if impl(ghc >= 8.4.1)\n    build-depends:\n        stm >=2.2\n  else\n    other-modules:\n        Control.Concurrent.STM.TMVar\n    hs-source-dirs:\n        vendor/stm-2.5.0.1/\n";
    }