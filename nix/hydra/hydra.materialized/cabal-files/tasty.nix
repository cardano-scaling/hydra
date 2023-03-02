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
    flags = { clock = true; unix = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "tasty"; version = "1.4.2.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "Roman Cheplyaka <roma@ro-che.info>";
      author = "Roman Cheplyaka <roma@ro-che.info>";
      homepage = "https://github.com/UnkindPartition/tasty";
      url = "";
      synopsis = "Modern and extensible testing framework";
      description = "Tasty is a modern testing framework for Haskell.\nIt lets you combine your unit tests, golden\ntests, QuickCheck/SmallCheck properties, and any\nother types of tests into a single test suite.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."unbounded-delays" or (errorHandler.buildDepError "unbounded-delays"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if flags.clock && !(compiler.isGhcjs && true)
          then [ (hsPkgs."clock" or (errorHandler.buildDepError "clock")) ]
          else [
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ])) ++ (pkgs.lib).optionals (!system.isWindows && !(compiler.isGhcjs && true)) ([
          (hsPkgs."wcwidth" or (errorHandler.buildDepError "wcwidth"))
          ] ++ (pkgs.lib).optional (flags.unix) (hsPkgs."unix" or (errorHandler.buildDepError "unix")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tasty-1.4.2.3.tar.gz";
      sha256 = "c914a7a9a28dfc33dc5a112c349f2e69f536cb66d2b2b44898a3411cbdcad0c6";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\nname:                tasty\nversion:             1.4.2.3\nsynopsis:            Modern and extensible testing framework\ndescription:         Tasty is a modern testing framework for Haskell.\n                     It lets you combine your unit tests, golden\n                     tests, QuickCheck/SmallCheck properties, and any\n                     other types of tests into a single test suite.\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Roman Cheplyaka <roma@ro-che.info>\nmaintainer:          Roman Cheplyaka <roma@ro-che.info>\nhomepage:            https://github.com/UnkindPartition/tasty\nbug-reports:         https://github.com/UnkindPartition/tasty/issues\n-- copyright:\ncategory:            Testing\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md, README.md\n\nSource-repository head\n  type:     git\n  location: git://github.com/UnkindPartition/tasty.git\n  subdir:   core\n\nflag clock\n  description:\n    Depend on the clock package for more accurate time measurement\n  default: True\n\nflag unix\n  description:\n    Depend on the unix package to install signal handlers\n  default: True\n\nlibrary\n  exposed-modules:\n    Test.Tasty,\n    Test.Tasty.Options,\n    Test.Tasty.Providers,\n    Test.Tasty.Providers.ConsoleFormat,\n    Test.Tasty.Runners\n    Test.Tasty.Ingredients,\n    Test.Tasty.Ingredients.Basic\n    Test.Tasty.Ingredients.ConsoleReporter\n\n    -- for testing only\n    Test.Tasty.Patterns.Types\n    Test.Tasty.Patterns.Parser\n    Test.Tasty.Patterns.Printer\n    Test.Tasty.Patterns.Eval\n  other-modules:\n    Control.Concurrent.Async\n    Test.Tasty.Parallel,\n    Test.Tasty.Core,\n    Test.Tasty.Options.Core,\n    Test.Tasty.Options.Env,\n    Test.Tasty.Patterns,\n    Test.Tasty.Patterns.Expr,\n    Test.Tasty.Run,\n    Test.Tasty.Runners.Reducers,\n    Test.Tasty.Runners.Utils,\n    Test.Tasty.CmdLine,\n    Test.Tasty.Ingredients.ListTests\n    Test.Tasty.Ingredients.IncludingOptions\n\n  build-depends:\n    base                 >= 4.9 && < 5,\n    stm                  >= 2.3,\n    containers,\n    transformers         >= 0.5,\n    tagged               >= 0.5,\n    optparse-applicative >= 0.14,\n    unbounded-delays     >= 0.1,\n    ansi-terminal        >= 0.9\n  if(!impl(ghc >= 8.0))\n    build-depends: semigroups\n\n  if flag(clock) && !impl(ghcjs)\n    build-depends: clock >= 0.4.4.0\n  else\n    build-depends: time  >= 1.4\n\n  if !os(windows) && !impl(ghcjs)\n    build-depends: wcwidth\n    if flag(unix)\n      build-depends: unix\n\n  -- hs-source-dirs:\n  default-language:    Haskell2010\n  default-extensions:  CPP, ScopedTypeVariables, DeriveDataTypeable\n  ghc-options:\n    -Wall\n    -Wno-incomplete-uni-patterns\n    -Wcompat\n";
    }