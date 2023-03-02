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
      identifier = { name = "unliftio"; version = "0.2.22.0"; };
      license = "MIT";
      copyright = "2017 FP Complete";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Francesco Mazzoli";
      homepage = "https://github.com/fpco/unliftio/tree/master/unliftio#readme";
      url = "";
      synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)";
      description = "Please see the documentation and README at <https://www.stackage.org/package/unliftio>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).le "7.10") (hsPkgs."nats" or (errorHandler.buildDepError "nats"));
        buildable = true;
        };
      tests = {
        "unliftio-spec" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      benchmarks = {
        "conc-bench" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unliftio-0.2.22.0.tar.gz";
      sha256 = "1fc4adb14bbefa303b01163a6dfd61c3bd7f775cebf8ee812d7194fb27ffbb88";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.4.\n--\n-- see: https://github.com/sol/hpack\n\nname:           unliftio\nversion:        0.2.22.0\nsynopsis:       The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)\ndescription:    Please see the documentation and README at <https://www.stackage.org/package/unliftio>\ncategory:       Control\nhomepage:       https://github.com/fpco/unliftio/tree/master/unliftio#readme\nauthor:         Michael Snoyman, Francesco Mazzoli\nmaintainer:     michael@snoyman.com\ncopyright:      2017 FP Complete\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nlibrary\n  exposed-modules:\n      UnliftIO\n      UnliftIO.Async\n      UnliftIO.Chan\n      UnliftIO.Concurrent\n      UnliftIO.Directory\n      UnliftIO.Environment\n      UnliftIO.Exception\n      UnliftIO.Foreign\n      UnliftIO.Internals.Async\n      UnliftIO.IO\n      UnliftIO.IO.File\n      UnliftIO.IORef\n      UnliftIO.Memoize\n      UnliftIO.MVar\n      UnliftIO.Process\n      UnliftIO.QSem\n      UnliftIO.QSemN\n      UnliftIO.STM\n      UnliftIO.Temporary\n      UnliftIO.Timeout\n  other-modules:\n      Paths_unliftio\n  hs-source-dirs:\n      src\n  ghc-options: -fwarn-incomplete-uni-patterns\n  build-depends:\n      async >2.1.1\n    , base >=4.9 && <5\n    , bytestring\n    , deepseq\n    , directory\n    , filepath\n    , process >=1.2.0.0\n    , stm >=2.5\n    , time\n    , transformers\n    , unliftio-core >=0.1.1.0\n  if os(windows)\n    cpp-options: -DWINDOWS\n  else\n    build-depends:\n        unix\n  if impl(ghc <= 7.10)\n    build-depends:\n        nats\n  if os(darwin)\n    other-modules:\n        UnliftIO.IO.File.Posix\n    c-sources:\n        cbits/time-osx.c\n        cbits/file-posix.c\n  else\n    if os(windows)\n      c-sources:\n          cbits/time-windows.c\n    else\n      other-modules:\n          UnliftIO.IO.File.Posix\n      c-sources:\n          cbits/file-posix.c\n          cbits/time-posix.c\n  default-language: Haskell2010\n\ntest-suite unliftio-spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      UnliftIO.AsyncSpec\n      UnliftIO.DirectorySpec\n      UnliftIO.ExceptionSpec\n      UnliftIO.IO.FileSpec\n      UnliftIO.IOSpec\n      UnliftIO.MemoizeSpec\n      UnliftIO.PooledAsyncSpec\n      Paths_unliftio\n  hs-source-dirs:\n      test\n  build-depends:\n      QuickCheck\n    , async >2.1.1\n    , base >=4.9 && <5\n    , bytestring\n    , containers\n    , deepseq\n    , directory\n    , filepath\n    , hspec\n    , process >=1.2.0.0\n    , stm >=2.5\n    , time\n    , transformers\n    , unliftio\n    , unliftio-core >=0.1.1.0\n  if os(windows)\n    cpp-options: -DWINDOWS\n  else\n    build-depends:\n        unix\n  default-language: Haskell2010\n\nbenchmark conc-bench\n  type: exitcode-stdio-1.0\n  main-is: ConcBench.hs\n  other-modules:\n      Paths_unliftio\n  hs-source-dirs:\n      bench\n  ghc-options: -O2 -threaded -rtsopts\n  build-depends:\n      async >2.1.1\n    , base >=4.9 && <5\n    , bytestring\n    , deepseq\n    , directory\n    , filepath\n    , gauge\n    , process >=1.2.0.0\n    , stm >=2.5\n    , time\n    , transformers\n    , unliftio\n    , unliftio-core >=0.1.1.0\n  if os(windows)\n    cpp-options: -DWINDOWS\n  else\n    build-depends:\n        unix\n  default-language: Haskell2010\n";
    }