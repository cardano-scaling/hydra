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
      identifier = { name = "silently"; version = "1.2.5.3"; };
      license = "BSD-3-Clause";
      copyright = "(c) Trystan Spangler 2011";
      maintainer = "Sönke Hahn <soenkehahn@gmail.com>, Simon Hengel <sol@typeful.net>, Andreas Abel";
      author = "Trystan Spangler";
      homepage = "https://github.com/hspec/silently";
      url = "https://github.com/hspec/silently";
      synopsis = "Prevent or capture writing to stdout and other handles.";
      description = "Prevent or capture writing to stdout, stderr, and other handles.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "spec-specific" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."nanospec" or (errorHandler.buildDepError "nanospec"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          buildable = true;
          };
        "spec-generic" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."nanospec" or (errorHandler.buildDepError "nanospec"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/silently-1.2.5.3.tar.gz";
      sha256 = "ba9dafafd29438a830afd158c3b83ce157d50e984352ff3ddf60a74c22f36372";
      });
    }) // {
    package-description-override = "cabal-version: >= 1.10\nname: silently\nversion: 1.2.5.3\nbuild-type: Simple\nlicense: BSD3\nlicense-file: LICENSE\ncopyright: (c) Trystan Spangler 2011\nmaintainer: Sönke Hahn <soenkehahn@gmail.com>, Simon Hengel <sol@typeful.net>, Andreas Abel\nhomepage: https://github.com/hspec/silently\npackage-url: https://github.com/hspec/silently\nbug-reports: https://github.com/hspec/silently/issues\nsynopsis: Prevent or capture writing to stdout and other handles.\ndescription: Prevent or capture writing to stdout, stderr, and other handles.\ncategory: System, Testing\nauthor: Trystan Spangler\n\ntested-with:\n  GHC == 9.4.1\n  GHC == 9.2.4\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n  GHC == 7.2.2\n  GHC == 7.0.4\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/silently\n\nLibrary\n  hs-source-dirs:\n      src\n  default-language:\n      Haskell98\n  exposed-modules:\n      System.IO.Silently\n\n  build-depends:\n      base >= 4.3 && < 5\n    , directory\n    , deepseq\n\n  if os(windows)\n    cpp-options: -DWINDOWS\n  if os(linux) || os(osx) || os(freebsd) || os(openbsd) || os(netbsd)\n    cpp-options: -DUNIX\n\n  ghc-options:\n    -Wall\n  if impl(ghc >= 8)\n    ghc-options:\n      -Wcompat\n\n-- This tests the platform specific implementation.\n--\n-- NOTE: Cabal 1.10 can not deal with conditional (== if-else) options.  This\n-- is why we depend on silently to test the platform specific implementation.\n--\n-- As a consequence we can not use Hspec for testing, as this would result in\n-- depending on two different versions of silently at the same time!\ntest-suite spec-specific\n  type:\n      exitcode-stdio-1.0\n  hs-source-dirs:\n      test\n  main-is:\n      Spec.hs\n  default-language:\n      Haskell98\n  ghc-options:\n      -Wall -threaded\n  build-depends:\n      base\n    , silently\n    , directory\n    , nanospec\n    , temporary\n\n-- This tests the generic implementation, that should work on all platforms.\ntest-suite spec-generic\n  type:\n      exitcode-stdio-1.0\n  hs-source-dirs:\n      src\n    , test\n  main-is:\n      Spec.hs\n  default-language:\n      Haskell98\n  ghc-options:\n      -Wall -threaded\n  other-modules:\n      System.IO.Silently\n\n  build-depends:\n      base\n    , deepseq\n    , directory\n    , nanospec\n    , temporary\n";
    }