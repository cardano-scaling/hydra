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
      identifier = { name = "logict"; version = "0.8.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2007-2014 Dan Doel,\n(c) 2011-2013 Edward Kmett,\n(c) 2014      Roman Cheplyaka,\n(c) 2020-2021 Andrew Lelechenko,\n(c) 2020-2021 Kevin Quick";
      maintainer = "Andrew Lelechenko <andrew.lelechenko@gmail.com>";
      author = "Dan Doel";
      homepage = "https://github.com/Bodigrim/logict#readme";
      url = "";
      synopsis = "A backtracking logic-programming monad.";
      description = "Adapted from the paper\n<http://okmij.org/ftp/papers/LogicT.pdf Backtracking, Interleaving, and Terminating Monad Transformers>\nby Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8.0") [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      exes = {
        "grandparents" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
            ];
          buildable = false;
          };
        };
      tests = {
        "logict-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/logict-0.8.0.0.tar.gz";
      sha256 = "fe926df295bc43df794247360e7a0a0dfdc778bce986f8aadba98fe52228fb56";
      });
    }) // {
    package-description-override = "name: logict\nversion: 0.8.0.0\nlicense: BSD3\nlicense-file: LICENSE\ncopyright:\n  (c) 2007-2014 Dan Doel,\n  (c) 2011-2013 Edward Kmett,\n  (c) 2014      Roman Cheplyaka,\n  (c) 2020-2021 Andrew Lelechenko,\n  (c) 2020-2021 Kevin Quick\nmaintainer: Andrew Lelechenko <andrew.lelechenko@gmail.com>\nauthor: Dan Doel\nhomepage: https://github.com/Bodigrim/logict#readme\nsynopsis: A backtracking logic-programming monad.\ndescription:\n  Adapted from the paper\n  <http://okmij.org/ftp/papers/LogicT.pdf Backtracking, Interleaving, and Terminating Monad Transformers>\n  by Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry.\ncategory: Control\nbuild-type: Simple\nextra-source-files:\n  changelog.md\n  README.md\ncabal-version: >=1.10\ntested-with: GHC ==7.0.4 GHC ==7.2.2 GHC ==7.4.2 GHC ==7.6.3 GHC ==7.8.4 GHC ==7.10.3 GHC ==8.0.2 GHC ==8.2.2 GHC ==8.4.4 GHC ==8.6.5 GHC ==8.8.4 GHC ==8.10.7 GHC ==9.0.2 GHC ==9.2.2\n\nsource-repository head\n  type: git\n  location: https://github.com/Bodigrim/logict\n\nlibrary\n  exposed-modules:\n    Control.Monad.Logic\n    Control.Monad.Logic.Class\n  default-language: Haskell2010\n\n  ghc-options: -O2 -Wall\n  if impl(ghc >= 8.0)\n    ghc-options:    -Wcompat\n\n  build-depends:\n    base >=4.3 && <5,\n    mtl >=2.0 && <2.4\n\n  if impl(ghc <8.0)\n    build-depends:\n      fail, transformers\n\nexecutable grandparents\n  buildable: False\n  main-is: grandparents.hs\n  hs-source-dirs: example\n  default-language: Haskell2010\n  build-depends:\n    base,\n    logict\n\ntest-suite logict-tests\n  type: exitcode-stdio-1.0\n  main-is: Test.hs\n  default-language: Haskell2010\n\n  ghc-options: -Wall\n  if impl(ghc >= 8.0)\n    ghc-options:    -Wcompat -Wno-incomplete-uni-patterns\n\n  build-depends:\n    base,\n    async >=2.0,\n    logict,\n    mtl,\n    tasty,\n    tasty-hunit\n\n  hs-source-dirs: test\n";
    }