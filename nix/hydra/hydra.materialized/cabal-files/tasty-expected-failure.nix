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
      identifier = { name = "tasty-expected-failure"; version = "0.12.3"; };
      license = "MIT";
      copyright = "2015 Joachim Breitner";
      maintainer = "mail@joachim-breitner.de";
      author = "Joachim Breitner";
      homepage = "http://github.com/nomeata/tasty-expected-failure";
      url = "";
      synopsis = "Mark tasty tests as failure expected";
      description = "With the function 'Test.Tasty.ExpectedFailure.expectFail' in the provided module\n\"Test.Tasty.ExpectedFailure\", you can mark that you expect test cases to fail,\nand not to pass.\n\nThis can for example be used for test-driven development: Create the tests,\nmark them with 'Test.Tasty.ExpectedFailure.expectFail', and you can still push\nto the main branch, without your continuous integration branch failing.\n\nOnce someone implements the feature or fixes the bug (maybe unknowingly), the\ntest suite will tell him so, due to the now unexpectedly passing test, and he\ncan remove the 'Test.Tasty.ExpectedFailure.expectFail' marker.\n\nThe module also provides 'Test.Tasty.ExpectedFailure.ignoreTest' to avoid\nrunning a test. Both funtions are implemented via the more general\n'Test.Tasty.ExpectedFailure.wrapTest', which is also provided.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."unbounded-delays" or (errorHandler.buildDepError "unbounded-delays"))
          ];
        buildable = true;
        };
      tests = {
        "expected-fail-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            ];
          buildable = true;
          };
        "expected-fail-hh-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tasty-expected-failure-0.12.3.tar.gz";
      sha256 = "cb07cc5ca62a6fd673ef54ae70b4bc5f9c12662fe835bea1f38b944684ee8f7e";
      });
    }) // {
    package-description-override = "name:                tasty-expected-failure\nversion:             0.12.3\nsynopsis:            Mark tasty tests as failure expected\ndescription:\n With the function 'Test.Tasty.ExpectedFailure.expectFail' in the provided module\n \"Test.Tasty.ExpectedFailure\", you can mark that you expect test cases to fail,\n and not to pass.\n .\n This can for example be used for test-driven development: Create the tests,\n mark them with 'Test.Tasty.ExpectedFailure.expectFail', and you can still push\n to the main branch, without your continuous integration branch failing.\n .\n Once someone implements the feature or fixes the bug (maybe unknowingly), the\n test suite will tell him so, due to the now unexpectedly passing test, and he\n can remove the 'Test.Tasty.ExpectedFailure.expectFail' marker.\n .\n The module also provides 'Test.Tasty.ExpectedFailure.ignoreTest' to avoid\n running a test. Both funtions are implemented via the more general\n 'Test.Tasty.ExpectedFailure.wrapTest', which is also provided.\nhomepage: http://github.com/nomeata/tasty-expected-failure\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Joachim Breitner\nmaintainer:          mail@joachim-breitner.de\ncopyright:           2015 Joachim Breitner\ncategory:            Testing\nbuild-type:          Simple\nextra-source-files:  README.md\ncabal-version:       >=1.10\ntested-with: GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.3, GHC == 8.10.1\n\nlibrary\n  exposed-modules:\n    Test.Tasty.ExpectedFailure\n  build-depends:\n    base >= 4.9 && <5,\n    tagged >= 0.7 && < 0.9,\n    tasty >= 0.11,\n    unbounded-delays < 0.2\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: git://github.com/nomeata/tasty-expected-failure\n\n\ntest-suite expected-fail-tests\n  type: exitcode-stdio-1.0\n  default-language:    Haskell2010\n  hs-source-dirs:      tests\n  main-is:             TestExpectedFails.hs\n  build-depends:       base,\n                       tasty,\n                       tasty-hunit,\n                       tasty-golden,\n                       tasty-expected-failure\n\n\ntest-suite expected-fail-hh-tests\n  type: exitcode-stdio-1.0\n  default-language:    Haskell2010\n  hs-source-dirs:      tests\n  main-is:             TestExpectedFailsHH.hs\n  build-depends:       base,\n                       hedgehog,\n                       tasty,\n                       tasty-hedgehog,\n                       tasty-expected-failure\n";
    }