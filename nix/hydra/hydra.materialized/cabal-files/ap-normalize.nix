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
    flags = { test-with-clang = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ap-normalize"; version = "0.1.0.1"; };
      license = "MIT";
      copyright = "Li-yao Xia 2020";
      maintainer = "lysxia@gmail.com";
      author = "Li-yao Xia";
      homepage = "";
      url = "";
      synopsis = "Self-normalizing applicative expressions";
      description = "An applicative functor transformer to normalize expressions using @(\\<$>)@,\n@(\\<*>)@, and @pure@ into a linear list of actions.\nSee \"ApNormalize\" to get started.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "example-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."ap-normalize" or (errorHandler.buildDepError "ap-normalize"))
            ];
          buildable = true;
          };
        "assoc-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."ap-normalize" or (errorHandler.buildDepError "ap-normalize"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ap-normalize-0.1.0.1.tar.gz";
      sha256 = "820613b12ce759c8c8a254c78a0e4c474b2cd4cfd08fc0c1d4d5584c58ff2288";
      });
    }) // {
    package-description-override = "cabal-version: >=1.10\n\nname:    ap-normalize\nversion: 0.1.0.1\nsynopsis: Self-normalizing applicative expressions\ndescription:\n  An applicative functor transformer to normalize expressions using @(\\<$>)@,\n  @(\\<*>)@, and @pure@ into a linear list of actions.\n\n  See \"ApNormalize\" to get started.\n\nbug-reports: https://gitlab.com/lysxia/ap-normalize/-/issues\nlicense: MIT\nlicense-file: LICENSE\nauthor:       Li-yao Xia\nmaintainer:   lysxia@gmail.com\ncopyright:    Li-yao Xia 2020\ncategory:     Control\nbuild-type:   Simple\nextra-source-files:  CHANGELOG.md, README.md\n\nlibrary\n  hs-source-dirs: src\n  exposed-modules:\n    ApNormalize\n    ApNormalize.Aps\n    ApNormalize.DList\n  build-depends:       base >=4.8 && <5\n  ghc-options: -Wall\n  default-language:    Haskell2010\n\ntest-suite example-test\n  main-is: example.hs\n  type:    exitcode-stdio-1.0\n  hs-source-dirs:   test\n  default-language: Haskell2010\n  build-depends:\n    base,\n    inspection-testing,\n    ap-normalize\n\ntest-suite assoc-test\n  main-is: assoc.hs\n  type:    exitcode-stdio-1.0\n  hs-source-dirs:   test\n  default-language: Haskell2010\n  build-depends:\n    base,\n    inspection-testing,\n    transformers,\n    ap-normalize\n  if flag(test-with-clang)\n    ghc-options: -pgmP \"clang -E -traditional -x c\"\n\nflag test-with-clang\n  manual: True\n  default: False\n";
    }