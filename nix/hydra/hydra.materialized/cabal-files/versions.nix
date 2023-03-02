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
      specVersion = "2.2";
      identifier = { name = "versions"; version = "5.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "colin@fosskers.ca";
      author = "Colin Woodbury";
      homepage = "https://github.com/fosskers/versions";
      url = "";
      synopsis = "Types and parsers for software version numbers.";
      description = "A library for parsing and comparing software version numbers. We like to give\nversion numbers to our software in a myriad of ways. Some ways follow strict\nguidelines for incrementing and comparison. Some follow conventional wisdom\nand are generally self-consistent. Some are just plain asinine. This library\nprovides a means of parsing and comparing /any/ style of versioning, be it a\nnice Semantic Version like this:\n\n> 1.2.3-r1+git123\n\n...or a monstrosity like this:\n\n> 2:10.2+0.0093r3+1-1\n\nPlease switch to <http://semver.org Semantic Versioning> if you aren't\ncurrently using it. It provides consistency in version incrementing and has\nthe best constraints on comparisons.\n\nThis library implements version @2.0.0@ of the SemVer spec.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          ];
        buildable = true;
        };
      tests = {
        "versions-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."versions" or (errorHandler.buildDepError "versions"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/versions-5.0.4.tar.gz";
      sha256 = "29746a407f98b33d72e517e30fe3925591cf1e9f073d2797e6dd0a32e70cdefe";
      });
    }) // {
    package-description-override = "cabal-version:      2.2\nname:               versions\nversion:            5.0.4\nsynopsis:           Types and parsers for software version numbers.\ndescription:\n  A library for parsing and comparing software version numbers. We like to give\n  version numbers to our software in a myriad of ways. Some ways follow strict\n  guidelines for incrementing and comparison. Some follow conventional wisdom\n  and are generally self-consistent. Some are just plain asinine. This library\n  provides a means of parsing and comparing /any/ style of versioning, be it a\n  nice Semantic Version like this:\n  .\n  > 1.2.3-r1+git123\n  .\n  ...or a monstrosity like this:\n  .\n  > 2:10.2+0.0093r3+1-1\n  .\n  Please switch to <http://semver.org Semantic Versioning> if you aren't\n  currently using it. It provides consistency in version incrementing and has\n  the best constraints on comparisons.\n  .\n  This library implements version @2.0.0@ of the SemVer spec.\n\ncategory:           Data\nhomepage:           https://github.com/fosskers/versions\nauthor:             Colin Woodbury\nmaintainer:         colin@fosskers.ca\nlicense:            BSD-3-Clause\nlicense-file:       LICENSE\nbuild-type:         Simple\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\ncommon commons\n  default-language: Haskell2010\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n\n  build-depends:\n    , base        >=4.10 && <4.18\n    , megaparsec  >=7\n    , text        ^>=1.2 || ^>= 2.0\n\nlibrary\n  import:          commons\n  exposed-modules: Data.Versions\n  build-depends:\n    , deepseq   >=1.4\n    , hashable  >=1.2\n    , parser-combinators >= 1.0\n\ntest-suite versions-test\n  import:         commons\n  type:           exitcode-stdio-1.0\n  main-is:        Test.hs\n  hs-source-dirs: test\n  ghc-options:    -threaded -with-rtsopts=-N\n  build-depends:\n    , microlens         >=0.4\n    , QuickCheck        >=2.9\n    , tasty             >=0.10.1.2\n    , tasty-hunit       >=0.9.2\n    , tasty-quickcheck  >=0.8\n    , versions\n";
    }