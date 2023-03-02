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
      specVersion = "1.18";
      identifier = { name = "fingertree"; version = "0.1.5.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2006 Ross Paterson, Ralf Hinze";
      maintainer = "Ross Paterson <R.Paterson@city.ac.uk>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Generic finger-tree structure, with example instances";
      description = "A general sequence representation with arbitrary\nannotations, for use as a base for implementations of\nvarious collection types, with examples, as described\nin section 4 of\n\n* Ralf Hinze and Ross Paterson,\n\\\"Finger trees: a simple general-purpose data structure\\\",\n/Journal of Functional Programming/ 16:2 (2006) pp 197-217.\n<http://staff.city.ac.uk/~ross/papers/FingerTree.html>\n\nFor a tuned sequence type, see @Data.Sequence@ in the\n@containers@ package, which is a specialization of\nthis structure.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "ft-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fingertree-0.1.5.0.tar.gz";
      sha256 = "f3263c92fa8b18f1e1a64cd12480c8c1bee2c1fa0584ab3345f3dd8522bdbf71";
      });
    }) // {
    package-description-override = "Name:           fingertree\nVersion:        0.1.5.0\nCabal-Version:  1.18\nCopyright:      (c) 2006 Ross Paterson, Ralf Hinze\nLicense:        BSD3\nLicense-File:   LICENSE\nMaintainer:     Ross Paterson <R.Paterson@city.ac.uk>\nbug-reports:    http://hub.darcs.net/ross/fingertree/issues\nCategory:       Data Structures\nSynopsis:       Generic finger-tree structure, with example instances\nDescription:\n                A general sequence representation with arbitrary\n                annotations, for use as a base for implementations of\n                various collection types, with examples, as described\n                in section 4 of\n                .\n                 * Ralf Hinze and Ross Paterson,\n                   \\\"Finger trees: a simple general-purpose data structure\\\",\n                   /Journal of Functional Programming/ 16:2 (2006) pp 197-217.\n                   <http://staff.city.ac.uk/~ross/papers/FingerTree.html>\n                .\n                For a tuned sequence type, see @Data.Sequence@ in the\n                @containers@ package, which is a specialization of\n                this structure.\nBuild-Type:     Simple\nExtra-Source-Files:\n                changelog\nExtra-Doc-Files:\n                images/search.svg\n\nSource-Repository head\n  Type: darcs\n  Location: http://hub.darcs.net/ross/fingertree\n\nLibrary\n  Build-Depends: base < 6\n  Default-Language: Haskell2010\n  Other-Extensions:\n                MultiParamTypeClasses\n                FunctionalDependencies\n                FlexibleInstances\n                UndecidableInstances\n  Exposed-Modules:\n                Data.FingerTree\n                Data.IntervalMap.FingerTree\n                Data.PriorityQueue.FingerTree\n\nTest-suite ft-properties\n  type: exitcode-stdio-1.0\n  main-is: tests/ft-properties.hs\n  cpp-options: -DTESTING\n  default-language: Haskell2010\n  build-depends:\n                base >= 4.2 && < 6,\n                HUnit,\n                QuickCheck,\n                test-framework,\n                test-framework-hunit,\n                test-framework-quickcheck2\n";
    }