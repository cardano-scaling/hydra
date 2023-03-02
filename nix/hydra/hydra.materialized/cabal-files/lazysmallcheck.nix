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
      specVersion = "1.0";
      identifier = { name = "lazysmallcheck"; version = "0.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Matthew Naylor <mfn@cs.york.ac.uk>";
      author = "Matthew Naylor and Fredrik Lindblad";
      homepage = "http://www.cs.york.ac.uk/~mfn/lazysmallcheck/";
      url = "";
      synopsis = "A library for demand-driven testing of Haskell programs";
      description = "Lazy SmallCheck is a library for exhaustive, demand-driven testing of\nHaskell programs.  It is based on the idea that if a property holds\nfor a partially-defined input then it must also hold for all\nfully-defined refinements of the that input.  Compared to ``eager''\ninput generation as in SmallCheck, Lazy SmallCheck may require\nsignificantly fewer test-cases to verify a property for all inputs up\nto a given depth.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lazysmallcheck-0.6.tar.gz";
      sha256 = "9dd4dfb590c77e4f6aff68296602de58422eed5e7148fc29190d875a4e7d0f53";
      });
    }) // {
    package-description-override = "Name:               lazysmallcheck\nVersion:            0.6\nMaintainer:         Matthew Naylor <mfn@cs.york.ac.uk>\nHomepage:           http://www.cs.york.ac.uk/~mfn/lazysmallcheck/\nBuild-Depends:      base < 5\nLicense:            BSD3\nLicense-File:       LICENSE\nAuthor:             Matthew Naylor and Fredrik Lindblad\nSynopsis:           A library for demand-driven testing of Haskell programs\nDescription:\n  Lazy SmallCheck is a library for exhaustive, demand-driven testing of\n  Haskell programs.  It is based on the idea that if a property holds\n  for a partially-defined input then it must also hold for all\n  fully-defined refinements of the that input.  Compared to ``eager'' \n  input generation as in SmallCheck, Lazy SmallCheck may require\n  significantly fewer test-cases to verify a property for all inputs up \n  to a given depth.\nCategory:           Testing\nBuild-Type:         Simple\nExtra-Source-Files:\n  examples/Catch.hs\n  examples/Mate.hs\n  examples/Sad.hs\n  examples/Countdown.hs\n  examples/Mux.hs\n  examples/SumPuz.hs\n  examples/Huffman.hs\n  examples/RedBlack.hs\n  examples/Turner.hs\n  examples/ListSet.hs\n  examples/RegExp.hs\n  examples/test/TestCatch.hs\n  examples/test/TestMux2.hs\n  examples/test/TestCountdown1.hs\n  examples/test/TestMux3.hs\n  examples/test/TestCountdown2.hs\n  examples/test/TestRedBlack.hs\n  examples/test/TestHuffman1.hs\n  examples/test/TestRegExp.hs\n  examples/test/TestHuffman2.hs\n  examples/test/TestSad.hs\n  examples/test/TestListSet1.hs\n  examples/test/TestSumPuz.hs\n  examples/test/TestMate.hs\n  examples/test/TestTurner.hs\n  examples/test/TestMux1.hs\n  examples/test/make.sh\n  Test/LazySmallCheck/Generic.hs\n\nExposed-modules:\n  Test.LazySmallCheck\n";
    }