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
    flags = { small_base = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "mersenne-random-pure64"; version = "0.2.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2008. Don Stewart <dons00@gmail.com>";
      maintainer = "Don Stewart <dons00@gmail.com>";
      author = "Don Stewart";
      homepage = "http://code.haskell.org/~dons/code/mersenne-random-pure64/";
      url = "";
      synopsis = "Generate high quality pseudorandom numbers purely using a Mersenne Twister";
      description = "The Mersenne twister is a pseudorandom number generator developed by\nMakoto Matsumoto and Takuji Nishimura that is based on a matrix linear\nrecurrence over a finite binary field. It provides for fast generation\nof very high quality pseudorandom numbers. The source for the C code\ncan be found here:\n\n<http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html>\n\nThis library provides a purely functional binding to the 64 bit\nclassic mersenne twister, along with instances of RandomGen, so the\ngenerator can be used with System.Random. The generator should\ntypically be a few times faster than the default StdGen (but a tad\nslower than the impure 'mersenne-random' library based on SIMD\ninstructions and destructive state updates.\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = if flags.small_base
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mersenne-random-pure64-0.2.2.0.tar.gz";
      sha256 = "ef1400ddc1ddafb0b98d4331a58bdbe3b5209a81579e17f85f180731a96d75d1";
      });
    }) // {
    package-description-override = "name:            mersenne-random-pure64\nversion:         0.2.2.0\nx-revision: 1\nhomepage:        http://code.haskell.org/~dons/code/mersenne-random-pure64/\nsynopsis:        Generate high quality pseudorandom numbers purely using a Mersenne Twister\ndescription:\n    The Mersenne twister is a pseudorandom number generator developed by\n    Makoto Matsumoto and Takuji Nishimura that is based on a matrix linear\n    recurrence over a finite binary field. It provides for fast generation\n    of very high quality pseudorandom numbers. The source for the C code\n    can be found here:\n    .\n    <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html>\n    .\n    This library provides a purely functional binding to the 64 bit\n    classic mersenne twister, along with instances of RandomGen, so the\n    generator can be used with System.Random. The generator should\n    typically be a few times faster than the default StdGen (but a tad\n    slower than the impure 'mersenne-random' library based on SIMD\n    instructions and destructive state updates.\n    .\ncategory:        Math, System\nlicense:         BSD3\nlicense-file:    LICENSE\ncopyright:       (c) 2008. Don Stewart <dons00@gmail.com>\nauthor:          Don Stewart\nmaintainer:      Don Stewart <dons00@gmail.com>\ncabal-version:   >= 1.6.0\nbuild-type:      Simple\ntested-with:     GHC == 7.6.2, GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.1\n\nsource-repository head\n  type: git\n  location: git://github.com/bgamari/mersenne-random-pure64\n\nflag small_base\n  description: Build with new smaller base library\n  default:     False\n\nlibrary\n    exposed-modules: System.Random.Mersenne.Pure64\n                     System.Random.Mersenne.Pure64.Base\n                     System.Random.Mersenne.Pure64.MTBlock\n                     System.Random.Mersenne.Pure64.Internal\n    extensions:      CPP, ForeignFunctionInterface\n\n    if flag(small_base)\n        build-depends: base  < 3\n    else\n        build-depends: base >= 3 && < 6, time >= 1.3, random\n\n    cc-options:\n        -O3 -finline-functions -fomit-frame-pointer\n        -fno-strict-aliasing --param max-inline-insns-single=1800\n\n    ghc-options:     -Wall -O2 -fexcess-precision\n\n    c-sources:        cbits/mt19937-64.c\n                      cbits/mt19937-64-unsafe.c\n                      cbits/mt19937-64-block.c\n    include-dirs:     include\n    includes:\n    install-includes: mt19937-64.h\n                      mt19937-64-unsafe.h\n                      mt19937-64-block.h\n";
    }