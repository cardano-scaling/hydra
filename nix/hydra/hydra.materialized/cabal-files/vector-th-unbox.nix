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
      identifier = { name = "vector-th-unbox"; version = "0.2.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2015 Liyang HU";
      maintainer = "Fumiaki Kinoshita <fumiexcel@gmail.com>";
      author = "Liyang HU <vector-th-unbox@liyang.hu>";
      homepage = "https://github.com/tsurucapital/vector-th-unbox";
      url = "";
      synopsis = "Deriver for Data.Vector.Unboxed using Template Haskell";
      description = "A Template Haskell deriver for unboxed vectors, given a pair of coercion\nfunctions to and from some existing type with an Unbox instance.\n\nRefer to \"Data.Vector.Unboxed.Deriving\" for documentation and examples.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "sanity" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-th-unbox" or (errorHandler.buildDepError "vector-th-unbox"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-th-unbox-0.2.2.tar.gz";
      sha256 = "8aa4ca464e842706e5b5234b8242d1aafec9ee755659b0e3ff44ecde13a80149";
      });
    }) // {
    package-description-override = "name:           vector-th-unbox\r\nversion:        0.2.2\r\nx-revision: 3\r\nsynopsis:       Deriver for Data.Vector.Unboxed using Template Haskell\r\ndescription:\r\n    A Template Haskell deriver for unboxed vectors, given a pair of coercion\r\n    functions to and from some existing type with an Unbox instance.\r\n    .\r\n    Refer to \"Data.Vector.Unboxed.Deriving\" for documentation and examples.\r\nstability:      experimental\r\nhomepage:       https://github.com/tsurucapital/vector-th-unbox\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\ncopyright:      (c) 2012-2015 Liyang HU\r\nauthor:         Liyang HU <vector-th-unbox@liyang.hu>\r\nmaintainer:     Fumiaki Kinoshita <fumiexcel@gmail.com>\r\ncategory:       Data\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\ntested-with:\r\n  GHC == 8.0.2,\r\n  GHC == 8.2.2,\r\n  GHC == 8.4.4,\r\n  GHC == 8.6.5, GHC == 8.8.3, GHC == 8.10.1, GHC == 9.0.1, GHC ==9.2.1\r\nextra-source-files:\r\n  CHANGELOG.md\r\n  README.md\r\n\r\nsource-repository head\r\n    type:       git\r\n    location:   http://github.com/tsurucapital/vector-th-unbox\r\n\r\nlibrary\r\n    default-language: Haskell2010\r\n    exposed-modules:\r\n        Data.Vector.Unboxed.Deriving\r\n\r\n    build-depends:\r\n        base >= 4.9 && < 4.18,\r\n        template-haskell >= 2.5 && <2.20,\r\n        vector >= 0.7.1 && <0.14\r\n\r\ntest-suite sanity\r\n    default-language: Haskell2010\r\n    type: exitcode-stdio-1.0\r\n    hs-source-dirs: tests\r\n    main-is: sanity.hs\r\n    build-depends:\r\n        base,\r\n        data-default,\r\n        vector,\r\n        vector-th-unbox\r\n    ghc-options: -Wall\r\n\r\n-- vim: et sw=4 ts=4 sts=4:\r\n\r\n";
    }