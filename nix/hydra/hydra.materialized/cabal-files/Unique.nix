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
      specVersion = "2.0";
      identifier = { name = "Unique"; version = "0.4.7.9"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ualinuxcn@gmail.com";
      author = "Volodymyr Yashchenko";
      homepage = "";
      url = "";
      synopsis = "It provides the functionality like unix \"uniq\" utility";
      description = "Library provides the functions to find unique and duplicate elements in the list";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      tests = {
        "HspecTest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "Criterion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Unique-0.4.7.9.tar.gz";
      sha256 = "34e8247ef2769cae219ff05bed0c8269d680396c451c1d31aa6ad506abc5c191";
      });
    }) // {
    package-description-override = "cabal-version: 2.0\nname:          Unique\nversion:       0.4.7.9\nlicense:       BSD3\nlicense-file:  LICENSE\nmaintainer:    ualinuxcn@gmail.com\nauthor:        Volodymyr Yashchenko\ntested-with:   ghc >=7.4 && <8.2.1 || >8.2.1 && <8.12\nsynopsis:      It provides the functionality like unix \"uniq\" utility\ndescription:\n    Library provides the functions to find unique and duplicate elements in the list\n\ncategory:      Data\nbuild-type:    Simple\n\nsource-repository head\n    type:     git\n    location: https://github.com/kapralVV/Unique.git\n\nlibrary\n    exposed-modules:\n        Data.List.Unique\n        Data.List.UniqueStrict\n        Data.List.UniqueUnsorted\n\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base >=4.0 && < 5,\n        containers >=0.5.0.0 && <=0.7,\n        extra >=1.6.2 && <=1.8,\n        hashable >= 1.2.6 && <=1.4,\n        unordered-containers >= 0.2.8 && <=0.3\n\ntest-suite HspecTest\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   tests\n    other-modules:\n        Unique.Complex\n        Unique.IsUnique\n        Unique.RepeatedBy\n        Unique.SortUniq\n        Unique.AllUnique\n        UniqueStrict.IsUnique\n        UniqueStrict.RepeatedBy\n        UniqueStrict.SortUniq\n        UniqueStrict.AllUnique\n        UniqueUnsorted.IsUnique\n        UniqueUnsorted.RemoveDuplicates\n        UniqueUnsorted.RepeatedBy\n        UniqueUnsorted.AllUnique\n\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base >=4.0 && <5,\n        Unique,\n        hspec -any,\n        containers >=0.5.0.0 && <=0.7,\n        QuickCheck    >= 2.10 && <2.15\n\nbenchmark Criterion\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   bench\n    default-language: Haskell2010\n    ghc-options:      -Wall -rtsopts\n    build-depends:\n        base >=4.0 && <5,\n        Unique,\n        criterion -any,\n        QuickCheck    >= 2.10 && <2.15,\n        quickcheck-instances -any,\n        bytestring -any,\n        hashable -any\n";
    }