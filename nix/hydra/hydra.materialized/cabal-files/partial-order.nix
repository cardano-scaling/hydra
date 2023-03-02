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
    flags = { extra-instances = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "partial-order"; version = "0.2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2020 Moritz Clasmeier";
      maintainer = "mtesseract@silverratio.net";
      author = "Moritz Clasmeier";
      homepage = "https://github.com/mtesseract/haskell-partial-order";
      url = "";
      synopsis = "Provides typeclass suitable for types admitting a partial order";
      description = "This packages provides the PartialOrd typeclass suitable for\ntypes admitting a partial order.\nThe only module exposed by this package is\nData.PartialOrd. Along with the PartialOrd\ntypeclass and some utility functions for working\nwith partially ordered types, it exports\ninstances for certain numeric types along with\ninstances for lists and sets.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (flags.extra-instances) (hsPkgs."containers" or (errorHandler.buildDepError "containers"));
        buildable = true;
        };
      tests = {
        "partial-order-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."partial-order" or (errorHandler.buildDepError "partial-order"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ] ++ (pkgs.lib).optional (flags.extra-instances) (hsPkgs."containers" or (errorHandler.buildDepError "containers"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/partial-order-0.2.0.0.tar.gz";
      sha256 = "b25fb46335d11e9a9201d6a3685bff94e13ab73baf7f99eb770218ad1edcc5c8";
      });
    }) // {
    package-description-override = "name:                partial-order\nversion:             0.2.0.0\nsynopsis:            Provides typeclass suitable for types admitting a partial order\ndescription:         This packages provides the PartialOrd typeclass suitable for\n                     types admitting a partial order.\n\n                     The only module exposed by this package is\n                     Data.PartialOrd. Along with the PartialOrd\n                     typeclass and some utility functions for working\n                     with partially ordered types, it exports\n                     instances for certain numeric types along with\n                     instances for lists and sets.\nhomepage:            https://github.com/mtesseract/haskell-partial-order\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Moritz Clasmeier\nmaintainer:          mtesseract@silverratio.net\ncopyright:           (c) 2016-2020 Moritz Clasmeier\ncategory:            Data\nbuild-type:          Simple\n-- extra-source-files:\ncabal-version:       >=1.10\n\nflag extra-instances\n  description:\n    Include additional instances of PartialOrd in Data.Set\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Data.PartialOrd\n  build-depends:       base >= 4.7 && < 5\n  default-language:    Haskell2010\n\n  default-extensions:  CPP\n\n  if flag(extra-instances)\n    build-depends:     containers >= 0.5.0.0 && < 0.7\n    CPP-Options:       -DEXTRA_INSTANCES\n\ntest-suite partial-order-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  build-depends:       base\n                     , partial-order\n                     , HUnit >= 1.5.0.0 && < 1.7\n                     , test-framework >= 0.8.1.1 && < 0.9\n                     , test-framework-hunit >= 0.3.0.2 && < 0.4\n                     , test-framework-quickcheck2 >= 0.3.0.3 && < 0.4\n                     , containers >= 0.5.0.0 && < 0.7\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n\n  default-extensions:  CPP\n\n  if flag(extra-instances)\n    build-depends:     containers >= 0.5.0.0 && < 0.7\n    CPP-Options:       -DEXTRA_INSTANCES\n\nsource-repository head\n  type:     git\n location: https://github.com/mtesseract/haskell-partial-order\n";
    }