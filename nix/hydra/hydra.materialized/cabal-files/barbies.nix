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
      identifier = { name = "barbies"; version = "2.0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "2018 Daniel Gorin";
      maintainer = "jcpetruzza@gmail.com";
      author = "Daniel Gorin";
      homepage = "https://github.com/jcpetruzza/barbies#readme";
      url = "";
      synopsis = "Classes for working with types that can change clothes.";
      description = "Types that are parametric on a functor are like Barbies that have an outfit for each role. This package provides the basic abstractions to work with them comfortably.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "barbies-test" = {
          depends = [
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        "barbies-test-legacy" = {
          depends = [
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/barbies-2.0.3.1.tar.gz";
      sha256 = "a13c1fd05e3e006bd0874d77e1930c5225765e83fa9925b2c5c1df314559df3d";
      });
    }) // {
    package-description-override = "name:           barbies\nversion:        2.0.3.1\nsynopsis:       Classes for working with types that can change clothes.\ndescription:    Types that are parametric on a functor are like Barbies that have an outfit for each role. This package provides the basic abstractions to work with them comfortably.\ncategory:       Data Structures\nhomepage:       https://github.com/jcpetruzza/barbies#readme\nbug-reports:    https://github.com/jcpetruzza/barbies/issues\nauthor:         Daniel Gorin\nmaintainer:     jcpetruzza@gmail.com\ncopyright:      2018 Daniel Gorin\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\nextra-source-files:\n    ChangeLog.md\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/jcpetruzza/barbies\n\nlibrary\n\n  exposed-modules:\n      Barbies\n      Barbies.Bare\n      Barbies.Bi\n      Barbies.Constraints\n      Barbies.Internal\n\n      Data.Functor.Barbie\n      Data.Functor.Transformer\n\n      -- Deprecated modules\n      Data.Barbie\n      Data.Barbie.Bare\n      Data.Barbie.Constraints\n      Data.Functor.Prod\n\n  other-modules:\n      Barbies.Generics.Applicative\n      Barbies.Generics.Bare\n      Barbies.Generics.Constraints\n      Barbies.Generics.Distributive\n      Barbies.Generics.Functor\n      Barbies.Generics.Traversable\n\n      Barbies.Internal.ApplicativeB\n      Barbies.Internal.ApplicativeT\n\n      Barbies.Internal.BareB\n      Barbies.Internal.ConstraintsB\n      Barbies.Internal.ConstraintsT\n      Barbies.Internal.Containers\n      Barbies.Internal.Dicts\n\n      Barbies.Internal.DistributiveB\n      Barbies.Internal.DistributiveT\n\n      Barbies.Internal.FunctorB\n      Barbies.Internal.FunctorT\n\n      Barbies.Internal.MonadT\n\n      Barbies.Internal.TraversableB\n      Barbies.Internal.TraversableT\n\n      Barbies.Internal.Trivial\n      Barbies.Internal.Wear\n      Barbies.Internal.Wrappers\n      Barbies.Internal.Writer\n\n      Data.Generics.GenericN\n\n      -- To be removed\n      Data.Barbie.Internal.Product\n      Data.Barbie.Internal.ProductC\n\n  hs-source-dirs:\n      src\n\n  build-depends:\n      base >=4.11 && <5,\n      distributive,\n      transformers\n\n  ghc-options: -Wall\n\n  default-language: Haskell2010\n  default-extensions:\n      ConstraintKinds\n    , DataKinds\n    , DefaultSignatures\n    , DeriveFunctor\n    , DeriveFoldable\n    , DeriveTraversable\n    , DeriveGeneric\n    , DeriveDataTypeable\n    , EmptyCase\n    , ExplicitForAll\n    , FlexibleContexts\n    , FlexibleInstances\n    , GADTSyntax\n    , KindSignatures\n    , LambdaCase\n    , MultiParamTypeClasses\n    , Rank2Types\n    , ScopedTypeVariables\n    , StandaloneDeriving\n    , TypeApplications\n    , TypeOperators\n\ntest-suite barbies-test\n  type: exitcode-stdio-1.0\n\n  main-is: Spec.hs\n\n  other-modules:\n      TestBarbies\n      TestBarbiesW\n      TestBiBarbies\n      Clothes\n      Spec.Applicative\n      Spec.Bare\n      Spec.Constraints\n      Spec.Distributive\n      Spec.Functor\n      Spec.Traversable\n      Spec.Wrapper\n\n  hs-source-dirs:\n      test\n\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall -O0\n\n  build-depends:\n      barbies\n    , base >=4.7 && <5\n    , distributive\n    , QuickCheck\n    , tasty\n    , tasty-hunit\n    , tasty-quickcheck\n\n  default-language: Haskell2010\n  default-extensions:\n    DeriveDataTypeable\n    DeriveGeneric\n    KindSignatures\n    LambdaCase\n    Rank2Types\n    ScopedTypeVariables\n    StandaloneDeriving\n    TypeApplications\n    TypeOperators\n\n-- This tests that the deprecated Data.Barbie interface\n-- can still be used to build code writen against 1.x,\n-- with deprecation warnings\ntest-suite barbies-test-legacy\n  type: exitcode-stdio-1.0\n\n  main-is: Legacy/Spec.hs\n\n  other-modules:\n      Legacy.TestBarbies\n      Legacy.TestBarbiesW\n      Legacy.Clothes\n      Legacy.Spec.Bare\n      Legacy.Spec.Constraints\n      Legacy.Spec.Functor\n      Legacy.Spec.Traversable\n      Legacy.Spec.Product\n      Legacy.Spec.Wrapper\n\n  hs-source-dirs:\n      test-legacy\n\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall -Wno-deprecations -O0\n\n  build-depends:\n      barbies\n    , base >=4.7 && <5\n    , QuickCheck\n    , tasty\n    , tasty-hunit\n    , tasty-quickcheck\n\n  default-language: Haskell2010\n  default-extensions:\n    DeriveDataTypeable\n    DeriveGeneric\n    KindSignatures\n    LambdaCase\n    Rank2Types\n    ScopedTypeVariables\n    StandaloneDeriving\n    TypeApplications\n    TypeOperators\n";
    }