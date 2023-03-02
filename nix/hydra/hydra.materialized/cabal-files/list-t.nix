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
      specVersion = "3.0";
      identifier = { name = "list-t"; version = "1.0.5.3"; };
      license = "MIT";
      copyright = "(c) 2014, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/list-t";
      url = "";
      synopsis = "ListT done right";
      description = "A correct implementation of the list monad-transformer.\nUseful for basic streaming.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."HTF" or (errorHandler.buildDepError "HTF"))
            (hsPkgs."mtl-prelude" or (errorHandler.buildDepError "mtl-prelude"))
            (hsPkgs."base-prelude" or (errorHandler.buildDepError "base-prelude"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/list-t-1.0.5.3.tar.gz";
      sha256 = "adf3d36457d4c505bd7c9d5f81760145102d10d66503240fe24c3882947b6e48";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: list-t\nversion: 1.0.5.3\nsynopsis: ListT done right\ndescription:\n  A correct implementation of the list monad-transformer.\n  Useful for basic streaming.\ncategory: Streaming, Data Structures, Control\nhomepage: https://github.com/nikita-volkov/list-t\nbug-reports: https://github.com/nikita-volkov/list-t/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2014, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/list-t.git\n\ncommon language-settings\n  default-extensions: BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, PolyKinds, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples, UndecidableInstances\n  default-language: Haskell2010\n\nlibrary\n  import: language-settings\n  hs-source-dirs: library\n  exposed-modules:\n    ListT\n  other-modules:\n    ListT.Prelude\n  build-depends:\n    base >=4.11 && <5,\n    foldl >=1 && <2,\n    logict >=0.7 && <0.9,\n    mmorph ==1.*,\n    monad-control >=0.3 && <2,\n    mtl ==2.*,\n    semigroups >=0.11 && <0.21,\n    transformers >=0.3 && <0.7,\n    transformers-base ==0.4.*\n\ntest-suite tests\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Main.hs\n  build-depends:\n    list-t,\n    mmorph,\n    HTF >=0.13 && <0.16,\n    mtl-prelude <3,\n    base-prelude\n";
    }