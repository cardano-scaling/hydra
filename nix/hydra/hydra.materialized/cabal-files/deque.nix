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
      identifier = { name = "deque"; version = "0.4.4"; };
      license = "MIT";
      copyright = "(c) 2016, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/deque";
      url = "";
      synopsis = "Double-ended queues";
      description = "Strict and lazy implementations of Double-Ended Queue (aka Dequeue or Deque)\nbased on head-tail linked list.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."strict-list" or (errorHandler.buildDepError "strict-list"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."deque" or (errorHandler.buildDepError "deque"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
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
      url = "http://hackage.haskell.org/package/deque-0.4.4.tar.gz";
      sha256 = "d646d82001cc6b1f17a969ab1d479c2f65f31547c1741dfd7d7c12f7709319f4";
      });
    }) // {
    package-description-override = "name: deque\nversion: 0.4.4\nsynopsis: Double-ended queues\ndescription:\n  Strict and lazy implementations of Double-Ended Queue (aka Dequeue or Deque)\n  based on head-tail linked list.\nhomepage: https://github.com/nikita-volkov/deque\nbug-reports: https://github.com/nikita-volkov/deque/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2016, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/deque.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: BangPatterns, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, NoImplicitPrelude, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies\n  ghc-options: -funbox-strict-fields\n  default-language: Haskell2010\n  exposed-modules:\n    Deque.Lazy\n    Deque.Lazy.Reader\n    Deque.Lazy.State\n    Deque.Strict\n    Deque.Strict.Reader\n    Deque.Strict.State\n  other-modules:\n    Deque.Lazy.Defs\n    Deque.Strict.Defs\n    Deque.Prelude\n  build-depends:\n    base >=4.9 && <5,\n    deepseq >=1.4.3 && <1.5,\n    hashable >=1.2 && <2,\n    mtl >=2.2 && <3,\n    strict-list >=0.1.6 && <0.2\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-extensions: BangPatterns, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, NoImplicitPrelude, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies\n  default-language: Haskell2010\n  main-is: Main.hs\n  build-depends:\n    deque,\n    QuickCheck >=2.8.1 && <3,\n    quickcheck-instances >=0.3.11 && <0.4,\n    rerebase <2,\n    tasty >=0.12 && <2,\n    tasty-hunit >=0.9 && <0.11,\n    tasty-quickcheck >=0.9 && <0.11\n";
    }