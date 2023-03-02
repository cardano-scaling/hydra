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
      identifier = { name = "strict-list"; version = "0.1.7"; };
      license = "MIT";
      copyright = "(c) 2019, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/strict-list";
      url = "";
      synopsis = "Strict linked list";
      description = "Implementation of strict linked list with care taken about stack.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."strict-list" or (errorHandler.buildDepError "strict-list"))
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
      url = "http://hackage.haskell.org/package/strict-list-0.1.7.tar.gz";
      sha256 = "70cd8accb5e1f68273b07c6cfe64e27f08815df59e2606bc5ed6f2fdcbb20e36";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: strict-list\nversion: 0.1.7\nsynopsis: Strict linked list\ndescription:\n  Implementation of strict linked list with care taken about stack.\nhomepage: https://github.com/nikita-volkov/strict-list\nbug-reports: https://github.com/nikita-volkov/strict-list/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2019, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/strict-list.git\n\ncommon language-settings\n  default-extensions: BangPatterns, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, NoImplicitPrelude, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies\n  default-language: Haskell2010\n\nlibrary\n  import: language-settings\n  hs-source-dirs: library\n  exposed-modules:\n    StrictList\n  other-modules:\n    StrictList.Prelude\n  build-depends:\n    base >=4.9 && <5,\n    deepseq >=1.4.3 && <1.5,\n    hashable >=1.2 && <2,\n    semigroupoids >=5.3 && <6,\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-extensions: BangPatterns, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, NoImplicitPrelude, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies\n  default-language: Haskell2010\n  main-is: Main.hs\n  build-depends:\n    strict-list,\n    QuickCheck >=2.8.1 && <3,\n    quickcheck-instances >=0.3.11 && <0.4,\n    rerebase <2,\n    tasty >=0.12 && <2,\n    tasty-hunit >=0.9 && <0.11,\n    tasty-quickcheck >=0.9 && <0.11,\n";
    }