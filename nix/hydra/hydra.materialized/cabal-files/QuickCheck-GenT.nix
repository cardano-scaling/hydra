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
      identifier = { name = "QuickCheck-GenT"; version = "0.2.2"; };
      license = "MIT";
      copyright = "(c) 2013, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/QuickCheck-GenT ";
      url = "";
      synopsis = "A GenT monad transformer for QuickCheck library.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/QuickCheck-GenT-0.2.2.tar.gz";
      sha256 = "df177bbb724ba734b86ccc811cc9ec0f1ec624c57390522abc6babfd1649c52e";
      });
    }) // {
    package-description-override = "name:\n  QuickCheck-GenT\nversion:\n  0.2.2\nsynopsis:\n  A GenT monad transformer for QuickCheck library.\ndescription:\nlicense:\n  MIT\nlicense-file:\n  LICENSE\nhomepage:\n  https://github.com/nikita-volkov/QuickCheck-GenT \nbug-reports:\n  https://github.com/nikita-volkov/QuickCheck-GenT/issues \nauthor:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright:\n  (c) 2013, Nikita Volkov\ncategory:\n  Testing\nbuild-type:\n  Simple\ncabal-version:\n  >=1.10\n\n\nsource-repository head\n  type:\n    git\n  location:\n    git://github.com/nikita-volkov/QuickCheck-GenT.git\n\n\nlibrary\n  hs-source-dirs:\n    src\n  exposed-modules:\n    QuickCheck.GenT\n  other-modules:\n    QuickCheck.GenT.Prelude\n  build-depends:\n    QuickCheck >= 2.7,\n    random,\n    mtl,\n    base >= 4.5 && < 5\n  default-extensions:\n    Arrows\n    DeriveGeneric\n    BangPatterns\n    PatternGuards\n    GADTs\n    StandaloneDeriving\n    MultiParamTypeClasses\n    ScopedTypeVariables\n    FlexibleInstances\n    TypeFamilies\n    TypeOperators\n    FlexibleContexts\n    NoImplicitPrelude\n    EmptyDataDecls\n    DataKinds\n    NoMonomorphismRestriction\n    RankNTypes\n    ConstraintKinds\n    DefaultSignatures\n    TupleSections\n    OverloadedStrings\n    TemplateHaskell\n    QuasiQuotes\n    DeriveDataTypeable\n    GeneralizedNewtypeDeriving\n    RecordWildCards\n    MultiWayIf\n    LiberalTypeSynonyms\n    LambdaCase\n  default-language:\n    Haskell2010\n";
    }